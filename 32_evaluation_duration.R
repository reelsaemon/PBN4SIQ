#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

################################################################
# Evaluation Script for Overall Case Duration Class Prediction #
################################################################

# prediction setup --------------------------------------------------------

inference_start_time <- proc.time()

pred_nodes_activity <- nodes(network_structure)[grep("^at_time", nodes(network_structure))]
pred_nodes_activity <- pred_nodes_activity[length(pred_nodes_activity)]

invisible(gc())

test_size <- nrow(test_wide)
test_wide <- test_wide[1:test_size, ]
max_trace_length_overall <- max(test_wide[, max(execution_order)], max_trace_length_train)

trace_lengths_test <- test_set[, .(trace_length=.N), by=get(id_colname)]
colnames(trace_lengths_test) <- c(id_colname, "trace_length")
test_wide <- left_join(test_wide, trace_lengths_test, by=id_colname)
relevant_test_rows <- test_wide[, which(execution_order<=trace_length)]

# parallel predictions ----------------------------------------------------

pb <- txtProgressBar(min=0, max=test_size, width=50, style=3)
usedCores <- detectCores()-1
close(file("time_feature_prediction_log.txt", open="w"))
cluster <- makeCluster(usedCores, outfile="time_feature_prediction_log.txt")
registerDoParallel(cluster)

activity_predictions <- foreach(i = relevant_test_rows, .packages=c("data.table")) %dopar% {
  
  setTxtProgressBar(pb, i)
  index <- test_wide[i, paste0(get(id_colname), "_", stringr::str_pad(execution_order, width=1+floor(log10(max_trace_length_overall)), side="left", pad = "0"))]
  evidence <- as.list(test_wide[i, lapply(.SD, function(x) if(any(is.na(x))){NULL} else {x}), .SDcols=bnlearn::nodes(fitted_network)])
  prediction <- clean_multi_cpdist(fitted_network, setdiff(pred_nodes_activity, names(evidence)), evidence, n_obs=n_samples)
  prediction_tabular <- as.data.table(lapply(prediction, function(x) if(is.null(x)) {NA} else {if(length(which.max(x))==0) {NA} else {names(which.max(x))}}))
  prediction_tabular[, index:=index]
  return(prediction_tabular)
}

close(pb)
stopCluster(cluster)

inference_duration <- as.numeric((proc.time() - inference_start_time)[3])

# evaluation --------------------------------------------------------------

eval_start_time <- proc.time()

barebone <- data.table()
barebone[, (pred_nodes_activity):=character(0)]
extended_list <- list()
extended_list[[1]] <- barebone
extended_list[2:(length(activity_predictions)+1)] <- activity_predictions

activity_prediction_frame <- do.call(rbind, c(extended_list, fill=TRUE))

colnames(activity_prediction_frame) <- paste0(colnames(activity_prediction_frame), "_pred")
activity_prediction_frame[, (id_colname) := test_wide[relevant_test_rows, get(id_colname)]]
activity_prediction_frame[, execution_order := test_wide[relevant_test_rows, execution_order]]
activity_prediction_frame[, trace_length := left_join(activity_prediction_frame, unique(test_set[, c(id_colname, "trace_length"), with=FALSE]), by=id_colname)[, trace_length]]

test_wide_eval <- make_wide_format_new(test_set, activity_colname, id_colname, binary=FALSE, input_max_trace_length=max_trace_length_train, input_activities=activities_train)

test_wide_eval <- test_wide_eval[get(id_colname)%in%activity_prediction_frame[, unique(get(id_colname))], ]

test_wide_eval <- left_join(test_wide_eval, case_durations)
test_wide_eval <- left_join(test_wide_eval, case_durations_binned)

# add outcomes to eval set
test_wide_eval[, (activity_variables):=lapply(.SD, function(x) if_else(x=="finished", 
                                                                   if_else(!is.na(duration_binned), paste0(x, "_class_", duration_binned), NA), 
                                                                   x)), .SDcols=activity_variables, by=get(id_colname)]
test_wide_eval[, (activity_variables):=lapply(.SD, function(x) factor(x, levels=updated_levels_test)), .SDcols=activity_variables]

activity_prediction_frame <- full_join(activity_prediction_frame, test_wide_eval[, c(id_colname, activity_variables), with=FALSE], by=id_colname)

activity_prediction_frame[, pred_outcome:=get(paste0(pred_nodes_activity, "_pred")), by=c(id_colname)]
activity_prediction_frame[, act_outcome:=get(pred_nodes_activity), by=c(id_colname)]

activity_prediction_frame_trace_length_only <- activity_prediction_frame[execution_order <= trace_length, ]

outcome_accuracy_only_trace_length <- round(activity_prediction_frame_trace_length_only[execution_order!=max(execution_order), sum(na.omit(pred_outcome==act_outcome))/.N], digits=5)

n_outcome_info_splits <- 6

activity_prediction_frame_trace_length_only[, percent_available_info:=execution_order/max(execution_order), by=get(id_colname)]
activity_prediction_frame_trace_length_only[, percent_available_info_discrete:=cut(percent_available_info, seq(0,1,1/n_outcome_info_splits))]

info_classes <- activity_prediction_frame_trace_length_only[, unique(percent_available_info_discrete)]

outcome_accuracy_available_info <- lapply(info_classes, function(x) round(activity_prediction_frame_trace_length_only[execution_order!=max(execution_order) & percent_available_info_discrete==x, sum(na.omit(pred_outcome==act_outcome))/.N], digits=5))

eval_duration <- as.numeric((proc.time() - eval_start_time)[3])

dir.create(file.path("export/metrics/"), showWarnings=FALSE, recursive=TRUE)
sink(paste0("export/metrics/metrics_",
            dataset, "_",
            "tr_", round(training_size*100), "_",
            "k_", k, "_",
            "smp_", n_samples, "_",
            "nb_", n_bins, "_",
            "oaa_", other_additional_attributes, "_",
            "bng_", binning_method, "_",
            "sm_", splitting_method, "_",
            "ru_", repl_unident, "_",
            "gbn_", gen_bin_names, "_",
            "seed_", random_seed,
            ".txt"))

print(paste0("Outcome Accuracy (average): ", outcome_accuracy_only_trace_length))
for(i in 1:length(info_classes)) {
  print(paste0(paste0("Outcome Accuracy (Information Set (%): ", info_classes[[i]], "): "), outcome_accuracy_available_info[[i]]))
}
print(paste0("Running time (s): ", round(proc.time()[3] - start_time[3], digits=2)))
sink()


# track metrics
metrics <- list(outcome_accuracy_only_trace_length=outcome_accuracy_only_trace_length,
                outcome_accuracy_available_info=outcome_accuracy_available_info,
                training_duration=training_duration,
                inference_duration=inference_duration,
                eval_duration=eval_duration)