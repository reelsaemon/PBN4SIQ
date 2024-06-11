#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

#############################################################################################
# Evaluation Script for Next Activity Prediction (NAP) and Remaining Trace Prediction (RTP) #
#############################################################################################

# prediction setup --------------------------------------------------------

inference_start_time <- proc.time()

pred_nodes_activity <- nodes(network_structure)[grep("^at_time", nodes(network_structure))]

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

activity_prediction_frame <- full_join(activity_prediction_frame, test_wide_eval[, c(id_colname, activity_variables), with=FALSE], by=id_colname)

# accuracy of next activity prediction (NAP) - complete evaluation frame
activity_prediction_frame[execution_order!=max(execution_order), pred_next_activity := get(paste0(activity_variables, "_pred")[execution_order+1]), by=c(id_colname, "execution_order")]
activity_prediction_frame[execution_order!=max(execution_order), act_next_activity := get(activity_variables[execution_order+1]), by=c(id_colname, "execution_order")]

# accuracy of next activity prediction (NAP) - complete evaluation frame
activity_prediction_frame[, pred_last_activity := get(paste0(activity_variables, "_pred")[max(execution_order)+1]), by=c(id_colname)]
activity_prediction_frame[, act_last_activity := get(activity_variables[max(execution_order)+1]), by=c(id_colname)]

# remaining trace prediction (RTP)
activity_prediction_frame[, trace_extract_helper := 1:.N]

# extracting the remaining trace - complete evaluation frame
activity_prediction_frame[execution_order!=max(execution_order), pred_remaining_trace := paste(unlist(lapply(.SD, as.character))[(execution_order+1):length(activity_variables)], collapse="-"), by=trace_extract_helper, .SDcols=paste0(activity_variables, "_pred")]

activity_prediction_frame[execution_order!=max(execution_order), act_remaining_trace := paste(unlist(lapply(.SD, as.character))[(execution_order+1):length(activity_variables)], collapse="-"), by=trace_extract_helper, .SDcols=activity_variables]

activity_prediction_frame[execution_order!=max(execution_order), act_full_trace := paste(unlist(lapply(.SD, as.character))[1:length(activity_variables)], collapse="-"), by=trace_extract_helper, .SDcols=activity_variables]

# remaining trace without "finished" activity
activity_prediction_frame[execution_order!=max(execution_order), pred_remaining_trace_wo_finished := paste(setdiff(unlist(lapply(.SD, as.character))[(execution_order+1):length(activity_variables)], "finished"), collapse="-"), by=trace_extract_helper, .SDcols=paste0(activity_variables, "_pred")]

activity_prediction_frame[execution_order!=max(execution_order), act_remaining_trace_wo_finished := paste(setdiff(unlist(lapply(.SD, as.character))[(execution_order+1):length(activity_variables)], "finished"), collapse="-"), by=trace_extract_helper, .SDcols=activity_variables]

activity_prediction_frame[execution_order!=max(execution_order), act_full_trace_wo_finished := paste(setdiff(unlist(lapply(.SD, as.character))[1:length(activity_variables)], "finished"), collapse="-"), by=trace_extract_helper, .SDcols=activity_variables]

activity_prediction_frame[, act_remaining_trace_list := str_split(act_remaining_trace, "-")]
activity_prediction_frame[, pred_remaining_trace_list := str_split(pred_remaining_trace, "-")]

activity_prediction_frame[, act_remaining_trace_wo_finished_list := str_split(act_remaining_trace_wo_finished, "-")]
activity_prediction_frame[, pred_remaining_trace_wo_finished_list := str_split(pred_remaining_trace_wo_finished, "-")]

similarities <- c()

for(index in (1:nrow(activity_prediction_frame))[activity_prediction_frame[, execution_order!=max(execution_order)]]) {
  
  similarities <- c(similarities, manual_ndls(activity_prediction_frame[index, act_remaining_trace_wo_finished_list], activity_prediction_frame[index, pred_remaining_trace_wo_finished_list]))
}

# now with trace length only
activity_prediction_frame_trace_length_only <- activity_prediction_frame[execution_order <= trace_length, ]

similarities_only_trace_length <- c()

for(index in (1:nrow(activity_prediction_frame_trace_length_only))[activity_prediction_frame_trace_length_only[, execution_order!=max(execution_order)]]) {
  
  similarities_only_trace_length <- c(similarities_only_trace_length, manual_ndls(activity_prediction_frame_trace_length_only[index, act_remaining_trace_wo_finished_list], activity_prediction_frame_trace_length_only[index, pred_remaining_trace_wo_finished_list]))
}

similarities_only_trace_length_non_NA <- c()

for(index in (1:nrow(activity_prediction_frame_trace_length_only))[activity_prediction_frame_trace_length_only[, execution_order!=max(execution_order)]]) {
  
  if(any(unlist(activity_prediction_frame_trace_length_only[index, pred_remaining_trace_wo_finished_list])=="NA")) {
    
    similarities_only_trace_length_non_NA <- c(similarities_only_trace_length_non_NA, NA)
    
  } else {
    
    similarities_only_trace_length_non_NA <- c(similarities_only_trace_length_non_NA, manual_ndls(activity_prediction_frame_trace_length_only[index, act_remaining_trace_wo_finished_list], activity_prediction_frame_trace_length_only[index, pred_remaining_trace_wo_finished_list]))
  }
}

eval_duration <- as.numeric((proc.time() - eval_start_time)[3])

nap_accuracy <- round(activity_prediction_frame[execution_order!=max(execution_order), sum(na.omit(pred_next_activity==act_next_activity))/.N], digits=5)
rtp_similarity <- round(mean(similarities), digits=5)
nap_accuracy_only_trace_length <- round(activity_prediction_frame_trace_length_only[execution_order!=max(execution_order), sum(na.omit(pred_next_activity==act_next_activity))/.N], digits=5)
rtp_similarity_only_trace_length <- round(mean(similarities_only_trace_length), digits=5)
nap_accuracy_only_trace_length_non_na <- round(activity_prediction_frame_trace_length_only[execution_order!=max(execution_order), sum(na.omit(pred_next_activity==act_next_activity))/sum(!is.na(pred_next_activity))], digits=5)
rtp_similarity_only_trace_length_non_na <- round(mean(na.omit(similarities_only_trace_length_non_NA)), digits=5)
na_share <- round(activity_prediction_frame_trace_length_only[, sum(is.na(pred_next_activity))/.N], digits=5)

dir.create(file.path("export/metrics/"), showWarnings = FALSE)
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

print(paste0("NAP Accuracy: ", nap_accuracy))
print(paste0("RTP Accuracy: ", round(activity_prediction_frame[execution_order!=max(execution_order), sum(na.omit(pred_remaining_trace_wo_finished==act_remaining_trace_wo_finished))/.N], digits=5)))
print(paste0("RTP Similarity: ", rtp_similarity))
print(paste0("NAP Accuracy (only trace length): ", nap_accuracy_only_trace_length))
print(paste0("RTP Similarity (only trace length): ", rtp_similarity_only_trace_length))
print(paste0("NAP Accuracy (only trace length and only non-NA predictions): ", nap_accuracy_only_trace_length_non_na))
print(paste0("RTP Similarity (only trace length and only non-NA predictions): ", rtp_similarity_only_trace_length_non_na))
print(paste0("Share of NA predictions (only trace length): ", na_share))
print(paste0("Running time (s): ", round(proc.time()[3] - start_time[3], digits=2)))
sink()


# track metrics
metrics <- list(nap_accuracy=nap_accuracy,
                rtp_similarity=rtp_similarity,
                nap_accuracy_only_trace_length=nap_accuracy_only_trace_length,
                rtp_similarity_only_trace_length=rtp_similarity_only_trace_length,
                nap_accuracy_only_trace_length_non_na=nap_accuracy_only_trace_length_non_na,
                rtp_similarity_only_trace_length_non_na=rtp_similarity_only_trace_length_non_na,
                na_share=na_share,
                training_duration=training_duration,
                inference_duration=inference_duration,
                eval_duration=eval_duration)