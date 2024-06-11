#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

###########################################################
# Model Training (Overall Case Duration Class Prediction) #
###########################################################

# Model parameters --------------------------------------------------------

k_values <- c(1)
repl_unident_values <- c(FALSE)
training_size_values <- c(0.80)
splitting_method_values <- c("random", "chronological")
n_samples_values <- c(10000)
n_bins_values <- c(3, 5, 10)
gen_bin_names_values <- c(TRUE)
other_additional_attributes_values <- c(TRUE, FALSE)
time_features_values <- c(TRUE, FALSE)
binning_method_values <- c("activity_only", "stage_only", "both", "plain")
use_quantile_binning_values <- c(FALSE, TRUE)
dataset_values <- c(
  "helpdesk"
  ,
  "BPI2012"
  ,
  "BPI2012W"
  ,
  "BPI2012Sub"
  ,
  "Permit"
  ,
  "RoadFines"
)
random_seed_values <- c(27626)

parameter_combinations <- as.data.table(expand.grid(list(k_values, 
                                                         repl_unident_values,
                                                         training_size_values,
                                                         splitting_method_values,
                                                         n_samples_values,
                                                         n_bins_values,
                                                         gen_bin_names_values,
                                                         other_additional_attributes_values,
                                                         time_features_values,
                                                         binning_method_values,
                                                         use_quantile_binning_values,
                                                         dataset_values,
                                                         random_seed_values)))

colnames(parameter_combinations) <- c("k", "repl_unident", "training_size", "splitting_method", 
                                      "n_samples", "n_bins", "gen_bin_names", "other_additional_attributes", 
                                      "time_features", "binning_method", "use_quantile_binning", 
                                      "dataset", "random_seed")

# pick parameter combination from parameter_combinations rows
comb_idx <- 1

start_time <- proc.time()

invisible(list2env(parameter_combinations[comb_idx, ], .GlobalEnv))
set.seed(random_seed)


# data import and preprocessing -------------------------------------------

if(dataset=="helpdesk") {
  
  event_log <- as.data.table(read.csv("data/helpdesk.csv", sep=","))
  id_colname <- "CaseID"
  activity_colname <- "ActivityID"
  timestamp_colname <- "CompleteTimestamp"
  
  duration_split <- 96
  
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c()
    dataset_specific_additional_attributes_discrete <- c()
  }
  
} else if(dataset=="BPI2012") {
  
  event_log <- as.data.table(read.csv("data/BPI_2012.csv", sep=","))
  id_colname <- "CASE_concept_name"
  activity_colname <- "activity_id"
  timestamp_colname <- "timestamp"
  
  duration_split <- 120
  
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c("CASE_AMOUNT_REQ")
    # dataset_specific_additional_attributes_discrete <- c("resource_id")
    dataset_specific_additional_attributes_discrete <- c()
    
    # event_log[, resource_id:=as.factor(resource_id)]
  }
  
} else if(dataset=="BPI2012W") {
  
  event_log <- as.data.table(read.csv("data/BPI_2012_W_complete.csv", sep=","))
  id_colname <- "CASE_concept_name"
  activity_colname <- "activity_id"
  timestamp_colname <- "timestamp"
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c("CASE_AMOUNT_REQ")
    # dataset_specific_additional_attributes_discrete <- c("resource_id")
    dataset_specific_additional_attributes_discrete <- c()
    
    # event_log[, resource_id:=as.factor(resource_id)]
  }
  
} else if(dataset=="BPI2012Sub") {
  
  event_log <- as.data.table(read.csv("data/BPI_2012_sub.csv", sep=","))
  id_colname <- "CASE_concept_name"
  activity_colname <- "activity_id"
  timestamp_colname <- "timestamp"
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c("CASE_AMOUNT_REQ")
    # dataset_specific_additional_attributes_discrete <- c("resource_id")
    dataset_specific_additional_attributes_discrete <- c()
    
    # event_log[, resource_id:=as.factor(resource_id)]
  }
  
}  else if(dataset=="Permit") {
  
  event_log <- as.data.table(read.csv("data/BPI_2020_permit_log.csv", sep=","))
  id_colname <- "CASE_concept_name"
  activity_colname <- "activity_id"
  timestamp_colname <- "timestamp"
  
  duration_split <- 1200
  
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c()
    dataset_specific_additional_attributes_discrete <- c()
  }
  
} else if(dataset=="RoadFines") {
  
  event_log <- as.data.table(read.csv("data/road_fines.csv", sep=" "))
  id_colname <- "CASE_concept_name"
  activity_colname <- "activity_id"
  timestamp_colname <- "timestamp"
  
  duration_split <- 8400
  
  if(other_additional_attributes==TRUE) {
    
    dataset_specific_additional_attributes <- c()
    dataset_specific_additional_attributes_discrete <- c()
  }
  
}

event_log[, execution_order := 1:.N, by=get(id_colname)]
event_log[, (activity_colname) := paste0("act_", get(activity_colname))]
event_log[, NextTimestamp := lead(get(timestamp_colname)), by=get(id_colname)]
event_log[, (activity_colname) := as.character(get(activity_colname))]
event_log[, LastTimestamp := lag(get(timestamp_colname)), by=get(id_colname)]
event_log[, FirstTimestampOverall := first(get(timestamp_colname)), by=get(id_colname)]
event_log[, TSLE := as.numeric(difftime(get(timestamp_colname), LastTimestamp, units="secs"))] # time since last event
event_log[, TSCS := as.numeric(difftime(get(timestamp_colname), FirstTimestampOverall, units="secs"))] # time since case start
event_log[, TSMN := as.numeric(as.ITime(get(timestamp_colname)))/60/60] # time since midnight (in hours)
event_log[is.na(TSLE), TSLE := 0]
event_log[, log_TSLE := log(TSLE+1)]
event_log[, log_TSCS := log(TSCS+1)]
event_log[, (activity_colname) := as.character(get(activity_colname))]
event_log[, FirstTimestamp := min(get(timestamp_colname)), by=get(id_colname)]
event_log <- event_log[order(FirstTimestamp), ]
event_log[, case_duration:=as.numeric(difftime(max(get(timestamp_colname)), min(get(timestamp_colname)), units="hours")), by=get(id_colname)]
event_log[, case_duration_binned:=cut(case_duration, c(-Inf, duration_split, Inf), include.lowest=TRUE)]

case_durations <- event_log[, .(duration=max(case_duration)), by=get(id_colname)]
colnames(case_durations) <- c(id_colname, "duration")
event_log[, case_duration_binned:=cut(case_duration, c(-Inf, duration_split, Inf), include.lowest=TRUE)]
case_durations_binned <- event_log[, .(duration_binned=unique(case_duration_binned)), by=get(id_colname)]
colnames(case_durations_binned) <- c(id_colname, "duration_binned")

if(other_additional_attributes==TRUE) {
  additional_attributes <- c("log_TSLE", "log_TSCS", "TSMN", dataset_specific_additional_attributes)
  
  additional_attributes_binned <- paste0(additional_attributes, "_binned")
} else {
  
  additional_attributes <- c("log_TSLE", "log_TSCS", "TSMN")
  
  additional_attributes_binned <- paste0(additional_attributes, "_binned")
}

# train-test-split --------------------------------------------------------

test_size <- 1-training_size

if(splitting_method=="chronological") {
  
  # chronological train test split
  trace_ids <- event_log[, unique(get(id_colname))]
  train_ids <- trace_ids[1:floor((training_size)*length(trace_ids))]
  test_ids <- trace_ids[(floor((training_size)*length(trace_ids))+1):length(trace_ids)]
  
} else if(splitting_method=="random") {
  
  # random train test split
  train_ids <- sample(event_log[, unique(get(id_colname))], floor(training_size*event_log[, length(unique(get(id_colname)))]), replace=FALSE)
  test_ids <- setdiff(event_log[, unique(get(id_colname))], train_ids)
}

training_set <- event_log[get(id_colname) %in% train_ids, ]
test_set <- event_log[get(id_colname) %in% test_ids, ]

training_set[, trace_length := .N, by=get(id_colname)]
test_set[, trace_length := .N, by=get(id_colname)]

max_trace_length_train <- training_set[, max(trace_length)]
activities_train <- training_set[, unique(get(activity_colname))]

# variable discretization -------------------------------------------------

if(binning_method=="activity_only") {
  
  if(use_quantile_binning==TRUE) {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(get(activity_colname))]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(get(activity_colname))]
    
    training_set_binning_params <- training_set[, c(activity_colname, paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(get(activity_colname)), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(activity_colname, paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by=activity_colname)
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) quantile_binning(get(x), nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  } else {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(get(activity_colname))]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(get(activity_colname))]
    
    training_set_binning_params <- training_set[, c(activity_colname, paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(get(activity_colname)), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(activity_colname, paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by=activity_colname)
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) bin_loc_sd(get(x), "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  }
  
  
} else if(binning_method=="stage_only") {
  
  if(use_quantile_binning==TRUE) {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(execution_order)]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(execution_order)]
    
    training_set_binning_params <- training_set[, c("execution_order", paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(execution_order), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c("execution_order", paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by="execution_order")
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) quantile_binning(get(x), nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  } else {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(execution_order)]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(execution_order)]
    
    training_set_binning_params <- training_set[, c("execution_order", paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(execution_order), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c("execution_order", paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by="execution_order")
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) bin_loc_sd(get(x), "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  }
  
} else if(binning_method=="both") {
  
  if(use_quantile_binning==TRUE) {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(get(activity_colname), execution_order)]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(get(activity_colname), execution_order)]
    
    training_set_binning_params <- training_set[, c(activity_colname, "execution_order", paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(get(activity_colname), execution_order), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(activity_colname, "execution_order", paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by=c(activity_colname, "execution_order"))
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) quantile_binning(get(x), nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
    
  } else {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes,  by=.(get(activity_colname), execution_order)]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes,  by=.(get(activity_colname), execution_order)]
    
    training_set_binning_params <- training_set[, c(activity_colname, "execution_order", paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), by=.(get(activity_colname), execution_order), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(activity_colname, "execution_order", paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- left_join(test_set, training_set_binning_params, by=c(activity_colname, "execution_order"))
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) bin_loc_sd(get(x), "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  }
  
} else if(binning_method=="plain") {
  
  if(use_quantile_binning==TRUE) {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(quantile_binning(x, nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes]
    
    training_set_binning_params <- training_set[, c(paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- cbind(test_set, training_set_binning_params)
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) quantile_binning(get(x), nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]
  } else {
    
    training_set_discretized <- training_set[, (paste0(additional_attributes, "_binned")):=lapply(.SD, function(x) bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE)), .SDcols=additional_attributes]
    
    training_set[, ((paste0(additional_attributes, "_breaks"))):=lapply(.SD, function(x) list(bin_loc_sd(x, "median", nbins=n_bins, generic_bin_names=gen_bin_names, only_params=TRUE))), .SDcols=additional_attributes]
    
    training_set_binning_params <- training_set[, c(paste0(additional_attributes, "_breaks")), with=FALSE]
    training_set_binning_params <- training_set_binning_params[, lapply(.SD, function(x) x[1]), .SDcols=paste0(additional_attributes, "_breaks")]
    colnames(training_set_binning_params) <- c(paste0(additional_attributes, "_breaks"))
    
    test_set_discretized <- cbind(test_set, training_set_binning_params)
    test_set_discretized[, idx:=1:.N]
    test_set_discretized[, (paste0(additional_attributes, "_binned")):=lapply(additional_attributes, function(x) bin_loc_sd(get(x), "median", nbins=n_bins, generic_bin_names=gen_bin_names, save_params=FALSE, input_breaks=get(paste0(x, "_breaks")))), by=idx]    
  }
}

if(other_additional_attributes==TRUE) {
  
  additional_attributes_binned <- c(additional_attributes_binned, dataset_specific_additional_attributes_discrete)
}

training_set <- copy(training_set_discretized)
test_set <- copy(test_set_discretized)


# place-annotation of all columns -----------------------------------------

train_wide <- make_wide_format_new_additional_attributes(training_set, activity_colname, id_colname, additional_attributes_binned, case_attribute_cols=c(), binary=FALSE, input_max_trace_length=max_trace_length_train)
test_wide <- make_wide_format_new_additional_attributes(test_set, activity_colname, id_colname, additional_attributes_binned, case_attribute_cols=c(), binary=FALSE, preserve_exec=TRUE, input_max_trace_length=max_trace_length_train)
train_wide <- left_join(train_wide, case_durations)
train_wide <- left_join(train_wide, case_durations_binned)
test_wide <- left_join(test_wide, case_durations)
test_wide <- left_join(test_wide, case_durations_binned)


# annotation of end activities with case duration classes -----------------

activity_variables <- colnames(train_wide)[grepl("^at_time_", colnames(train_wide))]

activity_variables_pred <- activity_variables[2:(length(activity_variables))]

train_wide[, (activity_variables):=lapply(.SD, function(x) if_else(x=="finished", 
                                                               if_else(!is.na(duration_binned), paste0(x, "_class_", duration_binned), NA), 
                                                               x)), .SDcols=activity_variables, by=get(id_colname)]

test_wide[, (activity_variables):=lapply(.SD, function(x) if_else(x=="finished", 
                                                              if_else(!is.na(duration_binned), paste0(x, "_class_", duration_binned), NA), 
                                                              x)), .SDcols=activity_variables, by=get(id_colname)]

# update factor levels
updated_levels_train <- sort(setdiff(train_wide[, levels(get(activity_variables[length(activity_variables)]))], "finished"))
updated_levels_test <- sort(setdiff(test_wide[, levels(get(activity_variables[length(activity_variables)-1]))], "finished"))

train_wide[, (activity_variables):=lapply(.SD, function(x) factor(x, levels=updated_levels_train)), .SDcols=activity_variables]
test_wide[, (activity_variables):=lapply(.SD, function(x) factor(x, levels=updated_levels_test)), .SDcols=activity_variables]

# generating the network structure ----------------------------------------

generic_structure <- make_multicategorical_structure(max_trace_length=max_trace_length_train+1, memory=k)

TSLE_nodes <- colnames(train_wide)[grepl("^log_TSLE_binned_", colnames(train_wide))]
TSCS_nodes <- colnames(train_wide)[grepl("^log_TSCS_binned_", colnames(train_wide))]
TSMN_nodes <- colnames(train_wide)[grepl("^TSMN_binned_", colnames(train_wide))]

if(other_additional_attributes==TRUE) {
  
  other_additional_attributes_nodes <- unlist(lapply(additional_attributes_binned, function(x) colnames(train_wide)[grepl(paste0("^", x, "_"), colnames(train_wide))]))
  other_additional_attributes_nodes <- setdiff(other_additional_attributes_nodes, c(TSLE_nodes, TSCS_nodes, TSMN_nodes))
} else {
  
  other_additional_attributes_nodes <- c()
}

if(length(other_additional_attributes_nodes)!=0) {
  
  if(time_features==TRUE) {
    
    for(node in c(TSLE_nodes, TSCS_nodes, TSMN_nodes, other_additional_attributes_nodes)) {
      
      generic_structure <- add.node(generic_structure, node)
    }
  } else {
    for(node in c(other_additional_attributes_nodes)) {
      
      generic_structure <- add.node(generic_structure, node)
    }
  }
  
} else {
  
  if(time_features==TRUE) {
    
    for(node in c(TSLE_nodes, TSCS_nodes, TSMN_nodes)) {
      
      generic_structure <- add.node(generic_structure, node)
    }
  } else {
    
  }  
}

new_arcs_TSLE <- rbind(data.frame(from=TSLE_nodes[1:(length(TSLE_nodes)-1)], to=lead(str_remove(TSLE_nodes, "^log_TSLE_binned_"))[1:(length(TSLE_nodes)-1)]),
                       data.frame(from=str_remove(TSLE_nodes, "^log_TSLE_binned_")[1:(length(TSLE_nodes)-1)], to=TSLE_nodes[2:length(TSLE_nodes)]))
new_arcs_TSCS <- rbind(data.frame(from=TSCS_nodes[1:(length(TSCS_nodes)-1)], to=lead(str_remove(TSCS_nodes, "^log_TSCS_binned_"))[1:(length(TSCS_nodes)-1)]),
                       data.frame(from=str_remove(TSCS_nodes, "^log_TSCS_binned_")[1:(length(TSCS_nodes)-1)], to=TSCS_nodes[2:length(TSCS_nodes)]))
new_arcs_TSMN <- rbind(data.frame(from=TSMN_nodes[1:(length(TSMN_nodes)-1)], to=lead(str_remove(TSMN_nodes, "^TSMN_binned_"))[1:(length(TSMN_nodes)-1)]),
                       data.frame(from=str_remove(TSMN_nodes, "^TSMN_binned_")[1:(length(TSMN_nodes)-1)], to=TSMN_nodes[2:length(TSMN_nodes)]))

if(time_features==TRUE) {
  
  new_arcs <- rbind(
    new_arcs_TSLE
    ,
    new_arcs_TSCS
    ,
    new_arcs_TSMN
  )
} else {
  
  new_arcs <- data.frame()
}

if(length(other_additional_attributes_nodes)!=0) {
  
  new_arcs_other_additional_attributes <- data.frame(from=c(other_additional_attributes_nodes, other_additional_attributes_nodes[1:(length(other_additional_attributes_nodes)-1)]), to=c(str_remove(other_additional_attributes_nodes,  paste0("^", paste(paste0(additional_attributes_binned, "_"), collapse="|^"))), other_additional_attributes_nodes[2:length(other_additional_attributes_nodes)]))
  
  attribute_groups <- lapply(setdiff(additional_attributes_binned, c("log_TSLE_binned", "log_TSCS_binned", "TSMN_binned")), function(x) other_additional_attributes_nodes[grepl(x, other_additional_attributes_nodes)])
  
  attribute_group_frame <- do.call(rbind, lapply(attribute_groups, function(x) na.omit(data.frame(from=x, to=lead(x)))))
  
  new_arcs_other_additional_attributes <- rbind(data.frame(from=other_additional_attributes_nodes, to=str_remove(other_additional_attributes_nodes,  paste0("^", paste(paste0(additional_attributes_binned, "_"), collapse="|^")))), attribute_group_frame)
  
  if(time_features==TRUE) {
    
    new_arcs <- rbind(
      new_arcs_TSLE
      ,
      new_arcs_TSCS
      ,
      new_arcs_TSMN
      ,
      new_arcs_other_additional_attributes
    )
  } else {
    
    new_arcs <- rbind(
      new_arcs_other_additional_attributes
    )
  }
}

arcs(generic_structure) <- rbind(arcs(generic_structure), new_arcs)

network_structure <- copy(generic_structure)

# fitting the network parameters ------------------------------------------

fitted_network <- bn.fit(network_structure, train_wide[, nodes(network_structure), with=FALSE], replace.unidentifiable=repl_unident)

training_duration <- as.numeric((proc.time() - start_time)[3])