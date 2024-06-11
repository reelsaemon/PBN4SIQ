#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

#######################
# Custom BN functions #
#######################

# binning -----------------------------------------------------------------

# quantile-wise binning
quantile_binning <- function(data, nbins=3, generic_bin_names=FALSE, input_breaks=NULL, save_params=FALSE, only_params=FALSE) {
  
  if(save_params==TRUE & only_params==TRUE) {
    
    stop("cannot save_params and only_params at the same time")
  }
  
  if(!is.null(input_breaks)) {
    
    if(class(input_breaks)=="list") {
      
      input_breaks <- unlist(input_breaks)
    }
    
    # for testing data we specify input location parameter values and input sd values
    breaks <- input_breaks
    if(only_params==TRUE) {
      
      return(breaks)
    } else {
      
      binned_data <- cut(data, breaks=breaks)
    }
    
  } else {
    
    std_dev <- sd(na.omit(data))
    
    if(is.na(std_dev)|std_dev==0) {
      
      breaks <- c(-Inf, unique(data), Inf)
      if(only_params==TRUE) {
        
        return(breaks)
      } else {
        
        binned_data <- cut(data, breaks=breaks)
      }
      
    } else {
      
      breaks <- unique(quantile(na.omit(data), probs=seq(0,1,1/nbins)))
      
      if(only_params==TRUE) {
        
        return(breaks)
      } else {
        
        binned_data <- cut(data, breaks=breaks, include.lowest=TRUE)
      }
    }
  }
  
  if(generic_bin_names==TRUE) {
    
    levels(binned_data) <- paste0("q_", 1:length(levels(binned_data)))
  }
  
  if(save_params==TRUE) {
    
    return(list(discretized=binned_data, params=list(breaks=breaks)))
  } else {
    
    return(binned_data)
  }
}

# binning of (preferrably gaussian) data with mean/median as center and bin borders defined with use of standard deviations
bin_loc_sd <- function(data, location=c("mean", "median"), nbins=3, generic_bin_names=FALSE, input_breaks=NULL, save_params=FALSE, only_params=FALSE) {
  
  if(!(location %in% c("mean", "median"))) {
    
    stop("choose either mean or median as location parameter!")
  }
  
  if(nbins<2) {
    stop("choose (uneven) binsize greater than one!")
  }
  
  if(save_params==TRUE & only_params==TRUE) {
    
    stop("cannot save_params and only_params at the same time")
  }
  
  if(nbins%%2==0) {
    warning("you should choose an uneven number of bins! increasing nbins by 1...")
  }
  
  if(!is.null(input_breaks)) {
    
    if(class(input_breaks)=="list") {
      
      input_breaks <- unlist(input_breaks)
    }
    
    # for testing data we specify input location parameter values and input sd values
    breaks <- input_breaks
    if(only_params==TRUE) {
      
      return(breaks)
    } else {
      
      binned_data <- cut(data, breaks=breaks)
    }
    
  } else {
    
    if(location=="median") {
      
      data_loc <- median(na.omit(data))
    } else if (location=="mean") {
      
      data_loc <- mean(na.omit(data))
    }
    
    std_dev <- sd(na.omit(data))
    
    if(is.na(std_dev)|std_dev==0) {
      
      breaks <- c(-Inf, data_loc, Inf)
      if(only_params==TRUE) {
        
        return(breaks)
      } else {
        
        binned_data <- cut(data, breaks=breaks)
      }
      
    } else {
      
      min_break <- data_loc - 2.5*std_dev
      max_break <- data_loc + 2.5*std_dev
      
      nbins_left_right <- ceiling((nbins-1)/2)
      
      inner_breaks_right <- sort(seq(data_loc, max_break, length.out=nbins_left_right+2), decreasing=FALSE)
      inner_breaks_left <- sort(seq(data_loc, min_break, length.out=nbins_left_right+2), decreasing=FALSE)
      
      breaks <- c(-Inf, inner_breaks_left[2:(length(inner_breaks_left)-1)], inner_breaks_right[2:(length(inner_breaks_right)-1)], Inf)
      
      if(only_params==TRUE) {
        
        return(breaks)
      } else {
        
        binned_data <- cut(data, breaks=breaks)
      }
    }
  }
  
  if(generic_bin_names==TRUE) {
    
    levels(binned_data) <- paste0("bin_", 1:length(levels(binned_data)))
  }
  
  if(save_params==TRUE) {
    
    return(list(discretized=binned_data, params=list(loc=data_loc, breaks=breaks)))
  } else {
    
    return(binned_data)
  }
}


# place-annotation --------------------------------------------------------

# data widening function for assigning activities to places
make_wide_format_new <- function(log_data, activity_colname, id_colname, binary=TRUE, preserve_exec=FALSE, input_max_trace_length=NULL, input_activities=NULL) {
  
  require(mltools)
  require(tidyr)
  
  log_data <- copy(log_data[, c(id_colname, activity_colname), with=FALSE])
  original_max_trace_length <- max(log_data[, .N, by=get(id_colname)][, N])
  
  if(!is.null(input_max_trace_length)) {
    
    original_max_trace_length <- input_max_trace_length
  }
  
  log_data <- log_data[, c(get(activity_colname), rep("finished", max(0, original_max_trace_length - .N + 1))), by=get(id_colname)]
  colnames(log_data) <- c(id_colname, activity_colname)
  log_data[, time_slice := paste0("at_time_", str_pad(string=1:.N, width=1+floor(log10(.N)), pad="0", side="left")), by=get(id_colname)]
  
  
  # first widening step
  wide_log_data <- as.data.table(pivot_wider(log_data[, c(id_colname, activity_colname, "time_slice"), with=FALSE], names_from="time_slice", values_from=activity_colname))
  
  if(!is.null(input_activities)) {
    own_activities <- as.character(log_data[get(activity_colname)!="finished", sort(unique(get(activity_colname)))])
    activities <- as.character(union(own_activities, input_activities))
  } else {
    
    activities <- as.character(log_data[get(activity_colname)!="finished", sort(unique(get(activity_colname)))])
  }
  
  time_columns <- sort(paste0("at_time_", str_pad(string=1:(original_max_trace_length + 1), width=1+floor(log10(original_max_trace_length + 1)), pad="0", side="left")))
  
  activity_at_time_columns <- apply(expand.grid(paste0("act_", c(activities, "finished"), "_"), time_columns), 1, paste, collapse="")
  
  # factorize activity columns
  wide_log_data[, (time_columns) := lapply(.SD, function(x) factor(x, levels=c(activities, "finished"))), .SDcols=time_columns]
  
  if(binary) {
    
    # second widening step
    
    # perform one hot encoding and recoding to boolean values
    one_hot_slices <- mltools::one_hot(wide_log_data[, time_columns, with=FALSE])
    one_hot_slices[, (colnames(one_hot_slices)) := lapply(.SD, function(x) factor(as.logical(x), levels=c(TRUE, FALSE))), .SDcols=colnames(one_hot_slices)]
    
    # add one hot encoded boolean values to original data table
    wide_log_data[, (activity_at_time_columns) := one_hot_slices]
    
    # remove time columns
    wide_log_data[, (time_columns) := NULL]
  }
  
  if(preserve_exec==TRUE) {
    
    variable_columns <- colnames(wide_log_data[, -c(id_colname), with=FALSE])
    
    wide_log_data <- wide_log_data[rep(1:.N, each=original_max_trace_length), ]
    wide_log_data[, execution_order:=1:.N, by=get(id_colname)]
    
    col_tiers <- as.numeric(str_remove(unlist(lapply(str_split(variable_columns, "at_time_"), function(x) x[[2]])), "^0+"))
    names(col_tiers) <- variable_columns
    
    lower_staircase_values <- wide_log_data[, lapply(variable_columns, function(x) lower_staircase(wide_log_data[, get(x)], wide_log_data[, execution_order], col_tiers[x]))]
    wide_log_data[, (variable_columns):=lower_staircase_values]
  }
  
  return(wide_log_data)
}

# # data widening function for assigning activities and attribute variables to places
make_wide_format_new_additional_attributes <- function(log_data, activity_colname, id_colname, additional_attributes_cols, case_attribute_cols, binary=TRUE, preserve_exec=FALSE, input_max_trace_length=NULL, input_activities=NULL) {
  
  require(mltools)
  require(tidyr)
  
  log_data <- copy(log_data[, c(id_colname, activity_colname, additional_attributes_cols), with=FALSE])
  original_max_trace_length <- max(log_data[, .N, by=get(id_colname)][, N])
  
  if(!is.null(input_max_trace_length)) {
    
    original_max_trace_length <- input_max_trace_length
  }
  
  # attribute_types <- as.list(log_data[, lapply(.SD, class), .SDcols=additional_attributes_cols])
  attribute_types <- setNames(unlist(log_data[, lapply(.SD, class), .SDcols=additional_attributes_cols]), additional_attributes_cols)
  
  attribute_levels <- as.list(log_data[, lapply(.SD, function(x) if(!is.null(levels(x))) {list(levels(x))} else {NA}), .SDcols=additional_attributes_cols])
  
  if(length(case_attribute_cols)==0) {
    
    padded_attributes_numeric <- log_data[, lapply(.SD, function(x) attribute_padder(x, rep(NA, original_max_trace_length - .N + 1))), .SDcols=additional_attributes_cols[attribute_types=="numeric"], by=get(id_colname)]
    padded_attributes_non_numeric <- log_data[, lapply(.SD, function(x) attribute_padder(x, rep("NO", original_max_trace_length - .N + 1))), .SDcols=additional_attributes_cols[attribute_types!="numeric"], by=get(id_colname)]
    
    padded_attributes <- do.call(full_join, c(list(padded_attributes_numeric, padded_attributes_non_numeric), by="get"))
    
  } else {
    
    padded_attributes_numeric <- log_data[, lapply(.SD, function(x) attribute_padder(x, rep(NA, original_max_trace_length - .N + 1))), .SDcols=setdiff(additional_attributes_cols[attribute_types=="numeric"], case_attribute_cols), by=get(id_colname)]
    padded_attributes_numeric_case_attributes <- log_data[, lapply(.SD, function(x) carry_helper_update(attribute_padder(x, rep(NA, original_max_trace_length - .N + 1)))), .SDcols=intersect(additional_attributes_cols[attribute_types=="numeric"], case_attribute_cols), by=get(id_colname)]
    padded_attributes_non_numeric <- log_data[, lapply(.SD, function(x) attribute_padder(x, rep("NO", original_max_trace_length - .N + 1))), .SDcols=setdiff(additional_attributes_cols[attribute_types!="numeric"], case_attribute_cols), by=get(id_colname)]
    padded_attributes_non_numeric_case_attributes <- log_data[, lapply(.SD, function(x) carry_helper_update(attribute_padder(x, rep(NA, original_max_trace_length - .N + 1)))), .SDcols=intersect(additional_attributes_cols[attribute_types!="numeric"], case_attribute_cols), by=get(id_colname)]
    
    padded_attributes_numeric <- do.call(full_join, c(list(padded_attributes_numeric, padded_attributes_numeric_case_attributes), by="get"))
    padded_attributes_non_numeric <- do.call(full_join, c(list(padded_attributes_non_numeric, padded_attributes_non_numeric_case_attributes), by="get"))
    padded_attributes <- do.call(full_join, c(list(padded_attributes_numeric, padded_attributes_non_numeric), by="get"))
    
  }
  
  log_data <- log_data[, c(get(activity_colname), rep("finished", max(0, original_max_trace_length - .N + 1))), by=get(id_colname)]
  log_data <- cbind(log_data, padded_attributes[, additional_attributes_cols, with=FALSE])
  colnames(log_data) <- c(id_colname, activity_colname, additional_attributes_cols)
  log_data[, time_slice := paste0("at_time_", str_pad(string=1:.N, width=1+floor(log10(.N)), pad="0", side="left")), by=get(id_colname)]
  
  
  # first widening step
  wide_log_data <- as.data.table(pivot_wider(log_data[, c(id_colname, activity_colname, "time_slice"), with=FALSE], names_from="time_slice", values_from=activity_colname))
  wide_log_data_attributes <- as.data.table(pivot_wider(log_data[, c(id_colname, additional_attributes_cols, "time_slice"), with=FALSE], names_from="time_slice", values_from=additional_attributes_cols))
  
  if(!is.null(input_activities)) {
    
    activities <- as.character(union(own_activities, input_activities))
  } else {
    
    activities <- as.character(log_data[get(activity_colname)!="finished", sort(unique(get(activity_colname)))])
  }
  
  time_columns <- sort(paste0("at_time_", str_pad(string=1:(original_max_trace_length + 1), width=1+floor(log10(original_max_trace_length + 1)), pad="0", side="left")))
  
  activity_at_time_columns <- apply(expand.grid(paste0("act_", c(activities, "finished"), "_"), time_columns), 1, paste, collapse="")
  
  # factorize activity columns
  wide_log_data[, (time_columns) := lapply(.SD, function(x) factor(x, levels=c(activities, "finished"))), .SDcols=time_columns]
  
  wide_log_data <- left_join(wide_log_data, wide_log_data_attributes, by=id_colname)
  
  if(binary) {
    
    # second widening step
    
    # perform one hot encoding and recoding to boolean values
    one_hot_slices <- mltools::one_hot(wide_log_data[, time_columns, with=FALSE])
    one_hot_slices[, (colnames(one_hot_slices)) := lapply(.SD, function(x) factor(as.logical(x), levels=c(TRUE, FALSE))), .SDcols=colnames(one_hot_slices)]
    
    # add one hot encoded boolean values to original data table
    wide_log_data[, (activity_at_time_columns) := one_hot_slices]
    
    # remove time columns
    wide_log_data[, (time_columns) := NULL]
  }
  
  if(preserve_exec==TRUE) {
    
    variable_columns <- colnames(wide_log_data[, -c(id_colname), with=FALSE])
    
    wide_log_data <- wide_log_data[rep(1:.N, each=original_max_trace_length), ]
    wide_log_data[, execution_order:=1:.N, by=get(id_colname)]
    
    col_tiers <- as.numeric(str_remove(unlist(lapply(str_split(variable_columns, "at_time_"), function(x) x[[2]])), "^0+"))
    names(col_tiers) <- variable_columns
    
    lower_staircase_values <- wide_log_data[, lapply(variable_columns, function(x) lower_staircase(wide_log_data[, get(x)], wide_log_data[, execution_order], col_tiers[x]))]
    wide_log_data[, (variable_columns):=lower_staircase_values]
  }
  
  return(wide_log_data)
}
# network generation ------------------------------------------------------

# automatic network structure generation for the multicategorical Bayesian Network approach (control-flow nodes)
make_multicategorical_structure <- function(max_trace_length, memory) {
  
  times <- paste0("at_time_", str_pad(string=1:(max_trace_length), width=1+floor(log10(max_trace_length)), pad="0", side="left"))
  
  parents_list <- list()
  
  for(i in 1:length(times)) {
    
    parents_list[[times[i]]] <- times[max(0, (i-memory)):(i-1)]
  }
  
  network_string_parts <- lapply(names(parents_list), function(x) paste0("[", x, ifelse(length(parents_list[[x]]) > 0, "|", ""), paste(parents_list[[x]], collapse=":"), "]"))
  
  network_string <- paste(unlist(network_string_parts), collapse="")
  
  multicat_network <- model2network(network_string)
  
  return(multicat_network)
}

# prediction and query functions ------------------------------------------

# clean output of multi-node conditional distributions
clean_multi_cpdist <- function(fit, nodes, evidence, method='lw', n_obs=NULL, consider_NA_samples=FALSE) {
  
  if(is.null(n_obs)) {
    
    dist <- tryCatch(bnlearn::cpdist(fit=fit, nodes=nodes, evidence=evidence, method=method), error=function(cond) {return(NULL)})
  } else {
    
    dist <- tryCatch(bnlearn::cpdist(fit=fit, nodes=nodes, evidence=evidence, method=method, n=n_obs), error=function(cond) {return(NULL)})
  }
  
  
  clean_output <- as.list(rep(NA, length(nodes)))
  names(clean_output) <- nodes
  
  if(is.null(dist)) {
    
    return(NULL)
  }
  
  if(consider_NA_samples) {
    for(node in nodes) {
      clean_output[[node]] <- round(table(dist[, node], useNA='always')/n_obs, digits=4)
    }
    return(clean_output)
  } else {
    for(node in nodes) {
      clean_output[[node]] <- round(table(dist[, node])/length(na.omit(dist[, node])), digits=4)
    }
    return(clean_output)
  }
}

# query conditional probabilities with a query string
get_query_node_prob <- function(fitted_network, event_node, evidence, n_samples=1e5) {
  
  if(length(fitted_network[[event_node]]$parents)==0) {
    
    node_levels <- names(fitted_network[[event_node]]$prob)
  } else {
    
    node_levels <- dimnames(fitted_network[[event_node]]$prob)[[1]]
  }
  
  event_strings <- lapply(node_levels, function(x) paste("(", event_node, " == '", x, "')", sep = ""))
  
  commands <- paste("cpquery(fitted_network, ", event_strings, ", ", evidence, ", n=", n_samples, ")", sep = "")
  probs <- lapply(commands, function(x) eval(parse(text=x)))
  dist <- unlist(probs)
  names(dist) <- node_levels
  
  return(dist)
}

# helper functions --------------------------------------------------------

# pad additional attributes to max trace length in widening function
attribute_padder <- function(vec, pad) {
  
  if(is.factor(vec)) {
    
    vec_levels <- levels(vec)
    padded_vector <- factor(c(vec, pad), levels=c(1:length(vec_levels), unique(pad)), labels=na.omit(c(vec_levels, unique(pad))))
  } else {
    
    padded_vector <- c(vec, pad)
  }
  
  return(padded_vector)
}

# introducing NA values in a lower-staircase pattern with respect to place information
lower_staircase <- function(val_col, order_col, tier_info) {
  
  staircase_values <- if_else(order_col < tier_info, NA, val_col)
  return(staircase_values)
}

# evaluation --------------------------------------------------------------

# wrapper for DamerauLevenshtein()-function from comparator-package
manual_ndls <- function(actual_trace, predicted_trace) {
  
  # actual_trace and predicted_trace are the sequences as lists of character vectors
  
  if(is.null(predicted_trace[[1]])) {
    
    return(NA)
  } else {
    
    if(length(actual_trace[[1]])==0 & length(predicted_trace[[1]])==0) {
      
      ndls <- 1
      return(ndls)
    }
    
    distance <- DamerauLevenshtein()(actual_trace, predicted_trace)
    normalized_distance <- distance/max(length(actual_trace[[1]]), length(predicted_trace[[1]]))
    ndls <- 1-normalized_distance
    return(ndls)
  }
}

