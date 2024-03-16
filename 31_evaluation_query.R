#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

############################################################################
# Script for Process Queries with the Process-aware Bayesian Network (PBN) #
############################################################################

# query setup -------------------------------------------------------------

query_type <- "outcome_prefix"
# query_type <- "intermediate_path"

# outcome query (multiple bins) -------------------------------------------

all_query_nodes <- nodes(fitted_network)[grepl("^at_time_", nodes(fitted_network))]
finished_levels <- train_wide[, levels(get(all_query_nodes[length(all_query_nodes)]))][grepl("^finished_class_", train_wide[, levels(get(all_query_nodes[length(all_query_nodes)]))])]
last_node <- all_query_nodes[length(all_query_nodes)]

if(query_type=="outcome_prefix") {
  
  all_query_dists <- pblapply(finished_levels, 
                              function(x) lapply(all_query_nodes, 
                                                 function(y) get_query_node_prob(fitted_network, y, paste0("((", last_node,"=='", x, "'))"))))
  
} else if(query_type=="intermediate_path") {
  
  all_query_dists <- pblapply(finished_levels, 
                              function(x) lapply(all_query_nodes, 
                                                 function(y) get_query_node_prob(fitted_network, y, paste0("((at_time_01=='act_1' & at_time_02=='act_1')&(", last_node,"=='", x, "'))"))))
}

# scale query probs
all_query_dists <- lapply(all_query_dists, function(x) lapply(x, function(y) y/sum(y)))
all_query_dists <- lapply(all_query_dists, function(x) setNames(x, all_query_nodes))

plot_frame_chunks_list <- lapply(all_query_dists, function(x) lapply(names(x), function(y) data.frame(values=x[[y]], node=y, level=names(x[[y]]))))
plot_frame_list <- lapply(plot_frame_chunks_list, function(x) do.call(rbind, x))

names(plot_frame_list) <- finished_levels
plot_frame_list <- lapply(names(plot_frame_list), function(x) plot_frame_list[[x]] <- as.data.table(plot_frame_list[[x]])[, outcome_level:=x])
names(plot_frame_list) <- finished_levels

big_plot_frame <- as.data.table(do.call(rbind, plot_frame_list))

bin_breaks <- unlist(lapply(str_split(finished_levels, ","), function(x) as.numeric(str_remove(x[1], "^finished_class_\\(|^finished_class_\\["))))
bin_breaks_order <- order(bin_breaks, na.last=TRUE)
bin_order_frame <- data.table(finished_levels=finished_levels[bin_breaks_order], bin_order=1:length(bin_breaks))
big_plot_frame <- left_join(big_plot_frame, bin_order_frame, by=join_by(outcome_level==finished_levels))
big_plot_frame[, bin_order:=(bin_order-min(bin_order))/(max(bin_order)-min(bin_order))]

big_plot_frame <- big_plot_frame[(grepl("^finished_class_", level) & level==outcome_level) | !grepl("^finished_class_", level), ]
big_plot_frame[, level:=if_else(grepl("^finished_class_", level), "finished", level)]

plot_facet_labels <- list(
  "at_time_01"="Place 1",
  "at_time_02"="Place 2",
  "at_time_03"="Place 3",
  "at_time_04"="Place 4",
  "at_time_05"="Place 5",
  "at_time_06"="Place 6",
  "at_time_07"="Place 7",
  "at_time_08"="Place 8",
  "at_time_09"="Place 9",
  "at_time_10"="Place 10",
  "at_time_11"="Place 11",
  "at_time_12"="Place 12",
  "at_time_13"="Place 13",
  "at_time_14"="Place 14",
  "at_time_15"="Place 15"
)

plot_labeller <- function(variable, value) {
  
  return(plot_facet_labels[value])
}

# violin plot
ggplot(big_plot_frame[!(level %in% finished_levels) & node %in% all_query_nodes[1:8], ], aes(x=level, y=values, col=bin_order)) + 
  geom_violin(scale="width") + # equal width of the violin bodies to make the differences visible
  geom_jitter(width=0.25) +
  scale_colour_gradientn(colours=c("#4c489b", "#fbb40f"), name="Case\nDuration", breaks=c(0,1), labels=c("low", "high")) + 
  facet_wrap(vars(node), nrow=2, labeller=plot_labeller) +
  ylim(0,1) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  labs(x="Activity", y="Occurrence Probability")
