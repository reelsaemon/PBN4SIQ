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
  "act_1"="Activity 1",
  "act_2"="Activity 2",
  "act_3"="Activity 3",
  "act_4"="Activity 4",
  "act_5"="Activity 5",
  "act_6"="Activity 6",
  "act_7"="Activity 7",
  "act_8"="Activity 8",
  "act_9"="Activity 9",
  "finished"="Activity end"
)

plot_labeller <- function(variable, value) {
  
  return(plot_facet_labels[value])
}

# violin plot
ggplot(big_plot_frame[!(level %in% finished_levels) & node %in% all_query_nodes[1:8] & !(level %in% c("act_2", "act_3", "act_4", "act_5", "act_7")), ], aes(x=node, y=values, col=bin_order)) +
  geom_violin(scale="width") + # equal width of the violin bodies to make the differences visible
  geom_jitter(width=0.25) +
  scale_colour_gradientn(colours=c("#4c489b", "#fbb40f"), name="Case\nDuration", breaks=c(0,1), labels=c("low", "high")) +
  facet_wrap(vars(level), nrow=1, labeller=plot_labeller) +
  ylim(0,1) +
  scale_x_discrete(labels=c(1:length(all_query_nodes))) +
  theme(strip.text.x = element_text(size=14, face="bold")) +
  labs(x="Place", y="Occurrence Probability")
