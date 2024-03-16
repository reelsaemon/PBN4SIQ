#######################################################################################
# Process-Aware Bayesian Networks for Sequential Inference and Querying on Event Logs #
#######################################################################################

#################
# Preliminaries #
#################

# extended functionality
library(data.table)
library(dplyr)
library(stringr)
library(pbapply)

# model packages
library(bnlearn)

# evaluation metrics
library(comparator)

# parallelization
library(doParallel)
library(parallel)

# plotting
library(ggplot2)

# custom functions
source("custom_BN_functions.R")