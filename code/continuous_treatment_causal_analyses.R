##### Note: GEE model (line 65) may take up to 96 GB for trim 5/95, or up to 250 GB for trim 1/99, to run #####

## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)

# Define parser arguments ----
parser <- ArgumentParser()
parser$add_argument("-e", "--exposure", default="mean_distance_all_persistent_dealers",
                    help="Exposure variable 'mean_distance_all_persistent_dealers' or 'mean_dist_commercial_dealers'", type="character")
parser$add_argument("-s", "--seed", default=100,
                    help="seed value", type="integer")
parser$add_argument("-sa", "--sensitivity_analysis", default="state",
                    help="Sensitivity analysis 'state' or 'state.urbanity'", type="character")
parser$add_argument("-p", "--percentiles", default="5.95",
                    help="Percentiles of exposure '5.95' or '1.99'", type="character") 
args = parser$parse_args()


## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))

## Get datasets for main analysis ----
data <- vector("list", 2)
names(data) <- c("mean_distance_all_persistent_dealers", "mean_dist_commercial_dealers")
data[["mean_distance_all_persistent_dealers"]] <- vector("list", 2)
data[["mean_dist_commercial_dealers"]] <- vector("list", 2)
names(data[["mean_distance_all_persistent_dealers"]]) <- c("state", "state.urbanity")
names(data[["mean_dist_commercial_dealers"]]) <- c("state", "state.urbanity")

data[["mean_distance_all_persistent_dealers"]][["state"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("State_Name", quantitative_covariates))
data[["mean_distance_all_persistent_dealers"]][["state.urbanity"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("State_Name", "urbanity", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state.urbanity"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", "urbanity", quantitative_covariates))


## Perform causal analysis

## Matching CausalGPS  ----

seed_ = args$seed
trim_ = list("5.95"=c(0.05, 0.95), "1.99"=c(0.01, 0.99))[[args$percentiles]]
data_ = data[[args$exposure]][[args$sensitivity_analysis]]
covars_ = list("state"="State_Name", "state.urbanity"=c("State_Name", "urbanity"))[[args$sensitivity_analysis]]

results_match <- all_matching_results_1model(
  seed_,
  data_,
  trim_,
  covars_,
  run_gee_model = F
)

var_arg_a_p_match = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_match")

# save covariate balance plot as png
ggsave(paste0(dir, "results/causal_analyses/", var_arg_a_p_match, "_correlation_plot.png"),
       make_correlation_plot(results_match$cov_bal.capped0.99))

# get mean AC of matched (number of matches capped at 99th percentile) and unadjusted pseudopopulation
results_match$mean_AC_matched <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
)
results_match$mean_AC_unadjusted <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Unmatched", `Absolute Correlation`]
)
results_match[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_match[["cov_bal.capped1"]] <- NULL

# save results as txt file
cat(args$e, "match", args$sensitivity_analysis, args$p,
    sep = "\n",
    file=paste0(dir, "results/causal_analyses/", var_arg_a_p_match, ".txt"), 
    append=TRUE)
lapply(1:length(results_match),
       function(i) cat(paste(names(results_match)[i], results_match[[i]], sep = "\n"),
                       sep = "\n",
                       file=paste0(dir, "results/causal_analyses/", var_arg_a_p_match,".txt"),
                       append=TRUE))

## Weighted CausalGPS ----


var_arg_a_p_weight = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_weight")


results_weight <- all_weighting_results_1model(
  seed_,
  data_,
  trim_,
  covars_
)

# save covariate balance plot as png
ggsave(paste0(dir, "results/causal_analyses/", var_arg_a_p_weight, "_correlation_plot.png"),
       make_correlation_plot(results_weight$cov_bal.capped0.99))

# get mean AC of weighted (capped at 99th percentile) and unadjusted pseudopopulation
results_weight$mean_AC_weighted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
)
results_weight$mean_AC_unadjusted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Unweighted", `Absolute Correlation`]
)
results_weight[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_weight[["cov_bal.capped1"]] <- NULL

# save results as txt file
cat(args$e, "weight", args$sensitivity_analysis, args$p,
    sep = "\n",
    file=paste0(dir, "results/causal_analyses/", var_arg_a_p_weight, ".txt"), 
    append=TRUE)
lapply(1:length(results_weight),
       function(i) cat(paste(names(results_weight)[i], results_weight[[i]], sep = "\n"),
                       sep = "\n",
                       file=paste0(dir, "results/causal_analyses/", var_arg_a_p_weight,".txt"),
                       append=TRUE))
