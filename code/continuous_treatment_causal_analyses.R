##### Note: GEE model (line 65) may take up to 96 GB for trim 5/95, or up to 250 GB for trim 1/99, to run #####

## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)

# define parser arguments ----
parser <- ArgumentParser()

# note: the -p and -t arguments are redundant
parser$add_argument("-s", "--seed", default=100,
                    help="seed value", type="integer")
parser$add_argument("-sa", "--sensitivity_analysis", default="state",
                    help="Sensitivity analysis 'state' or 'state.urbanity'", type="character")
parser$add_argument("-p", "--percentiles", default="5.95",
                    help="Percentiles of exposure '5.95' or '1.99'", type="character") 
parser$add_argument("-t", "--trim_quantiles", default="95",
                    help="trim quantiles '95' or '99'", type="character") 
args = parser$parse_args()


## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## Prepare dataset for main analysis ----

data <- list()

data[["state"]] <- get_analysis_df(
  df,
  "mean_total_miles",
  c("State_Name", quantitative_covariates)
)

## for sensitivity analysis: get data including urban_rural variable

data[["state.urbanity"]] <- get_analysis_df(
  df,
  "mean_total_miles",
  c("State_Name", quantitative_covariates, "urban_rural"))


## Perform causal analysis

## Matching CausalGPS  ----

seed_ = args$seed
trim_ = list("95"=c(0.05, 0.95), "99"=c(0.01, 0.99))[[args$trim_quantiles]]
data_ = data[[args$sensitivity_analysis]]
covars_ = list("state"="State_Name", "state.urbanity"=c("State_Name", "urban_rural"))[[args$sensitivity_analysis]]

results_match <- all_matching_results_1model(
  seed_,
  data_,
  trim_,
  covars_,
  run_gee_model = T
)

var_arg_a_p_match = paste0(args$sensitivity_analysis, ".", args$percentiles,"_match")

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
lapply(1:length(results_match),
       function(i) cat(paste(names(results_match)[i], results_match[[i]], collapse = ": "),
                       sep = "\n",
                       file=paste0(dir, "results/causal_analyses/", var_arg_a_p_match,".txt"),
                       append=TRUE))

## Weighted CausalGPS ----


var_arg_a_p_weight = paste0(args$sensitivity_analysis, ".", args$percentiles,"_weight")


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
lapply(1:length(results_weight),
       function(i) cat(paste(names(results_weight)[i], results_weight[[i]], collapse = ": "),
                       sep = "\n",
                       file=paste0(dir, "results/causal_analyses/", var_arg_a_p_weight,".txt"),
                       append=TRUE))
