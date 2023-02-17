## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)

# define parser arguments ----
parser <- ArgumentParser()

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

source(paste0(dir, "lib/helper_functions.R"))
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

results <- all_matching_results_1model(
  seed_,
  data_,
  trim_,
  covars_
)

var_arg_a_p_match = paste0(args$sensitivity_analysis, ".", args$percentiles,"_match")

ggsave(paste0(dir, "results/", var_arg_a_p_match, "_correlation_plot.png"),
       make_correlation_plot(results$cov_bal.capped0.99))

results$abs_corr_mean <- mean(
  results$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results,
       function(x) cat(paste(x, collapse = " "),
                       sep = "\n",
                       file=paste0(dir, "results/", var_arg_a_p_match,".txt"),
                       append=TRUE))

## Weighted CausalGPS ----


var_arg_a_p_weight = paste0(args$sensitivity_analysis, ".", args$percentiles,"_weight")


results_weight <- all_weighting_results_1model(
  seed_,
  data_,
  trim_,
  "State_Name"
)


ggsave(paste0(dir, "results/", var_arg_a_p_weight, "_correlation_plot.png"),
       make_correlation_plot(results_weight$cov_bal.capped0.99))
results_weight$abs_corr_mean <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results,
       function(x) cat(paste(x, collapse = " "),
                       sep = "\n",
                       file=paste0(dir, "results/", var_arg_a_p_weight, ".txt"),
                       append=TRUE))
