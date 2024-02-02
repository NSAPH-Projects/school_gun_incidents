##### Note: GEE model (line 65) may take up to 96 GB for trim 5/95, or up to 250 GB for trim 1/99, to run #####

## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
# library(argparse)
# 
# # Define parser arguments ----
# parser <- ArgumentParser()
# parser$add_argument("-e", "--exposure", default="mean_distance_all_persistent_dealers",
#                     help="Exposure variable 'mean_distance_all_persistent_dealers' or 'mean_dist_commercial_dealers'", type="character")
# parser$add_argument("-sd", "--seed", default=100,
#                     help="seed value", type="integer")
# parser$add_argument("-s", "--sensitivity_analysis", default="state",
#                     help="Sensitivity analysis 'state' or 'state.urbanicity'", type="character")
# parser$add_argument("-p", "--percentiles", default="5.95",
#                     help="Percentiles of exposure '5.95' or '1.99'", type="character") 
# args = parser$parse_args()


## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/sensitivity_analyses/final_data_sep2023.csv"))

## Get dataset for main analysis ----
data <- get_analysis_df(df, "mean_euclidean_dist_commercial_dealers", c(categorical_covariates, quantitative_covariates))


## Perform causal analysis

seed_ = 100
trim_ = c(0.05, 0.95)
data_ = data
covars_ = c("State_Name", "urbanicity")


## Matching CausalGPS  ----

results_match <- all_matching_results_1model(
  seed_,
  data_,
  trim_,
  covars_,
  run_gee_model = T
)

##### to do: edit the below

var_arg_a_p_match = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_match")

# save covariate balance as csv and plot as png
fwrite(results_match$cov_bal.capped0.99, paste0(dir, "results/causal_analyses/", var_arg_a_p_match, "_correlation.csv"))
ggsave(paste0(dir, "results/causal_analyses/", var_arg_a_p_match, "_correlation_plot.png"),
       make_correlation_plot(results_match$cov_bal.capped0.99))

# get mean and max AC of matched (number of matches capped at 99th percentile) and unadjusted pseudopopulation
results_match$mean_AC_matched <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
)
results_match$mean_AC_unadjusted <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_match$max_AC_matched <- max(
  results_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
)
results_match$max_AC_unadjusted <- max(
  results_match$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_match[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_match[["cov_bal.capped1"]] <- NULL

# save results as csv file
results_as_table <- data.table(Exposure = args$e,
                               Model = "Match",
                               Cat_Confounder = args$s,
                               Trim = args$p,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
results_as_table <- cbind(results_as_table, as.data.table(results_match))
fwrite(results_as_table, paste0(dir, "results/causal_analyses/", var_arg_a_p_match, ".csv"))


## Weighted CausalGPS ----

var_arg_a_p_weight = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_weight")

results_weight <- all_weighting_results_1model(
  seed_,
  data_,
  trim_,
  covars_
)

# save covariate balance as csv and plot as png
fwrite(results_weight$cov_bal.capped0.99, paste0(dir, "results/causal_analyses/", var_arg_a_p_weight, "_correlation.csv"))
ggsave(paste0(dir, "results/causal_analyses/", var_arg_a_p_weight, "_correlation_plot.png"),
       make_correlation_plot(results_weight$cov_bal.capped0.99))

# get mean and max AC of weighted (capped at 99th percentile) and unadjusted pseudopopulation
results_weight$mean_AC_weighted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
)
results_weight$mean_AC_unadjusted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_weight$max_AC_weighted <- max(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
)
results_weight$max_AC_unadjusted <- max(
  results_weight$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_weight[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_weight[["cov_bal.capped1"]] <- NULL

# save results as csv file
results_as_table <- data.table(Exposure = args$e,
                               Model = "Weight",
                               Cat_Confounder = args$s,
                               Trim = args$p,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
results_as_table <- cbind(results_as_table, as.data.table(results_weight))
fwrite(results_as_table, paste0(dir, "results/causal_analyses/", var_arg_a_p_weight, ".csv"))
