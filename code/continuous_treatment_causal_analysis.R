## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "code/helper_functions.R"))
source(paste0(dir, "code/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## Prepare dataset for main analysis ----
data_with_state <- get_analysis_df(
  df, 
  "mean_total_miles", 
  c("State_Name", quantitative_covariates)
  )

## for sensitivity analysis: get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(
  df, 
  "mean_total_miles", 
  c("State_Name", quantitative_covariates, "urban_rural"))

## Perform causal analysis
results <- list()

## Matching CausalGPS  ----

## state.5.95_match
results[["state.5.95_match"]] <- all_matching_results_1model(
  100, 
  data_with_state, 
  c(0.05, 0.95), 
  "State_Name"
  )
ggsave(paste0(dir, "results/gps_results/state.5.95_match_correlation_plot.png"), 
       make_correlation_plot(results[["state.5.95_match"]]$cov_bal.capped0.99))
results[["state.5.95_match"]]$abs_corr_mean <- mean(
  results[["state.5.95_match"]]$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
  ) # get mean AC of matched pseudopopulation
lapply(results[["state.5.95_match"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.5.95_match.txt"), 
                       append=TRUE))

## state.1.99_match
results[["state.1.99_match"]] <- all_matching_results_1model(
  100, 
  data_with_state, 
  c(0.01, 0.99), 
  "State_Name"
  )
ggsave(paste0(dir, "results/gps_results/state.1.99_match_correlation_plot.png"), 
       make_correlation_plot(results[["state.1.99_match"]]$cov_bal.capped0.99))
results[["state.1.99_match"]]$abs_corr_mean <- mean(
  results[["state.1.99_match"]]$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
  ) # get mean AC of matched pseudopopulation
lapply(results[["state.1.99_match"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.1.99_match.txt"), 
                       append=TRUE))

## state.urbanity.5.95_match
results[["state.urbanity.5.95_match"]] <- all_matching_results_1model(
  100, 
  data_with_urbanity_state, 
  c(0.05, 0.95), 
  c("State_Name", "urban_rural")
  )
ggsave(paste0(dir, "results/gps_results/state.urbanity.5.95_match_correlation_plot.png"), 
       make_correlation_plot(results[["state.urbanity.5.95_match"]]$cov_bal.capped0.99))
results[["state.urbanity.5.95_match"]]$abs_corr_mean <- mean(
  results[["state.urbanity.5.95_match"]]$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
  ) # get mean AC of matched pseudopopulation
lapply(results[["state.urbanity.5.95_match"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.urbanity.5.95_match.txt"), 
                       append=TRUE))

## state.urbanity.1.99_match
results[["state.urbanity.1.99_match"]] <- all_matching_results_1model(
  100, 
  data_with_urbanity_state, 
  c(0.01, 0.99), 
  c("State_Name", "urban_rural")
)
ggsave(paste0(dir, "results/gps_results/state.urbanity.1.99_match_correlation_plot.png"), 
       make_correlation_plot(results[["state.urbanity.1.99_match"]]$cov_bal.capped0.99))
results[["state.urbanity.1.99_match"]]$abs_corr_mean <- mean(
  results[["state.urbanity.1.99_match"]]$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results[["state.urbanity.1.99_match"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.urbanity.1.99_match.txt"), 
                       append=TRUE))

## Weighted CausalGPS ----

## state.5.95_weight
results[["state.5.95_weight"]] <- all_weighting_results_1model(
  100, 
  data_with_state, 
  c(0.05, 0.95), 
  "State_Name"
  )
ggsave(paste0(dir, "results/gps_results/state.5.95_weight_correlation_plot.png"), 
       make_correlation_plot(results[["state.5.95_weight"]]$cov_bal.capped0.99))
results[["state.5.95_weight"]]$abs_corr_mean <- mean(
  results[["state.5.95_weight"]]$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results[["state.5.95_weight"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.5.95_weight.txt"), 
                       append=TRUE))

## state.1.99_weight
results[["state.1.99_weight"]] <- all_weighting_results_1model(
  100, 
  data_with_state, 
  c(0.01, 0.99), 
  "State_Name"
)
ggsave(paste0(dir, "results/gps_results/state.1.99_weight_correlation_plot.png"), 
       make_correlation_plot(results[["state.1.99_weight"]]$cov_bal.capped0.99))
results[["state.1.99_weight"]]$abs_corr_mean <- mean(
  results[["state.1.99_weight"]]$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results[["state.1.99_weight"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.1.99_weight.txt"), 
                       append=TRUE))

## state.urbanity.5.95_weight
results[["state.urbanity.5.95_weight"]] <- all_weighting_results_1model(
  100, 
  data_with_urbanity_state, 
  c(0.05, 0.95), 
  "State_Name"
)
ggsave(paste0(dir, "results/gps_results/state.urbanity.5.95_weight_correlation_plot.png"), 
       make_correlation_plot(results[["state.urbanity.5.95_weight"]]$cov_bal.capped0.99))
results[["state.urbanity.5.95_weight"]]$abs_corr_mean <- mean(
  results[["state.urbanity.5.95_weight"]]$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results[["state.urbanity.5.95_weight"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.urbanity.5.95_weight.txt"), 
                       append=TRUE))

## state.urbanity.1.99_weight
results[["state.urbanity.1.99_weight"]] <- all_weighting_results_1model(
  100, 
  data_with_urbanity_state, 
  c(0.01, 0.99), 
  "State_Name"
)
ggsave(paste0(dir, "results/gps_results/state.urbanity.1.99_weight_correlation_plot.png"), 
       make_correlation_plot(results[["state.urbanity.1.99_weight"]]$cov_bal.capped0.99))
results[["state.urbanity.1.99_weight"]]$abs_corr_mean <- mean(
  results[["state.urbanity.1.99_weight"]]$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
) # get mean AC of matched pseudopopulation
lapply(results[["state.urbanity.1.99_weight"]], 
       function(x) cat(paste(x, collapse = " "), 
                       sep = "\n", 
                       file=paste0(dir, "results/gps_results/state.urbanity.1.99_weight.txt"), 
                       append=TRUE))
