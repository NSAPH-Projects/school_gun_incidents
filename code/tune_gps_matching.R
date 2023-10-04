## Load packages ----

library(ggplot2)
library(tidyr)
library(data.table)


## Load functions ----

dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))


## Load datasets ----

df <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))
data <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", quantitative_covariates))


## Matching calipers to test  ----

# caliper_candidates <- (quantile(data$a, 0.75) - quantile(data$a, 0.25)) / 1:10 # try calipers that make sense based on IQR of exposure (in half-miles)
caliper_candidates <- 1:5/10


## GPS matching  ----

save_tuning_results <- function(caliper){
  # get GPS-matched pseudopopulation
  results_match <- all_matching_results_1model(seed = 100,
                                               data = data,
                                               trim = c(0.05, 0.95),
                                               cat_covariate_names = "State_Name",
                                               run_gee_model = F,
                                               caliper = caliper)
  
  # filename for results
  var_arg_a_p_match <- paste0("mean_dist_commercial_dealers", ".", "state", ".", "5.95", ".", "matching_caliper_", round(caliper, 2))
  
  # save covariate balance as csv and plot as png
  fwrite(results_match$cov_bal.uncapped, paste0(dir, "results/causal_analyses/tune_matching/uncapped_matching/", var_arg_a_p_match, "_correlation.csv"))
  ggsave(paste0(dir, "results/causal_analyses/tune_matching/uncapped_matching/", var_arg_a_p_match, "_correlation_plot.png"),
         make_correlation_plot(results_match$cov_bal.uncapped))
  
  # get mean and max AC of matched (number of matches capped at 99th percentile) and unadjusted pseudopopulation
  results_match$mean_AC_matched <- mean(
    results_match$cov_bal.uncapped[Dataset == "Matched", `Absolute Correlation`]
  )
  results_match$mean_AC_unadjusted <- mean(
    results_match$cov_bal.uncapped[Dataset == "Unadjusted", `Absolute Correlation`]
  )
  results_match$max_AC_matched <- max(
    results_match$cov_bal.uncapped[Dataset == "Matched", `Absolute Correlation`]
  )
  results_match$max_AC_unadjusted <- max(
    results_match$cov_bal.uncapped[Dataset == "Unadjusted", `Absolute Correlation`]
  )
  results_match[["cov_bal.uncapped"]] <- NULL
  
  # save results as txt file
  cat("mean_dist_commercial_dealers", "match", "state", "5.95", caliper,
      sep = "\n",
      file=paste0(dir, "results/causal_analyses/tune_matching/uncapped_matching/", var_arg_a_p_match, ".txt"), 
      append=TRUE)
  lapply(1:length(results_match),
         function(i) cat(paste(names(results_match)[i], results_match[[i]], sep = "\n"),
                         sep = "\n",
                         file=paste0(dir, "results/causal_analyses/tune_matching/uncapped_matching/", var_arg_a_p_match,".txt"),
                         append=TRUE))
}
sapply(caliper_candidates, save_tuning_results)
