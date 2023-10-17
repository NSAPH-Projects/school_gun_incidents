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
any_ffl <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("State_Name", "urbanicity", quantitative_covariates))
commercial_ffl <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", "urbanicity", quantitative_covariates))


## GPS matching  ----

capped_matching <- function(data, exposure_name){
  # get GPS-matched pseudopopulation
  results_match <- all_matching_results_1model(seed = 100,
                                               data = data,
                                               trim = c(0.05, 0.95),
                                               cat_covariate_names = c("State_Name", "urbanicity"),
                                               run_gee_model = T)
  
  # filename for results
  var_arg_a_p_match <- paste0(exposure_name, ".", "state.urbanity", ".", "5.95", ".", "match")
  
  # save covariate balance as csv and plot as png
  fwrite(results_match$cov_bal.capped0.99, paste0(dir, "results/causal_analyses/capped_matching/", var_arg_a_p_match, "_correlation.csv"))
  ggsave(paste0(dir, "results/causal_analyses/capped_matching/", var_arg_a_p_match, "_correlation_plot.png"),
         make_correlation_plot(results_match$cov_bal.capped0.99))
  
  # get mean and max AC of matched and unadjusted pseudopopulation
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
  results_match[["cov_bal.capped1"]] <- NULL
  
  # save results as txt file
  cat(exposure_name, "match", "state.urbanity", "5.95",
      sep = "\n",
      file=paste0(dir, "results/causal_analyses/capped_matching/", var_arg_a_p_match, ".txt"), 
      append=TRUE)
  lapply(1:length(results_match),
         function(i) cat(paste(names(results_match)[i], results_match[[i]], sep = "\n"),
                         sep = "\n",
                         file=paste0(dir, "results/causal_analyses/capped_matching/", var_arg_a_p_match,".txt"),
                         append=TRUE))
}

capped_matching(any_ffl, exposure_name = "mean_distance_all_persistent_dealers")
capped_matching(commercial_ffl, exposure_name = "mean_dist_commercial_dealers")
