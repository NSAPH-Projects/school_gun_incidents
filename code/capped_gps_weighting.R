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


## GPS weighting  ----

capped_weighting <- function(data, exposure_name){
  # get GPS-weighted pseudopopulation
  results_weight <- all_weighting_results_1model(seed = 100,
                                                data = data,
                                                trim = c(0.05, 0.95),
                                                cat_covariate_names = c("State_Name", "urbanicity"))
  
  # filename for results
  var_arg_a_p_weight <- paste0(exposure_name, ".", "state.urbanity", ".", "5.95", ".", "weight")
  
  # save covariate balance as csv and plot as png
  fwrite(results_weight$cov_bal.capped0.99, paste0(dir, "results/causal_analyses/capped_weighting/", var_arg_a_p_weight, "_correlation.csv"))
  ggsave(paste0(dir, "results/causal_analyses/capped_weighting/", var_arg_a_p_weight, "_correlation_plot.png"),
         make_correlation_plot(results_weight$cov_bal.capped0.99))
  
  # get mean and max AC of weighted unadjusted pseudopopulation
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
  results_weight[["cov_bal.capped1"]] <- NULL
  
  # save results as txt file
  cat(exposure_name, "weight", "state.urbanity", "5.95",
      sep = "\n",
      file=paste0(dir, "results/causal_analyses/capped_weighting/", var_arg_a_p_weight, ".txt"), 
      append=TRUE)
  lapply(1:length(results_weight),
         function(i) cat(paste(names(results_weight)[i], results_weight[[i]], sep = "\n"),
                         sep = "\n",
                         file=paste0(dir, "results/causal_analyses/capped_weighting/", var_arg_a_p_weight,".txt"),
                         append=TRUE))
}

capped_weighting(any_ffl, exposure_name = "mean_distance_all_persistent_dealers")
capped_weighting(commercial_ffl, exposure_name = "mean_dist_commercial_dealers")
