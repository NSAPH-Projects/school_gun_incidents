# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

##### Get data and functions

df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_with_state <- na.omit(data_with_state)
data_with_state$a <- data_with_state$a / 0.5 # get exposure in half-miles
# data_state_trimmed <- data_with_state[which(data_with_state$a <= 16.2 ),] # get 99th-exposure-percentile-trimmed data

# for sensitivity analysis: get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders, "urban_rural"))
data_with_urbanity_state <- na.omit(data_with_urbanity_state)

# get 1st/99th and 5th/95th percentiles of exposure (for trimming)
# exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))
# formatted1.99 <- paste0("[", round(exposure1.99, 2)[1], ", ", round(exposure1.99, 2)[2], "]")
formatted5.95 <- paste0("[", round(exposure5.95, 2)[1], ", ", round(exposure5.95, 2)[2], "]")


##### CausalGPS matching

all_matching_results_1model <- function(seed, data, trim, cat_confounder_names, quant_confounders = quantitative_confounders){
  set.seed(seed)
  results_list <- list()
  
  # GPS matching
  matched_pop <- get_gps_matched_pseudo_pop(data$y,
                                            data$a,
                                            data[, c(cat_confounder_names, quant_confounders)],
                                            trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(matched_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(matched_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th)")
  
  # Create alternate pseudopopulation where counter is capped
  matched_pop_capped99 <- copy(matched_pop)
  matched_pop_capped99$pseudo_pop$counter_weight[which(matched_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Store logistic regression output
  results_list[["logistic_regression_output"]] <- concatenate_results(get_gps_matched_logistic_results(matched_pop)$coefficients["w", ])
  results_list[["logistic_regression_output_capped99"]] <- concatenate_results(get_gps_matched_logistic_results(matched_pop_capped99)$coefficients["w", ])
  
  # Save covariate balance plots and splines
  for (capped in c(1, .99)){ # quantiles of counter # c(1, .99, .95)
    if (capped == 1){
      pseudo_pop <- matched_pop
    } else if (capped == .99){
      pseudo_pop <- matched_pop_capped99
    }
    
    # for capped pseudopopulations, recalculate covariate balance
    if (capped < 1){
      adjusted_corr_obj <- check_covar_balance(w = as.data.table(pseudo_pop$pseudo_pop$w),
                                               c = subset(pseudo_pop$pseudo_pop, select = quant_confounders),
                                               ci_appr = "matching",
                                               counter_weight = as.data.table(pseudo_pop$pseudo_pop$counter_weight),
                                               nthread = 1,
                                               covar_bl_method = "absolute",
                                               covar_bl_trs = 0.1,
                                               covar_bl_trs_type = "mean",
                                               optimized_compile=TRUE)
      pseudo_pop$adjusted_corr_results <- adjusted_corr_obj$corr_results
    }
    
    # save covariate balance plot
    results_list[[paste0("cov_bal.capped", capped)]] <- get_matched_correlation_plot(pseudo_pop, cat_confounder_names, data$a, subset(data, select = cat_confounder_names), quant_confounders)
    # cov_bal <- make_correlation_plot(pseudo_pop, "matching", cat_confounder_names, data$a, subset(data, select = cat_confounder_names))
  }
  
  return(results_list) # numerical output, to be stored in results table
}

# Get numerical results
state.5.95_match <- all_matching_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
# state.1.99_match <- all_matching_results_1model(100, data_with_state, c(0.01, 0.99), "State_Name")
make_correlation_plot(state.5.95_match$cov_bal.capped0.99)
mean(state.5.95_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
state.5.95_match$logistic_regression_output_capped99

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.05, 0.95), c("State_Name", "urban_rural"))
# state.urbanity.1.99_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.01, 0.99), c("State_Name", "urban_rural"))
make_correlation_plot(state.urbanity.5.95_match$cov_bal.capped0.99)
mean(state.urbanity.5.95_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
state.urbanity.5.95_match$logistic_regression_output_capped99


##### Weight by GPS

all_weighting_results_1model <- function(seed, data, trim, cat_confounder_names, quant_confounders = quantitative_confounders){
  set.seed(seed)
  results_list <- list()
  
  # GPS weighting
  weighted_pop <- get_gps_weighted_pseudo_pop(data$y,
                                              data$a,
                                              data[, c(cat_confounder_names, quant_confounders)],
                                              trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(weighted_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(weighted_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th)")
  
  # Create alternate pseudopopulation where counter is capped
  weighted_pop_capped99 <- copy(weighted_pop)
  weighted_pop_capped99$pseudo_pop$counter_weight[which(weighted_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Store logistic regression output
  results_list[["logistic_regression_output"]] <- concatenate_results(get_gps_weighted_logistic_results(weighted_pop)$coefficients["w", ])
  results_list[["logistic_regression_output_capped99"]] <- concatenate_results(get_gps_weighted_logistic_results(weighted_pop_capped99)$coefficients["w", ])
  
  # Save covariate balance plots
  for (capped in c(1, .99)){ # quantiles of counter # c(1, .99, .95)
    if (capped == 1){
      pseudo_pop <- weighted_pop
    } else if (capped == .99){
      pseudo_pop <- weighted_pop_capped99
    }
    
    # for capped pseudopopulations, recalculate covariate balance
    if (capped < 1){
      adjusted_corr_obj <- check_covar_balance(w = as.data.table(pseudo_pop$pseudo_pop$w),
                                               c = subset(pseudo_pop$pseudo_pop, select = quant_confounders),
                                               ci_appr = "matching",
                                               counter_weight = as.data.table(pseudo_pop$pseudo_pop$counter_weight),
                                               nthread = 1,
                                               covar_bl_method = "absolute",
                                               covar_bl_trs = 0.1,
                                               covar_bl_trs_type = "mean",
                                               optimized_compile=TRUE)
      pseudo_pop$adjusted_corr_results <- adjusted_corr_obj$corr_results
    }
    
    # save covariate balance plot
    results_list[[paste0("cov_bal.capped", capped)]] <- get_weighted_correlation_plot(pseudo_pop, cat_confounder_names, data$a, subset(data, select = cat_confounder_names), quant_confounders)
    # cov_bal <- make_correlation_plot(pseudo_pop, "weighting", cat_confounder_names, data$a, subset(data, select = cat_confounder_names))
  }
  
  return(results_list) # numerical output, to be stored in results table
}

# Get numerical results
state.5.95_weight <- all_weighting_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
# state.1.99_weight <- all_weighting_results_1model(100, data_with_state, c(0.01, 0.99), "State_Name")
make_correlation_plot(state.5.95_weight$cov_bal.capped0.99)
mean(state.5.95_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
state.5.95_weight$logistic_regression_output_capped99

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.05, 0.95), c("State_Name", "urban_rural"))
# state.urbanity.1.99_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.01, 0.99), c("State_Name", "urban_rural"))
make_correlation_plot(state.urbanity.5.95_weight$cov_bal.capped0.99)
mean(state.urbanity.5.95_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
state.urbanity.5.95_weight$logistic_regression_output_capped99
