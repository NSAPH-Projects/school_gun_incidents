# This file contains functions (and a couple constants) used in our causal analysis

all_confounder_names <- c("total_population_2020", "housing_units_per_sq_meter",
                          "log_median_hh_income", "schools_per_sq_meter", "state_fips", 
                          "county_fips",
                          "log_median_hh_income_15to24", "total_crime_2021", 
                          "dealers_per_sq_meter",
                          "daytime_pop_2021", "prop_white_only", "prop_black_only", 
                          "prop_asian_only", "prop_multiracial", "prop_hispanic_latino", 
                          "prop_food_stamps_2019", "prop_public_assist_income_2019",
                          "prop_below_poverty_2019", "prop_without_vehicles_2019",
                          "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021", 
                          "prop_grad_deg_25plus_2021", "prop_unemployed_2021",
                          "prop_unemployed_16to24_2021", "prop_institutional_group",
                          "prop_noninstitutional_group", "prop_18plus")

state_confounders <- all_confounder_names[!all_confounder_names %in% c("state_fips")]

load_packages <- function(clear_env = F){
  if (clear_env){
    rm(list=ls())
  }
  library(CausalGPS)
  library(MASS)
  library(ggplot2)
  library(tidyr)
  library(pscl)
  library(dplyr)
  library(SuperLearner)
  library(data.table)
  library(lubridate)
  library(stringr)
}

read_all_data_as_df <- function(dir){
  return(read.csv(paste0(dir, "data/all_tracts_2020_subset_vars.csv"), header = TRUE, stringsAsFactors = FALSE))
}

get_analysis_df <- function(data, treatment = "mean_total_km", confounders){
  data <- as.data.frame(data)
  
  ## Outcome Variable 
  y <- data[, "binary_shooting_incident"]
  
  ## Treatment Variable
  a <- data[, treatment]
  # ceiling_a <- ceiling(a)
  
  ## Confounders
  x <- data[, confounders]
  x <- t(apply(x, 1, unlist))
  
  ## Generate Analysis Data
  data_analysis <- cbind(y, a, as.data.frame(x))
  return(data_analysis)
}

trim_exposure <- function(df, exposure, trim_quantile = 0.95){
  return(df[exposure <= quantile(exposure, trim_quantile), ])
}

get_matched_pseudo_pop <- function(outcome, exposure, confounders, trim_quantiles = c(0.05, 0.95)){
  return(generate_pseudo_pop(Y = outcome,
                             w = exposure,
                             c = as.data.frame(confounders),
                             ci_appr = "matching",
                             pred_model = "sl",
                             gps_model = "parametric",
                             use_cov_transform = TRUE,
                             transformers = list("pow2", "pow3"),
                             sl_lib = c("m_xgboost"),
                             params = list(xgb_nrounds = c(50)),
                             nthread = 4,
                             covar_bl_method = "absolute",
                             covar_bl_trs = 0.1,
                             covar_bl_trs_type = "mean",
                             trim_quantiles = trim_quantiles, 
                             optimized_compile = TRUE, 
                             max_attempt = 5,
                             matching_fun = "matching_l1",
                             delta_n = 0.2,
                             scale = 1.0))
}

get_gps <- function(outcome, exposure, confounders, trim_quantiles = c(0.05, 0.95)){
  return(generate_pseudo_pop(Y = outcome,
                             w = exposure,
                             c = as.data.frame(confounders),
                             ci_appr = "weighting",
                             pred_model = "sl",
                             gps_model = "parametric",
                             use_cov_transform = TRUE,
                             transformers = list("pow2", "pow3"),
                             sl_lib = c("m_xgboost"),
                             params = list(xgb_nrounds = c(50)),
                             nthread = 4,
                             covar_bl_method = "absolute",
                             covar_bl_trs = 0.1,
                             covar_bl_trs_type = "mean",
                             max_attempt = 5))
}

make_matched_correlation_plot <- function(matched_pop, exposure, confounders, confounder_names){
  # get correlations of matched and unmatched data
  cor_val_matched <- matched_pop$adjusted_corr_results
  cor_val_unmatched <- matched_pop$original_corr_results
  
  # gather correlations into 1 data frame
  abs_cor = data.frame(cov = confounder_names,
                       unmatched = cor_val_unmatched$absolute_corr,
                       matched = cor_val_matched$absolute_corr) %>%
    gather(c(unmatched, matched), key = 'dataset', value = 'absolute correlation')
  
  # make plot
  p <- ggplot(abs_cor, aes(x = cov, y = `absolute correlation`, color = dataset, group = dataset)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(p)
}

get_matched_logistic_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                  family = binomial,
                                  data = pseudo,
                                  ci_appr = "matching")
  return(summary(outcome))
}

get_matched_semiparametric_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  semi_outcome <- estimate_semipmetric_erf(formula = Y ~ w,
                                           family = binomial,
                                           data = pseudo,
                                           ci_appr = "matching")
  return(summary(semi_outcome))
}

get_matched_weighting_results <- function(gps_pop){
  pseudo <- gps_pop$pseudo_pop
  weighting_outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                            family = binomial,
                                            data = pseudo,
                                            ci_appr = "weighting")
  return(summary(weighting_outcome))
}

get_matched_nonparametric_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  erf_obj <- estimate_npmetric_erf(as.double(pseudo$Y),
                                   as.double(pseudo$w),
                                   bw_seq=seq(0.2,2,0.2),
                                   w_vals = seq(0,15,0.5),
                                   nthread = 1)
  
  return(plot(erf_obj))
}

# Function to winsorize counter (i.e., # of times each included observation is GPS-matched) at high quantile
# Parameter quantile should be between 0.5 and 1
winsorize_counter_onesided <- function(counter, quantile){
  cutoff <- quantile(counter, quantile)
  return(ifelse(counter > cutoff, cutoff, counter))
}

# function to map 1 variable to {1,2,3,4} by quartile
quartile_var <- function(var){
  quartiles <- quantile(var, c(0.25, 0.5, 0.75), na.rm=T)
  var_quartiled <- ifelse(var <= quartiles[1], 1,
                          ifelse(var <= quartiles[2], 2,
                                 ifelse(var <= quartiles[3], 3, 4)))
  return(var_quartiled)
}

subset_state_data_trimmed <- function(fips){
  data_state <- data_analysis[data_analysis$state_fips == fips, ]
  data_state <- trim_exposure(data_state, data_state$a) # trim exposure at 95th percentile at state level
  data_state$state_fips <- NULL
  return(data_state)
}

generate_state_pseudo_pop <- function(fips){
  data_state <- subset_state_data_trimmed(fips)
  matched_pop <- get_matched_pseudo_pop(data_state$y, data_state$a, subset(data_state, select = state_confounders))
  return(matched_pop)
}

make_state_correlation_plot <- function(fips, matched_pop){
  data_state <- subset_state_data_trimmed(fips)
  return(make_matched_correlation_plot(matched_pop, data_state$a, subset(data_state, select = state_confounders), state_confounders))
}
