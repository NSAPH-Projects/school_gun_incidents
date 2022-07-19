# This file contains functions (and a couple constants) used in our causal analysis

all_confounder_names <- c("total_population_2020", "housing_units_per_sq_meter", "Tract_Area_sq_meters",
                          "log_median_hh_income", "schools_per_sq_meter", "census_division_number",
                          "log_median_hh_income_15to24", "total_crime_2021", 
                          "dealers_per_sq_meter", "mental_health_index",
                          "daytime_pop_2021", "prop_white_only", "prop_black_only", 
                          "prop_asian_only", "prop_multiracial", "prop_hispanic_latino", 
                          "prop_food_stamps_2019", "prop_public_assist_income_2019",
                          "prop_below_poverty_2019", "prop_without_vehicles_2019",
                          "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021", 
                          "prop_grad_deg_25plus_2021", "prop_unemployed_2021",
                          "prop_unemployed_16to24_2021", "prop_institutional_group",
                          "prop_noninstitutional_group", "prop_18plus")
quantitative_confounders <- all_confounder_names[!all_confounder_names %in% c("census_division_number")]


load_packages <- function(){
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
  library(WeightIt)
  library(cobalt)
  library(jtools)
  library(MatchIt)
}

load_packages()

read_cleaned_data_as_df <- function(dir = ""){
  return(read.csv(paste0(dir, "data/all_tracts_2020_subset_vars.csv"), header = TRUE, stringsAsFactors = FALSE))
}

# to do: figure out if/why this function returns a character matrix
factorize_cat_vars <- function(data){
  if ("census_division" %in% colnames(data)){
    data$census_division <- as.factor(data$census_division)
  }
  if ("census_region" %in% colnames(data)){
    data$census_region <- as.factor(data$census_region)
  }
  if ("state_fips" %in% colnames(data)){
    data$state_fips <- ifelse(nchar(data$state_fips) == 1, paste0("0", data$state_fips), data$state_fips)
  }
  if ("county_fips" %in% colnames(data)){
    data$county_fips <- ifelse(nchar(data$county_fips) == 1, paste0("0", data$county_fips), data$county_fips)
  }
  return(data)
}

# to do: figure out if/why this function returns a character matrix
get_confounders_matrix <- function(data, confounder_names){
  # x <- factorize_cat_vars(data[, confounder_names])
  x <- data[, confounder_names]
  x <- t(apply(x, 1, unlist)) # turn data frame into matrix, for CausalGPS functions
  return(x)
}

get_analysis_df <- function(data, treatment = "mean_total_km", confounder_names){
  data <- as.data.frame(data)
  y <- data[, "binary_shooting_incident"]
  a <- data[, treatment]
  # x <- factorize_cat_vars(data[, confounder_names])
  x <- data[, confounder_names]
  return(cbind(y, a, x))
}

match_discretized <- function(data, control_name = "1", confounder_names,
                              exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                              caliper = NULL, seed = 42){
  set.seed(seed)
  
  treatments <- levels(as.factor(data$a)) # Levels of treatment variable
  data$x =  data[, confounder_names]
  data$x <- t(apply(data$x, 1, unlist)) # Get confounders as matrix
  data$match.weights <- 1 # Initialize matching weights
  cov_bal_plots <- list() # Initalize pairwise covariate balance plots
  
  for (i in treatments[treatments != control_name]) {
    d <- data[data$a %in% c(i, control_name),] # Subset just the control and treatment i
    d$treat_i <- as.numeric(d$a != i) # Create new binary treatment variable # why not d$a == i?
    m <- matchit(treat_i ~ x, data = d, method = "nearest", exact = exact_matches,  replace = TRUE, caliper = caliper)
    data[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] # Assign matching weights
    
    cov_bal_plots[[i]] <- love.plot(m,
                                    drop.distance = TRUE, 
                                    var.order = "unadjusted",
                                    abs = TRUE,
                                    line = TRUE, 
                                    thresholds = c(m = .1))  +
      ggtitle(paste("Covariate Balance - Group", i, "vs Group", control_name))
  }
  
  match.weights <- data$match.weights
  
  #Estimate treatment effects
  model_summary <- summary(glm(y ~ relevel(as.factor(a), control_name),
                               data = data[data$match.weights > 0,],
                               weights = match.weights),
                           family = binomial(link = "logit"), robust = "HC1", digits = 5)
  
  return(list("match.weights" = match.weights, "cov_bal_plots" = cov_bal_plots, "model_summary" = model_summary))
}

# Function to winsorize counter (i.e., # of times each included observation is GPS-matched) at high quantile
# Parameter quantile should be between 0.5 and 1
winsorize_counter_onesided <- function(counter, quantile){
  cutoff <- quantile(counter, quantile)
  return(ifelse(counter > cutoff, cutoff, counter))
}

subset_state_data <- function(fips, trim_exposure = T){
  data_state <- data_analysis[data_analysis$state_fips == fips, ]
  if (trim_exposure){
    data_state <- trim_exposure(data_state, data_state$a) # trim exposure at 95th percentile at state level
  }
  data_state$state_fips <- NULL
  if ("census_division_number" %in% colnames(data_analysis)){
    data_state$census_division_number <- NULL
  }
  return(data_state)
}

naive_state_logistic <- function(fips, trim_exposure){
  state_data <- subset_state_data(fips, trim_exposure)
  logistic_model <- glm(y ~ ., 
                        data = state_data, 
                        family = "binomial")
  return(summary(logistic_model))
}

generate_state_pseudo_pop <- function(fips){
  data_state <- subset_state_data(fips)
  matched_pop <- get_matched_pseudo_pop(data_state$y, data_state$a, subset(data_state, select = confounders_without_division))
  return(matched_pop)
}

make_state_correlation_plot <- function(fips, matched_pop){
  data_state <- subset_state_data(fips)
  return(make_matched_correlation_plot(matched_pop, data_state$a, subset(data_state, select = confounders_without_division), confounders_without_division))
}

weight_discretized <- function(df){
  w.out <- weightit(as.factor(a) ~ . -y -a -match.weights, data = df, focal = "1", estimand = "ATE")
  
  #Check balance
  print(bal.tab(w.out, which.treat = .all))
  
  #Estimate treatment effects (using jtools to get robust SEs)
  #(Can also use survey package)
  print(summ(glm(y ~ relevel(as.factor(a), "1"), data = df, weights = w.out$weights), robust = "HC1", digits = 5))
  print(summ(glm(y ~ a, data = df,
           weights = w.out$weights), robust = "HC1"))
  return(0)
}


##### THE FOLLOWING FUNCTIONS ARE OUTDATED (CONTINUOUS EXPOSURE) #####

# exposure_vec: enter exposure as a vector of values, not as name of exposure variable
trim_exposure <- function(df, exposure_vec, trim_quantile = 0.95){
  return(df[exposure_vec <= quantile(exposure_vec, trim_quantile), ])
}

get_gps_matched_pseudo_pop <- function(outcome, exposure, confounders, trim_quantiles = c(0.05, 0.95)){
  # if ("census_division" %in% colnames(confounders)){
  #   confounders$census_division <- NULL # to do: figure out if this function can use categorical variables
  # }
  
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

make_matched_correlation_plot <- function(matched_pop, confounder_names){
  # confounder_names <- confounder_names[confounder_names != "census_division"] # to do: figure out if this function can use categorical variables
  
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

get_gps_matched_logistic_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                  family = binomial,
                                  data = pseudo,
                                  ci_appr = "matching")
  return(summary(outcome))
}

get_gps_matched_semiparametric_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  semi_outcome <- estimate_semipmetric_erf(formula = Y ~ w,
                                           family = binomial,
                                           data = pseudo,
                                           ci_appr = "matching")
  return(summary(semi_outcome))
}

get_gps_weighting_results <- function(gps_pop){
  pseudo <- gps_pop$pseudo_pop
  weighting_outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                            family = binomial,
                                            data = pseudo,
                                            ci_appr = "weighting")
  return(summary(weighting_outcome))
}

get_gps_matched_nonparametric_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  erf_obj <- estimate_npmetric_erf(as.double(pseudo$Y),
                                   as.double(pseudo$w),
                                   bw_seq=seq(0.2,2,0.2),
                                   w_vals = seq(0,15,0.5),
                                   nthread = 1)
  
  return(plot(erf_obj))
}
