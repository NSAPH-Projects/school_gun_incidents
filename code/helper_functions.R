# This file contains functions (and a couple constants) used in our causal analysis

quantitative_confounders <- c("total_population_2020", "housing_units_per_sq_meter", "Tract_Area_sq_meters",
                          "log_median_hh_income", "schools_per_sq_meter",
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
all_confounder_names <- c("census_division_number", quantitative_confounders)
confounders_incl_urban_rural <- c(all_confounder_names, "urban_rural")


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
  library(lmtest)
  library(sandwich)
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
  if ("census_division_number" %in% colnames(data)){
    data$census_division_number <- as.factor(data$census_division_number)
  }
  if ("census_region" %in% colnames(data)){
    data$census_region <- as.factor(data$census_region)
  }
  if ("GEOID" %in% colnames(data)){
    data$GEOID <- as.factor(data$GEOID)
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
  x <- factorize_cat_vars(data[, confounder_names])
  return(cbind(y, a, x))
}

round_results <- function(x){
  rounded <- format(round(as.numeric(x), 4), scientific = F)
  if (rounded == 0) rounded <- format(round(as.numeric(x), 5), scientific = F)
  return(rounded)
}

codify_significance <- function(significance){
  significance <- as.numeric(significance)
  if (significance < 0.001) return("***")
  else if (significance < 0.01) return("**")
  else if (significance < 0.05) return("*")
  else if (significance < 0.1) return(".")
  else return(" ")
}

concatenate_results <- function(results_row){
  estimate <- round_results(results_row["Estimate"])
  SE <- round_results(results_row["Std. Error"])
  if ("Pr(>|z|)" %in% names(results_row)){
    signif_code <- codify_significance(results_row["Pr(>|z|)"])
  } else{
    signif_code <- codify_significance(results_row["Pr(>|t|)"])
  }
  return(paste0(estimate, " (", SE, ") ", signif_code))
}

match_binary_exposure <- function(formula, df, seed = 42, exact_vars = NULL, one_to_one = F){
  set.seed(seed)
  matched_pop <- matchit(formula,
                         data = df,
                         estimand = "ATC",
                         method = "nearest",
                         replace = !one_to_one,
                         exact = exact_vars)
  return(matched_pop)
}

get_match.data_outcome_model <- function(matched_pop){
  matched.data <- match.data(matched_pop)
  outcome <- glm(y ~ a,
                 data = matched.data,
                 weights = weights,
                 family = quasibinomial(link = "logit"))
  return(outcome)
}

# Heteroskedasticity Robust SE (MacKinnon and White, 1985)
get_HC1_results <- function(matched_pop){
  outcome <- get_match.data_outcome_model(matched_pop)
  results <- summary(outcome, robust = "HC1", digits = 5)
  return(results$coefficients["a", ])
}

# Heteroskedasticity Consistent SE (Zeileis, 2006)
get_vcovHC_results <- function(matched_pop){
  outcome <- get_match.data_outcome_model(matched_pop)
  results <- coeftest(outcome, vcov. = vcovHC)
  return(results["a", ])
}

# Clustered SE
get_vcovCL_results <- function(matched_pop){
  gm <- get_matches(matched_pop)
  model <- glm(y ~ a, data = gm, weights = weights,
               family = quasibinomial(link = "logit"))
  # summary(model)
  results <- coeftest(model, vcov. = vcovCL, cluster = ~ id + subclass)
  return(results["a", ])
}

loveplot_star_raw <- function(matched_pop, title = "Covariate Balance"){
  love.plot(matched_pop,
            drop.distance = TRUE, 
            var.order = "unadjusted",
            abs = TRUE,
            line = TRUE, 
            thresholds = c(m = .1),
            stars = "raw",
            title = title)
}

explore_matches <- function(matched_pop){
  cat("Number of control units = Number of included/matched control units = Number of matches =", length(matched_pop$match.matrix))
  cat("Number of matched treated units:", length(unique(matched_pop$match.matrix)))
  cat("Number of units that are not matched (all are treated units):", sum(matched_pop$weights == 0))
  cat("Max times any [included/matched] treated unit is matched:", max(table(matched_pop$match.matrix)))
  cat("Min times any [included/matched] treated unit is matched:", min(table(matched_pop$match.matrix)))
  print("Distribution of number of times any [included/matched] treated unit is matched")
  quantile(as.numeric(table(matched_pop$match.matrix)), c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
  print("IPW weights calculating from matches:")
  summary(matched_pop$weights)
  
  # n_matched_pop <- nrow(match.data(matched_pop))
  # n_matched_pop
  # n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
  # n_imbalanced
}


##### THE FOLLOWING FUNCTIONS ARE OUTDATED (4-LEVEL EXPOSURE) #####

match_discretized <- function(data, control_name = "1", confounder_names,
                              exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                              caliper = NULL, seed = 42, confounders_in_regression = F){
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
                                    thresholds = c(m = .1),
                                    stars = "raw")  +
      ggtitle(paste("Covariate Balance - Group", i, "vs Group", control_name))
  }
  
  match.weights <- data$match.weights
  
  #Estimate treatment effects
  if (confounders_in_regression){
    model_summary <- summary(glm(y ~ relevel(as.factor(a), control_name) + x,
                                 data = data[data$match.weights > 0,],
                                 weights = match.weights),
                             family = binomial(link = "logit"), robust = "HC1", digits = 5)
  } else{
    model_summary <- summary(glm(y ~ relevel(as.factor(a), control_name),
                                 data = data[data$match.weights > 0,],
                                 weights = match.weights),
                             family = binomial(link = "logit"), robust = "HC1", digits = 5)
  }
  
  return(list("match.weights" = match.weights, "cov_bal_plots" = cov_bal_plots, "model_summary" = model_summary))
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

logit <- function(p){
  return(log(p/(1-p)))
}

inverse_logit <- function(x){
  return(1 / (1 + exp(-x)))
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

# Function to winsorize counter (i.e., # of times each included observation is GPS-matched) at high quantile
# Parameter quantile should be between 0.5 and 1
winsorize_counter_onesided <- function(counter, quantile){
  cutoff <- quantile(counter, quantile)
  return(ifelse(counter > cutoff, cutoff, counter))
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

get_gps_matched_semiparametric_obj <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  semi_outcome <- estimate_semipmetric_erf(formula = Y ~ w,
                                           family = binomial,
                                           data = pseudo,
                                           ci_appr = "matching")
  return(semi_outcome)
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
  w_lims <- range(pseudo$w)
  erf_obj <- estimate_npmetric_erf(as.double(pseudo$Y),
                                   as.double(pseudo$w),
                                   bw_seq=seq(0.2,5,0.2), # bw_seq=seq(0.1, 10, length.out = 15),
                                   w_vals = seq(0, w_lims[2], length.out = 30),
                                   nthread = 1)
  
  return(plot(erf_obj))
}

get_gps_matched_nonparametric_results_with_counter <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  w_lims <- range(pseudo$w)
  erf_obj <- estimate_npmetric_erf(as.double(pseudo$Y),
                                   as.double(pseudo$w),
                                   pseudo$counter,
                                   bw_seq=seq(0.2,5,0.2), # bw_seq=seq(0.1, 10, length.out = 15),
                                   w_vals = seq(0, w_lims[2], length.out = 30),
                                   nthread = 1)
  
  return(plot(erf_obj))
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
