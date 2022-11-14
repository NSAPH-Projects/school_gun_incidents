# This file contains functions (and a couple constants) used in our causal analysis

##### Functions to set up analysis #####

quantitative_confounders <- c("total_population_2020", "housing_units_per_100_sq_miles", "area_sq_miles",
                          "log_median_hh_income", "schools_per_100_sq_miles",
                          "log_median_hh_income_15to24", "total_crime_2021", 
                          "dealers_per_100_sq_miles", "mental_health_index",
                          "daytime_pop_2021", "prop_white_only", "prop_black_only", 
                          "prop_asian_only", "prop_multiracial", "prop_hispanic_latino", 
                          "prop_food_stamps_2019", "prop_public_assist_income_2019",
                          "prop_below_poverty_2019", "prop_without_vehicles_2019",
                          "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021", 
                          "prop_grad_deg_25plus_2021", "prop_unemployed_2021",
                          "prop_unemployed_16to24_2021", "prop_institutional_group",
                          "prop_noninstitutional_group", "prop_18plus")


load_packages <- function(){
  library(CausalGPS)
  library(MASS)
  library(ggplot2)
  library(tidyr)
  # library(pscl) # for zeroinfl
  library(dplyr)
  library(SuperLearner)
  library(data.table)
  # library(lubridate)
  # library(stringr)
  # library(WeightIt)
  # library(cobalt) # for loveplot
  library(jtools)
  # library(MatchIt)
  library(lmtest)
  # library(sandwich)
  library(xtable)
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
  if ("State_Name" %in% colnames(data)){
    data$State_Name <- as.factor(data$State_Name)
  }
  if ("County_Name" %in% colnames(data)){
    data$County_Name <- as.factor(data$County_Name)
  }
  if ("urban_rural" %in% colnames(data)){
    data$urban_rural <- as.factor(data$urban_rural)
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
# get_confounders_matrix <- function(data, confounder_names){
#   # x <- factorize_cat_vars(data[, confounder_names])
#   x <- data[, confounder_names]
#   x <- t(apply(x, 1, unlist)) # turn data frame into matrix, for CausalGPS functions
#   return(x)
# }

get_analysis_df <- function(data, treatment = "mean_total_miles", confounder_names){
  data <- as.data.frame(data)
  y <- data[, "binary_shooting_incident"]
  a <- data[, treatment]
  x <- factorize_cat_vars(data[, confounder_names])
  return(cbind(y, a, x))
}

##### Functions to handle parametric results #####

round_results <- function(x){
  rounded <- format(round(as.numeric(x), 4), scientific = F)
  if (rounded == 0) rounded <- format(round(as.numeric(x), 5), scientific = F)
  return(rounded)
}

codify_significance <- function(significance){
  significance <- as.numeric(significance)
  if (significance < 0.01) return("***")
  else if (significance < 0.05) return("**")
  else if (significance < 0.1) return("*")
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

inv.logit <- function(x){
  return(exp(x)/(1+exp(x)))
}

##### Functions to perform causal analyses for continuous exposure (GPS) #####

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

get_gps_weighted_pseudo_pop <- function(outcome, exposure, confounders, trim_quantiles = c(0.05, 0.95)){
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
                             trim_quantiles = trim_quantiles, 
                             optimized_compile = TRUE, 
                             max_attempt = 5,
                             matching_fun = "matching_l1",
                             delta_n = 0.2,
                             scale = 1.0))
}

get_gps <- function(outcome, exposure, confounders){
  return(estimate_gps(Y = outcome,
                      w = exposure,
                      c = as.data.frame(confounders),
                      pred_model = "sl",
                      gps_model = "parametric",
                      sl_lib = c("m_xgboost"),
                      params = list(xgb_nrounds = c(50)),
                      nthread = 4,
                      internal_use = FALSE))
}

# Generate point-biserial correlation between continuous exposure (w) and binary covariate (binary_cov)
# Note - equivalent to pearson correlation
pt_biserial_cor <- function(w, binary_cov){
  mean_gp1 <- mean(w[binary_cov == 1])
  mean_gp0 <- mean(w[binary_cov == 0])
  s_nminus1 <- sd(w)
  n1 <- sum(binary_cov == 1)
  n0 <- sum(binary_cov == 0)
  n <- length(binary_cov) # n = n0 + n1
  
  cor_pb <- (mean_gp1 - mean_gp0) / s_nminus1 * sqrt(n1 / n * n0 / (n-1))
  return(cor_pb)
}

abs_pt_biserial_cor <- function(w, binary_cov){
  return(abs(cor(w, binary_cov)))
}

# Calculate mean of absolute point-biserial correlation between continuous exposure and each binary indicator for an unordered categorical covariate 
# params: w is the vector of continuous exposure, unordered_var is vector of unordered categorical covariate 
cor_unordered_var <- function(w, unordered_var){
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  abs_cor_pb <- lapply(binary_indicators, abs_pt_biserial_cor, w = w)
  return(mean(unlist(abs_cor_pb)))
}

weighted_cor_unordered_var <- function(w, unordered_var, weights){
  library(wCorr)
  
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  weighted_cor <- lapply(binary_indicators, weightedCorr, y = w, method = "Pearson", weights = weights)
  abs_weighted_cor <- lapply(weighted_cor, abs)
  return(mean(unlist(abs_weighted_cor)))
}

get_matched_correlation_plot <- function(matched_pop, cat_confounder_names, w_orig, unordered_vars_orig, quant_confounders = quantitative_confounders){
  confounder_names <- c(cat_confounder_names, quant_confounders)
  cor_unmatched <- matched_pop$original_corr_results$absolute_corr[confounder_names]
  cor_matched <- matched_pop$adjusted_corr_results$absolute_corr[confounder_names]
  weights <- matched_pop$pseudo_pop$counter_weight
  
  # correct abs corr values for unordered categorical variables
  for (unordered_var in cat_confounder_names){
    cor_matched[unordered_var] <- weighted_cor_unordered_var(matched_pop$pseudo_pop$w, matched_pop$pseudo_pop[[unordered_var]], weights)
    cor_unmatched[unordered_var] <- cor_unordered_var(w_orig, unordered_vars_orig[[unordered_var]])
  }
  cor_matched <- cor_matched[!is.na(cor_matched)]
  
  abs_cor <- data.frame(Covariate = confounder_names,
                        Unmatched = cor_unmatched,
                        Matched = cor_matched)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unmatched)
  abs_cor <- abs_cor %>% gather(c(Unmatched, Matched), key = 'Dataset', value = 'Absolute Correlation')
  return(abs_cor)
}

get_weighted_correlation_plot <- function(weighted_pop, cat_confounder_names, w_orig, unordered_vars_orig, quant_confounders = quantitative_confounders){
  confounder_names <- c(cat_confounder_names, quant_confounders)
  cor_unweighted <- weighted_pop$original_corr_results$absolute_corr[confounder_names]
  cor_weighted <- weighted_pop$adjusted_corr_results$absolute_corr[confounder_names]
  weights <- weighted_pop$pseudo_pop$counter_weight
  
  # correct abs corr values for unordered categorical variables
  for (unordered_var in cat_confounder_names){
    cor_weighted[unordered_var] <- weighted_cor_unordered_var(weighted_pop$pseudo_pop$w, weighted_pop$pseudo_pop[[unordered_var]], weights)
    cor_unweighted[unordered_var] <- cor_unordered_var(w_orig, unordered_vars_orig[[unordered_var]])
  }
  cor_weighted <- cor_weighted[!is.na(cor_weighted)]
  
  abs_cor <- data.frame(Covariate = confounder_names,
                        Unweighted = cor_unweighted,
                        Weighted = cor_weighted)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unweighted)
  abs_cor <- abs_cor %>% gather(c(Unweighted, Weighted), key = 'Dataset', value = 'Absolute Correlation')
  return(abs_cor)
}

make_correlation_plot <- function(pseudo_pop, ci, cat_confounder_names, w_orig, unordered_vars_orig){
  if (ci == "matching"){
    abs_cor <- get_matched_correlation_plot(pseudo_pop, cat_confounder_names, w_orig, unordered_vars_orig)
  } else if (ci == "weighting"){
    abs_cor <- get_weighted_correlation_plot(pseudo_pop, cat_confounder_names, w_orig, unordered_vars_orig)
  }
  p <- ggplot(abs_cor, aes(x = `Absolute Correlation`, y = Covariate, color = Dataset, group = Dataset)) +
    geom_point() +
    geom_line(orientation = "y")
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

get_gps_matched_logistic_results_glm <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- glm(formula = Y ~ w,
                 family = "binomial",
                 data = pseudo,
                 weights = counter_weight)
  return(summary(outcome))
}

get_gps_weighted_logistic_results <- function(weighted_pop){
  pseudo <- weighted_pop$pseudo_pop
  outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                  family = binomial,
                                  data = pseudo,
                                  ci_appr = "weighting")
  return(summary(outcome))
}

get_gps_adjusted_logistic_results <- function(data_with_gps){
  logistic_model <- glm(y ~ ., 
                        data = data_with_gps, 
                        family = "binomial")
  return(summary(logistic_model))
}


get_gps_weighting_results <- function(gps_pop){
  pseudo <- gps_pop$pseudo_pop
  weighting_outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                            family = binomial,
                                            data = pseudo,
                                            ci_appr = "weighting")
  return(summary(weighting_outcome))
}


##### THE FOLLOWING FUNCTIONS ARE NOT USED ANYMORE (BINARY EXPOSURE) #####

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


##### THE FOLLOWING FUNCTIONS ARE NOT USED ANYMORE (4-LEVEL EXPOSURE) #####

match_discretized <- function(data, control_name = "1", confounder_names,
                              exact_matches = c("census_division_number", "dealers_per_100_sq_miles_decile", "log_median_hh_income_decile"),
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


##### THE FOLLOWING FUNCTIONS ARE NOT USED ANYMORE (CONTINUOUS EXPOSURE) #####

# exposure_vec: enter exposure as a vector of values, not as name of exposure variable
trim_exposure <- function(df, exposure_vec, trim_quantile = 0.95){
  return(df[exposure_vec <= quantile(exposure_vec, trim_quantile), ])
}

# Function to winsorize counter (i.e., # of times each included observation is GPS-matched) at high quantile
# Parameter quantile should be between 0.5 and 1
winsorize_counter_onesided <- function(counter, quantile){
  cutoff <- quantile(counter, quantile)
  return(ifelse(counter > cutoff, cutoff, counter))
}

log_nonneg <- function(x){
  if (min(x) >= 0) return(log(x + 0.001)) else return(x)
}

logit_nonneg <- function(x){
  if (min(x) >= 0 & max(x) <= 1){
    return(log((x + 0.001)/(1 - x + 0.001)))
  } else{
    return(x)
  }
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

get_gps_matched_nonparametric_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  w_lims <- range(pseudo$w)
  erf_obj <- estimate_npmetric_erf(as.double(pseudo$Y),
                                   as.double(pseudo$w),
                                   as.double(pseudo$counter_weight),
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
