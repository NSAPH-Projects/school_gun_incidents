## Load packages ----

library(SuperLearner)
library(CausalGPS)
library(lmtest) # for coeftest, getting clustered robust standard errors (CRSE)
library(sandwich) # for vcovCL, getting clustered robust standard errors (CRSE)
library(gee)

####
# Functions to perform causal analyses for continuous exposure (GPS)
####

#### GPS matching ####

all_matching_results_1model <- function(seed,
                                        data,
                                        trim,
                                        cat_covariate_names,
                                        run_gee_model = F,
                                        quant_covariates = quantitative_covariates){
  set.seed(seed)
  results_list <- list()
  
  # GPS matching
  matched_pop <- get_gps_matched_pseudo_pop(data$y,
                                            data$a,
                                            data[, c(cat_covariate_names, quant_covariates)],
                                            trim)
  # Store how many observations were matched
  results_list[["num_obs_matched"]] <- sum(matched_pop$pseudo_pop$counter_weight > 0)
  
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(matched_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(matched_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th percentile)")
  
  # Create alternate pseudopopulation where counter is capped
  matched_pop_capped99 <- copy(matched_pop)
  matched_pop_capped99$pseudo_pop$counter_weight[which(matched_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Fit logistic regression outcome model on capped matched population, since covariate balance is good (AC < 0.1)
  # (do not proceed to logistic regression on uncapped matched population since covariate balance is poor)
  matching_results <- get_gps_matched_logistic_results(matched_pop_capped99)$coefficients["w", ]
  
  # Store point estimate for odds (exponentiated log odds)
  results_list[["logistic_regression_estimated_odds"]] <- round(exp(matching_results["Estimate"]), 4)
  
  # Before getting clustered robust standard errors (CRSE) and GEE model with clusters,
  # convert data to long format (to make clustering explicit)
  pseudopop_long <- matched_pop_capped99$pseudo_pop[, lapply(.SD, function(x) rep(x, counter_weight)), by = row_index]
  
  # Store clustered robust standard errors (CRSE) for logistic model
  logit_model <- glm(Y ~ w,
                     family = binomial(link = "logit"),
                     data = pseudopop_long[, c("Y", "w", "row_index")])
  cl_sd_results <- coeftest(logit_model,
                            vcov. = vcovCL(logit_model,
                                           cluster = pseudopop_long$row_index,
                                           type = "HC0"))
  results_list[["cl_sd_lb_95ci"]] <- round(exp(cl_sd_results[2,1] - 1.96 * cl_sd_results[2,2]), 4)
  results_list[["cl_sd_ub_95ci"]] <- round(exp(cl_sd_results[2,1] + 1.96 * cl_sd_results[2,2]), 4)
  results_list[["cl_sd_lb_90ci"]] <- round(exp(cl_sd_results[2,1] - 1.645 * cl_sd_results[2,2]), 4)
  results_list[["cl_sd_ub_90ci"]] <- round(exp(cl_sd_results[2,1] + 1.645 * cl_sd_results[2,2]), 4)
  
  if (run_gee_model){
    
    # Fit GEE model
    outcome <- gee(formula = Y ~ w,
                   family = "binomial",
                   data = pseudopop_long[, c("Y", "w", "row_index")], 
                   id = pseudopop_long$row_index,
                   corstr = "exchangeable") # allows same correlation coefficient for the same row index
    
    # Store GEE model results
    results_list[["GEE_estimated_odds"]] <- round(exp(summary(outcome)$coefficients["w",]["Estimate"]), 4)
    results_list[["GEE_lb_95ci"]] <- round(exp(summary(outcome)$coefficients["w",]["Estimate"] - 1.96 * summary(outcome)$coefficients["w",]["Robust S.E."]), 4)
    results_list[["GEE_ub_95ci"]] <- round(exp(summary(outcome)$coefficients["w",]["Estimate"] + 1.96 * summary(outcome)$coefficients["w",]["Robust S.E."]), 4)
    results_list[["GEE_lb_90ci"]] <- round(exp(summary(outcome)$coefficients["w",]["Estimate"] - 1.645 * summary(outcome)$coefficients["w",]["Robust S.E."]), 4)
    results_list[["GEE_ub_90ci"]] <- round(exp(summary(outcome)$coefficients["w",]["Estimate"] + 1.645 * summary(outcome)$coefficients["w",]["Robust S.E."]), 4)
  }
  
  # Save covariate balance plots
  for (capped in c(1, .99)){ # 1 is uncapped counter_weight, .99 is counter_weight capped at 99th percentile
    if (capped == 1){
      pseudo_pop <- matched_pop
    } else if (capped == .99){
      pseudo_pop <- matched_pop_capped99
    }
    
    # for capped pseudopopulations, recalculate covariate balance
    if (capped < 1){
      adjusted_corr_obj <- check_covar_balance(w = as.data.table(pseudo_pop$pseudo_pop$w),
                                               c = subset(pseudo_pop$pseudo_pop, select = quant_covariates),
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
    results_list[[paste0("cov_bal.capped", capped)]] <- 
      get_matched_correlations(pseudo_pop, 
                                   cat_covariate_names, 
                                   data$a, 
                                   subset(data, select = cat_covariate_names), 
                                   quant_covariates)
  }
  
  return(results_list) # numerical output, to be stored in results table
}

get_gps_matched_pseudo_pop <- function(outcome, exposure, covariates, trim_quantiles = c(0.05, 0.95), caliper = 0.2){
  return(generate_pseudo_pop(Y = outcome,
                             w = exposure,
                             c = as.data.frame(covariates),
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
                             delta_n = caliper,
                             scale = 1.0))
}

get_matched_correlations <- function(matched_pop, cat_covariate_names, w_orig, unordered_vars_orig, quant_covariates = quantitative_covariates){
  covariate_names <- c(cat_covariate_names, quant_covariates)
  cor_unmatched <- matched_pop$original_corr_results$absolute_corr[covariate_names]
  cor_matched <- matched_pop$adjusted_corr_results$absolute_corr[covariate_names]
  weights <- matched_pop$pseudo_pop$counter_weight
  
  # correct abs corr values for unordered categorical variables
  for (unordered_var in cat_covariate_names){
    cor_matched[unordered_var] <- weighted_cor_unordered_var(matched_pop$pseudo_pop$w, matched_pop$pseudo_pop[[unordered_var]], weights)
    cor_unmatched[unordered_var] <- cor_unordered_var(w_orig, unordered_vars_orig[[unordered_var]])
  }
  cor_matched <- cor_matched[!is.na(cor_matched)]
  
  abs_cor <- data.frame(Covariate = covariate_names,
                        Unadjusted = cor_unmatched,
                        Matched = cor_matched)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unadjusted)
  abs_cor <- abs_cor %>% gather(c(Unadjusted, Matched), key = 'Dataset', value = 'Absolute Correlation')
  abs_cor <- as.data.table(abs_cor)
  abs_cor[, Dataset := factor(Dataset, levels = c("Unadjusted", "Matched"))]
  return(abs_cor)
}

get_gps_matched_logistic_results <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                  family = binomial,
                                  data = pseudo,
                                  ci_appr = "matching")
  return(summary(outcome))
}

#### GPS weighting ####

all_weighting_results_1model <- function(seed,
                                         data,
                                         trim,
                                         cat_covariate_names,
                                         quant_covariates = quantitative_covariates){
  set.seed(seed)
  results_list <- list()
  
  # GPS weighting
  weighted_pop <- get_gps_weighted_pseudo_pop(data$y,
                                              data$a,
                                              data[, c(cat_covariate_names, quant_covariates)],
                                              trim)
  # Store key counter quantiles
  results_list[["weight_max"]] <- round(max(weighted_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(weighted_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["weight99"]] <- paste(cap99, "(99th percentile)")
  
  # Create alternate pseudopopulation where counter is capped
  weighted_pop_capped99 <- copy(weighted_pop)
  weighted_pop_capped99$pseudo_pop$counter_weight[which(weighted_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Fit logistic regression outcome model on capped weighted population, since covariate balance is good (AC < 0.1)
  # (do not proceed to logistic regression on uncapped weighted population since covariate balance is poor)
  weighting_results <- get_gps_matched_logistic_results(weighted_pop_capped99)$coefficients["w", ]
  
  # Save point estimate, 95%, and 90% confidence intervals for odds (exponentiated log odds)
  results_list[["logistic_regression_estimated_odds"]] <- round(exp(weighting_results["Estimate"]), 4)
  results_list[["lb_95ci"]] <- round(exp( weighting_results["Estimate"] - 1.96 * weighting_results["Std. Error"]), 4)
  results_list[["ub_95ci"]] <- round(exp( weighting_results["Estimate"] + 1.96 * weighting_results["Std. Error"]), 4)
  results_list[["lb_90ci"]] <- round(exp( weighting_results["Estimate"] - 1.645 * weighting_results["Std. Error"]), 4)
  results_list[["ub_90ci"]] <- round(exp( weighting_results["Estimate"] + 1.645 * weighting_results["Std. Error"]), 4)
  
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
                                               c = subset(pseudo_pop$pseudo_pop, select = quant_covariates),
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
    results_list[[paste0("cov_bal.capped", capped)]] <- get_weighted_correlations(pseudo_pop,
                                                                                  cat_covariate_names,
                                                                                  data$a,
                                                                                  subset(data, select = cat_covariate_names),
                                                                                  quant_covariates)
  }
  
  return(results_list) # numerical output, to be stored in results table
}

get_gps_weighted_pseudo_pop <- function(outcome, exposure, covariates, trim_quantiles = c(0.05, 0.95)){
  return(generate_pseudo_pop(Y = outcome,
                             w = exposure,
                             c = as.data.frame(covariates),
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

get_weighted_correlations <- function(weighted_pop, cat_covariate_names, w_orig, unordered_vars_orig, quant_covariates = quantitative_covariates){
  covariate_names <- c(cat_covariate_names, quant_covariates)
  cor_unweighted <- weighted_pop$original_corr_results$absolute_corr[covariate_names]
  cor_weighted <- weighted_pop$adjusted_corr_results$absolute_corr[covariate_names]
  weights <- weighted_pop$pseudo_pop$counter_weight
  
  # correct abs corr values for unordered categorical variables
  for (unordered_var in cat_covariate_names){
    cor_weighted[unordered_var] <- weighted_cor_unordered_var(weighted_pop$pseudo_pop$w, weighted_pop$pseudo_pop[[unordered_var]], weights)
    cor_unweighted[unordered_var] <- cor_unordered_var(w_orig, unordered_vars_orig[[unordered_var]])
  }
  cor_weighted <- cor_weighted[!is.na(cor_weighted)]
  
  abs_cor <- data.frame(Covariate = covariate_names,
                        Unadjusted = cor_unweighted,
                        Weighted = cor_weighted)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unadjusted)
  abs_cor <- abs_cor %>% gather(c(Unadjusted, Weighted), key = 'Dataset', value = 'Absolute Correlation')
  abs_cor <- as.data.table(abs_cor)
  abs_cor[, Dataset := factor(Dataset, levels = c("Unadjusted", "Weighted"))]
  return(abs_cor)
}

get_gps_weighted_logistic_results <- function(weighted_pop){
  pseudo <- weighted_pop$pseudo_pop
  outcome <- estimate_pmetric_erf(formula = Y ~ w,
                                  family = binomial,
                                  data = pseudo,
                                  ci_appr = "weighting")
  return(summary(outcome))
}
