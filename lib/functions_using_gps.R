## Load packages ----

library(SuperLearner)
library(CausalGPS)

####
# Functions to perform causal analyses for continuous exposure (GPS)
####

#### gps matched ####

all_matching_results_1model <- function(seed, data, trim,
                                        cat_covariate_names, quant_covariates = quantitative_covariates){
  set.seed(seed)
  results_list <- list()
  
  # GPS matching
  matched_pop <- get_gps_matched_pseudo_pop(data$y,
                                            data$a,
                                            data[, c(cat_covariate_names, quant_covariates)],
                                            trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(matched_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(matched_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th percentile)")
  
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
      get_matched_correlation_plot(pseudo_pop, 
                                   cat_covariate_names, 
                                   data$a, 
                                   subset(data, select = cat_covariate_names), 
                                   quant_covariates)
    # cov_bal <- make_correlation_plot(pseudo_pop, "matching", cat_covariate_names, data$a, subset(data, select = cat_covariate_names))
  }
  
  return(results_list) # numerical output, to be stored in results table
}

get_gps_matched_pseudo_pop <- function(outcome, exposure, covariates, trim_quantiles = c(0.05, 0.95)){
  # if ("census_division" %in% colnames(covariates)){
  #   covariates$census_division <- NULL # to do: figure out if this function can use categorical variables
  # }
  
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
                             delta_n = 0.2,
                             scale = 1.0))
}

get_matched_correlation_plot <- function(matched_pop, cat_covariate_names, w_orig, unordered_vars_orig, quant_covariates = quantitative_covariates){
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
                        Unmatched = cor_unmatched,
                        Matched = cor_matched)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unmatched)
  abs_cor <- abs_cor %>% gather(c(Unmatched, Matched), key = 'Dataset', value = 'Absolute Correlation')
  abs_cor <- as.data.table(abs_cor)
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

#### gps weighted ####

all_weighting_results_1model <- function(seed, data, trim,
                                         cat_covariate_names, quant_covariates = quantitative_covariates){
  set.seed(seed)
  results_list <- list()
  
  # GPS weighting
  weighted_pop <- get_gps_weighted_pseudo_pop(data$y,
                                              data$a,
                                              data[, c(cat_covariate_names, quant_covariates)],
                                              trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(weighted_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(weighted_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th percentile)")
  
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
    results_list[[paste0("cov_bal.capped", capped)]] <- get_weighted_correlation_plot(pseudo_pop, cat_covariate_names, data$a, subset(data, select = cat_covariate_names), quant_covariates)
    # cov_bal <- make_correlation_plot(pseudo_pop, "weighting", cat_covariate_names, data$a, subset(data, select = cat_covariate_names))
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

get_weighted_correlation_plot <- function(weighted_pop, cat_covariate_names, w_orig, unordered_vars_orig, quant_covariates = quantitative_covariates){
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
                        Unweighted = cor_unweighted,
                        Weighted = cor_weighted)
  abs_cor$Covariate <- reorder(abs_cor$Covariate, abs_cor$Unweighted)
  abs_cor <- abs_cor %>% gather(c(Unweighted, Weighted), key = 'Dataset', value = 'Absolute Correlation')
  abs_cor <- as.data.table(abs_cor)
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
