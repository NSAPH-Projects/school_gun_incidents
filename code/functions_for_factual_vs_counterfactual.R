#### Functions used by factual_vs_counterfactual_causal_continuous_exposure.R ####

get_gps_matched_logistic_results_glm <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- glm(formula = Y ~ w,
                 family = "binomial",
                 data = pseudo,
                 weights = counter_weight)
  return(summary(outcome))
}

get_number_of_tracts_in_bin <- function(factual_exposure_upper_bound, factual_exposures){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  return(sum(census_tracts_within_range))
}

## Functions for calculations

get_factual_logit_p_hat <- function(beta0, beta1, factual_exposure_upper_bound){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  exposures_within_range <- factual_exposures[(factual_exposures >= factual_exposure_lower_bound) &
                                                (factual_exposures < factual_exposure_upper_bound)]
  return(beta0 + beta1 * exposures_within_range)
}

get_counterfactual_logit_p_hat <- function(beta0, beta1, factual_exposure_upper_bound, counterfactual_exposure){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  num_exposures_within_range <- sum((factual_exposures >= factual_exposure_lower_bound) &
                                      (factual_exposures < factual_exposure_upper_bound))
  return(beta0 + beta1 * rep(counterfactual_exposure, num_exposures_within_range))
}

inv.logit <- function(x){
  return(exp(x) / (1 + exp(x)))
}

## Functions to calculate number of events avoided

get_expected_events <- function(logit_p_hats){
  p_hats <- inv.logit(logit_p_hats)
  return(sum(p_hats))
}

# get_true_factual_events <- function(factual_exposure_upper_bound){
#   factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
#   census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
#     (factual_exposures < factual_exposure_upper_bound)
#   return(sum(data_with_state$y[census_tracts_within_range]))
# }


## Functions to calculated number of people affected

get_expected_ppl_affected <- function(factual_exposure_upper_bound, logit_p_hats, data){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  p_hats <- inv.logit(logit_p_hats)
  ppl <- data$total_population_2020[census_tracts_within_range]
  return(sum(p_hats * ppl))
}

# get_true_ppl_affected <- function(factual_exposure_upper_bound, data){
#   factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
#   census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
#     (factual_exposures < factual_exposure_upper_bound)
#   return(sum(data$y[census_tracts_within_range] * data$total_population_2020[census_tracts_within_range]))
# }
