### Functions to set up calculations ###

get_gps_matched_logistic_results_glm <- function(matched_pop){
  pseudo <- matched_pop$pseudo_pop
  outcome <- glm(formula = Y ~ w,
                 family = "binomial",
                 data = pseudo,
                 weights = counter_weight)
  return(summary(outcome))
}

inv.logit <- function(x){
  return(exp(x) / (1 + exp(x)))
}

### Functions to perform calculations ###

# an eligible census tract is a census tract that had a SGI and an exposure under the threshold
get_exposures_and_populations_of_eligible_tracts <- function(threshold){
  eligible_census_tracts <- (data_with_state$y == 1) & (factual_exposures < threshold)
  return(data_with_state[eligible_census_tracts, c("a", "total_population_2020")])
}

get_number_of_eligible_tracts <- function(threshold){
  return(nrow(get_exposures_and_populations_of_eligible_tracts(threshold)))
}

get_number_of_factual_events <- function(threshold){
  return(get_number_of_eligible_tracts(threshold))
}

# we define the number of people affected as the 2020 population of a census tract that experienced a SGI
get_number_of_factual_people_affected <- function(threshold){
  data <- get_exposures_and_populations_of_eligible_tracts(threshold)
  return(sum(data$total_population_2020))
}

# use our model to get a predicted probability of an event happening in an eligible census tract, if its exposure were the threshold
# for scaling/calibration purposes, divide by our model's predicted probability of an event happening, under the real exposure
get_scaled_probabilities_of_event <- function(threshold, beta0, beta1){
  data <- get_exposures_and_populations_of_eligible_tracts(threshold)
  probability_for_threshold <- inv.logit(beta0 + beta1 * threshold)
  probability_for_factual <- inv.logit(beta0 + beta1 * data$a)
  scaled_probability <- probability_for_threshold / probability_for_factual
  return(scaled_probability)
}

get_number_of_counterfactual_events <- function(threshold, beta0, beta1){
  return(sum(get_scaled_probabilities_of_event(threshold, beta0, beta1)))
}

get_number_of_counterfactual_people_affected <- function(threshold, beta0, beta1){
  data <- get_exposures_and_populations_of_eligible_tracts(threshold)
  scaled_probability <- get_scaled_probabilities_of_event(threshold, beta0, beta1)
  return(sum(data$total_population_2020 * scaled_probability))
}

get_number_of_events_avoided <- function(threshold, beta0, beta1){
  return(get_number_of_factual_events(threshold) - get_number_of_counterfactual_events(threshold, beta0, beta1))
}

get_fewer_people_affected <- function(threshold, beta0, beta1){
  return(get_number_of_factual_people_affected(threshold) - get_number_of_counterfactual_people_affected(threshold, beta0, beta1))
}

get_proportion_of_events_avoided <- function(threshold, beta0, beta1){
  return(get_number_of_events_avoided(threshold, beta0, beta1) / get_number_of_factual_events(threshold))
}

get_proportion_of_fewer_people_affected <- function(threshold, beta0, beta1){
  return(get_fewer_people_affected(threshold, beta0, beta1) / get_number_of_factual_people_affected(threshold))
}
