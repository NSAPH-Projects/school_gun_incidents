## Load packages ----
library(MASS)
library(data.table)
library(ggplot2)
library(tidyr)

## Load functions ----
dir <- "../" # run code in the script location


source(paste0(dir, "lib/helper_functions.R"))
source(paste0(dir, "lib/functions_for_factual_vs_counterfactual.R"))
source(paste0(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## Main body ----

# prepare dataset for main analysis
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates))
factual_exposures <- data_with_state$a


#### Get GPS matching results (trim 5/95, cap 99) #######

set.seed(100)
state.5.95_match <- get_gps_matched_pseudo_pop(
  data_with_state$y, 
  data_with_state$a, 
  data_with_state[, c("State_Name", quantitative_covariates)], 
  trim_quantiles = c(0.05, 0.95)
  )
state.5.95_cap99 <- round(quantile(state.5.95_match$pseudo_pop$counter_weight, 0.99), 2)
state.5.95_match.cap99 <- copy(state.5.95_match)
state.5.95_match.cap99$pseudo_pop$counter_weight[which(state.5.95_match.cap99$pseudo_pop$counter_weight >= state.5.95_cap99)] <- state.5.95_cap99

## matched logistic
state.5.95_match.cap99.logistic <- get_gps_matched_logistic_results_glm(state.5.95_match.cap99)
beta0_state <- state.5.95_match.cap99.logistic$coefficients[1,1] #-4.1514
beta1_state <- state.5.95_match.cap99.logistic$coefficients[2,1] #-0.0296
vcov_state <- vcov(state.5.95_match.cap99.logistic) # matrix(c(0.0001321, -0.00001798, -0.00001798, 0.000003337), nrow = 2)

#### Number of events avoided #######

# total number of census tracts with SGI whose exposure is under the threshold ("eligible census tracts")
get_total_factual_events_for_threshold <- function(threshold){
  eligible_census_tracts <- factual_exposures < threshold
  return(sum(data_with_state$y[eligible_census_tracts]))
}

# vector of hazard ratios, for all census tracts
get_all_census_tracts_hazard_ratios_for_threshold <- function(threshold){
  probability_for_threshold <- inv.logit(beta0_state + beta1_state * pmax(factual_exposures, threshold))
  probability_for_factual <- inv.logit(beta0_state + beta1_state * factual_exposures)
  return(probability_for_threshold / probability_for_factual)
}

# vector of expected counterfactual events, for eligible census tracts
get_all_counterfactual_events_for_threshold <- function(threshold){
  eligible_census_tracts <- factual_exposures < threshold
  return(data_with_state$y * eligible_census_tracts * get_all_census_tracts_hazard_ratios_for_threshold(threshold))
}

# expected number of events avoided, in eligible census tracts
get_total_events_avoided_for_threshold <- function(threshold){
  total_factual_events <- get_total_factual_events_for_threshold(threshold)
  total_counterfactual_events <- sum(get_all_counterfactual_events_for_threshold(threshold))
  return(total_factual_events - total_counterfactual_events)
}


#### Fewer people affected #######

# total number of people whose census tracts had a SGI and an exposure under the threshold ("eligible people")
get_total_factual_people_for_threshold <- function(threshold){
  eligible_census_tracts <- factual_exposures < threshold
  return(sum(data_with_state$y[eligible_census_tracts] * data_with_state$total_population_2020[eligible_census_tracts]))
}

# vector of expected counterfactual people affected, for eligible census tracts
get_all_counterfactual_people_for_threshold <- function(threshold){
  eligible_census_tracts <- factual_exposures < threshold
  return(data_with_state$y * eligible_census_tracts * get_all_census_tracts_hazard_ratios_for_threshold(threshold) * data_with_state$total_population_2020)
}

# expected fewer people affected, in eligible census tracts
get_total_people_avoided_for_threshold <- function(threshold){
  total_factual_people <- get_total_factual_people_for_threshold(threshold)
  total_counterfactual_people <- sum(get_all_counterfactual_people_for_threshold(threshold))
  return(total_factual_people - total_counterfactual_people)
}


