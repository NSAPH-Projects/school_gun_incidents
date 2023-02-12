## Load packages ----
library(MASS)
library(purrr)
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
beta0_estimated <- state.5.95_match.cap99.logistic$coefficients[1,1] #-4.1514
beta1_estimated <- state.5.95_match.cap99.logistic$coefficients[2,1] #-0.0296
vcov_estimated <- vcov(state.5.95_match.cap99.logistic) # matrix(c(0.0001321, -0.00001798, -0.00001798, 0.000003337), nrow = 2)


#### Calculate number of events avoided and fewer people affected #######

# Note: An "eligible" census tract is a census tract that had a SGI and an exposure under the threshold
# Note: Threshold and exposure are in half-miles! Therefore divide by 2 to report number of miles

## Print point estimates

cat("POINT ESTIMATES, NOT ROUNDED")
for (threshold in 1:6){
  cat("Number of eligible events avoided if threshold is", threshold / 2, "miles:", get_number_of_events_avoided(threshold, beta0_estimated, beta1_estimated), "\n")
  cat("Percent of eligible events avoided if threshold is", threshold / 2, "miles:", 100 * get_proportion_of_events_avoided(threshold, beta0_estimated, beta1_estimated), "%\n")
  cat("Fewer people affected if threshold is", threshold / 2, "miles:", get_fewer_people_affected(threshold, beta0_estimated, beta1_estimated), "\n")
  cat("Percent of fewer people affected avoided if threshold is", threshold / 2, "miles:", 100 * get_proportion_of_fewer_people_affected(threshold, beta0_estimated, beta1_estimated), "%\n")
  cat("\n")
}

cat("POINT ESTIMATES, ROUNDED UP TO NEAREST INTEGER")
for (threshold in 1:6){
  cat("Number of eligible events avoided if threshold is", threshold / 2, "miles:", ceiling(get_number_of_events_avoided(threshold, beta0_estimated, beta1_estimated)), "\n")
  cat("Percent of eligible events avoided if threshold is", threshold / 2, "miles:", round(100 * get_proportion_of_events_avoided(threshold, beta0_estimated, beta1_estimated), 2), "%\n")
  cat("Fewer people affected if threshold is", threshold / 2, "miles:", ceiling(get_fewer_people_affected(threshold, beta0_estimated, beta1_estimated)), "\n")
  cat("Percent of fewer people affected avoided if threshold is", threshold / 2, "miles:", round(100 * get_proportion_of_fewer_people_affected(threshold, beta0_estimated, beta1_estimated), 2), "%\n")
  cat("\n")
}


## Print 95% credible intervals

set.seed(100)
n_sims <- 10^4

# simulate
betas_sims <- mvrnorm(n_sims, c(beta0_estimated, beta1_estimated), vcov_estimated)
betas_sims_as_list <- as.list(as.data.frame(t(betas_sims)))
CIs <- data.frame(threshold = 1:6,
                  events_avoided_CI_lower = NA,
                  events_avoided_CI_upper = NA,
                  fewer_people_CI_lower = NA,
                  fewer_people_CI_upper = NA)

for (threshold in 1:6){
  events_avoided_sims <- unlist(map2(betas_sims[, 1], betas_sims[, 2], get_number_of_events_avoided, threshold = threshold))
  CIs$events_avoided_CI_lower[threshold] <- round(quantile(events_avoided_sims, probs = 0.025, na.rm=T), 2)
  CIs$events_avoided_CI_upper[threshold] <- round(quantile(events_avoided_sims, probs = 0.975, na.rm=T), 2)
  
  fewer_people_affected_sims <- unlist(map2(betas_sims[, 1], betas_sims[, 2], get_fewer_people_affected, threshold = threshold))
  CIs$fewer_people_CI_lower[threshold] <- round(quantile(fewer_people_affected_sims, probs = 0.025, na.rm=T), 2)
  CIs$fewer_people_CI_upper[threshold] <- round(quantile(fewer_people_affected_sims, probs = 0.975, na.rm=T), 2)
}

cat("95% CREDIBLE INTERVALS")
for (threshold in 1:6){
  cat("Number of eligible events avoided if threshold is", threshold / 2, "miles (95% CI): [", CIs$events_avoided_CI_lower[threshold], ", ", CIs$events_avoided_CI_upper[threshold], "]\n")
  cat("Fewer people affected if threshold is", threshold / 2, "miles (95% CI): [", CIs$fewer_people_CI_lower[threshold], ", ", CIs$fewer_people_CI_upper[threshold], "]\n")
  cat("\n")
}

cat("95% CREDIBLE INTERVALS, BOUNDS ROUNDED CONSERVATIVELY")
for (threshold in 1:6){
  cat("Number of eligible events avoided if threshold is", threshold / 2, "miles (95% CI): [", floor(CIs$events_avoided_CI_lower[threshold]), ", ", ceiling(CIs$events_avoided_CI_upper[threshold]), "]\n")
  cat("Fewer people affected if threshold is", threshold / 2, "miles (95% CI): [", floor(CIs$fewer_people_CI_lower[threshold]), ", ", ceiling(CIs$fewer_people_CI_upper[threshold]), "]\n")
  cat("\n")
}


