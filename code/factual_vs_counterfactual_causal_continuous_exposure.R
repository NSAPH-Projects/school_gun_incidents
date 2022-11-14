# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

##### Get data and functions #####

df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_with_state <- na.omit(data_with_state)
factual_exposures <- data_with_state$a

# the following dataset (sensitivity analysis) is not used because we only perform this counterfactual-vs-factual calculation on the main analysis
# data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders, "urban_rural"))
# data_with_urbanity_state <- na.omit(data_with_urbanity_state)


#### Get GPS matching results ####

### The code below performs matching; commented out to save time ###

# set.seed(100)
# state.5.95_match <- get_gps_matched_pseudo_pop(data_with_state$y, data_with_state$a, data_with_state[, c("State_Name", quantitative_confounders)], trim_quantiles = c(0.05, 0.95))
# state.5.95_cap99 <- round(quantile(state.5.95_match$pseudo_pop$counter_weight, 0.99), 2)
# state.5.95_match.cap99 <- copy(state.5.95_match)
# state.5.95_match.cap99$pseudo_pop$counter_weight[which(state.5.95_match.cap99$pseudo_pop$counter_weight >= state.5.95_cap99)] <- state.5.95_cap99
# state.5.95_match.cap99.logistic <- get_gps_matched_logistic_results_glm(state.5.95_match.cap99)
# state.5.95_match.cap99.logistic
# vcov(state.5.95_match.cap99.logistic)

# set.seed(100)
# state.urbanity.5.95_match <- get_gps_matched_pseudo_pop(data_with_urbanity_state$y, data_with_urbanity_state$a, data_with_urbanity_state[, c("State_Name", "urban_rural", quantitative_confounders)], trim_quantiles = c(0.05, 0.95))
# state.urbanity.5.95_cap99 <- round(quantile(state.urbanity.5.95_match$pseudo_pop$counter_weight, 0.99), 2)
# state.urbanity.5.95_match.cap99 <- copy(state.urbanity.5.95_match)
# state.urbanity.5.95_match.cap99$pseudo_pop$counter_weight[which(state.urbanity.5.95_match.cap99$pseudo_pop$counter_weight >= state.urbanity.5.95_cap99)] <- state.urbanity.5.95_cap99
# state.urbanity.5.95_match.cap99.logistic <- get_gps_matched_logistic_results_glm(state.urbanity.5.95_match.cap99)
# state.urbanity.5.95_match.cap99.logistic
# vcov(state.urbanity.5.95_match.cap99.logistic)

### To save time, hard-code results from GPS matching (trim 5/95, cap 99) ###
beta0_state <- -4.1528
beta1_state <- -0.0520
beta0_state.urbanity <- -4.0811
beta1_state.urbanity <- -0.0502
vcov_state <- matrix(c(0.0002590, -0.00006991, -0.00006991, 0.00002570), nrow = 2)
vcov_state.urbanity <- matrix(c(0.0002415, -0.00006514, -0.00006514, 0.00002387), nrow = 2)


#### Calculate number of events avoided ####

# parameters for counterfactual calculation
all.factual.exposure.lower.bounds <- 0:8
all.factual.exposure.upper.bounds <- all.factual.exposure.lower.bounds + 1
all.counterfactual.exposures <- all.factual.exposure.upper.bounds + 1

# get n for each exposure bin
get_number_of_tracts_in_bin <- function(factual_exposure_upper_bound){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  return(sum(census_tracts_within_range))
}
n.tracts.in.bins <- sapply(all.counterfactual.exposures, get_number_of_tracts_in_bin)

# vector and dataframes to store results
state_factual.expected.events <- rep(NA, length(all.factual.exposure.upper.bounds))
state.urbanity_factual.expected.events <- rep(NA, length(all.factual.exposure.upper.bounds))
true.factual.events <- rep(NA, length(all.factual.exposure.upper.bounds))

state_counterfactual.expected.events <- matrix(nrow = length(all.factual.exposure.upper.bounds),
                                               ncol = length(all.counterfactual.exposures)) # rows: factual 1-mile bins of exposure. columns: counterfactual exposures
rownames(state_counterfactual.expected.events) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_counterfactual.expected.events) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
# state_counterfactual.expected.events[, 1] <- n.tracts.in.bins
state.urbanity_counterfactual.expected.events <- copy(state_counterfactual.expected.events)

state_expected.events.avoided <- copy(state_counterfactual.expected.events)
state.urbanity_expected.events.avoided <- copy(state_counterfactual.expected.events)
state_expected.events.avoided2 <- copy(state_counterfactual.expected.events)
state.urbanity_expected.events.avoided2 <- copy(state_counterfactual.expected.events)

# functions for calculations
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

get_expected_events <- function(logit_p_hats){
  p_hats <- inv.logit(logit_p_hats)
  return(sum(p_hats))
}

get_true_factual_events <- function(factual_exposure_upper_bound){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  return(sum(data_with_state$y[census_tracts_within_range]))
}

# make factual calculations
for (i in all.factual.exposure.upper.bounds){
  state_logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, beta1 = beta1_state, factual_exposure_upper_bound = i)
  state_factual.expected.events[i] <- get_expected_events(state_logit.p.hats)
  
  state.urbanity_logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state.urbanity, beta1 = beta1_state.urbanity, factual_exposure_upper_bound = i)
  state.urbanity_factual.expected.events[i] <- get_expected_events(state.urbanity_logit.p.hats)
}
rm(state_logit.p.hats, state.urbanity_logit.p.hats)

# calculate true number of events in each exposure bin
for (i in all.factual.exposure.upper.bounds){
  true.factual.events[i] <- get_true_factual_events(i)
}

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  for (j in 1:length(all.counterfactual.exposures)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    counterfactual_exposure <- all.counterfactual.exposures[j]
    if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
      state_logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
      state_counterfactual.expected.events[i,j] <- get_expected_events(state_logit.p.hats)
      state_expected.events.avoided[i,j] <- state_factual.expected.events[i] - state_counterfactual.expected.events[i,j]
      state_expected.events.avoided2[i,j] <- true.factual.events[i] - state_counterfactual.expected.events[i,j]
      
      state.urbanity_logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state.urbanity, beta1_state.urbanity, factual_exposure_upper_bound, counterfactual_exposure)
      state.urbanity_counterfactual.expected.events[i,j] <- get_expected_events(state.urbanity_logit.p.hats)
      state.urbanity_expected.events.avoided[i,j] <- state.urbanity_factual.expected.events[i] - state.urbanity_counterfactual.expected.events[i,j]
      state.urbanity_expected.events.avoided2[i,j] <- true.factual.events[i] - state.urbanity_counterfactual.expected.events[i,j]
    }
  }
}
rm(state_logit.p.hats, state.urbanity_logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
state_expected.events.avoided <- round(state_expected.events.avoided, 2)
state_expected.events.avoided2 <- round(state_expected.events.avoided2, 2)
state.urbanity_expected.events.avoided <- round(state.urbanity_expected.events.avoided, 2)
state.urbanity_expected.events.avoided2 <- round(state.urbanity_expected.events.avoided2, 2)
write.csv(state_expected.events.avoided, "results/factual_vs_counterfactual/filtered_FFL/state_expected.events.avoided.from.model.csv")
write.csv(state_expected.events.avoided2, "results/factual_vs_counterfactual/filtered_FFL/state_expected.events.avoided.from.reality.csv")
write.csv(state.urbanity_expected.events.avoided, "results/factual_vs_counterfactual/filtered_FFL/state.urbanity_expected.events.avoided.from.model.csv")
write.csv(state.urbanity_expected.events.avoided2, "results/factual_vs_counterfactual/filtered_FFL/state.urbanity_expected.events.avoided.from.reality.csv")

### compute confidence intervals

set.seed(100)
n_sims <- 10^4

## set up matrices and arrays to store results
state_factual.expected.events_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
state_counterfactual.expected.events_sims <- array(dim = c(n_sims,
                                                           length(all.factual.exposure.upper.bounds),
                                                           length(all.counterfactual.exposures)))
state_expected.events.avoided_sims <- copy(state_counterfactual.expected.events_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_state_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in all.factual.exposure.upper.bounds){
    state_logit.p.hats <- get_factual_logit_p_hat(beta0 = betas_state_sim[1], beta1 = betas_state_sim[2], factual_exposure_upper_bound = i)
    state_factual.expected.events_sims[sim, i] <- get_expected_events(state_logit.p.hats)
  }
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    for (j in 1:length(all.counterfactual.exposures)){
      factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
      counterfactual_exposure <- all.counterfactual.exposures[j]
      if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
        state_logit.p.hats <- get_counterfactual_logit_p_hat(betas_state_sim[1], betas_state_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
        state_counterfactual.expected.events_sims[sim, i,j] <- get_expected_events(state_logit.p.hats)
        state_expected.events.avoided_sims[sim, i,j] <- state_factual.expected.events_sims[sim, i] - state_counterfactual.expected.events_sims[sim, i,j]
      }
    }
  }
}
rm(state_logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval (using quantiles) from simulations
state_counterfactual.expected.events.from.model_mean <- apply(state_counterfactual.expected.events_sims, c(2,3), mean)

state_expected.events.avoided.from.model_mean <- apply(state_expected.events.avoided_sims, c(2,3), mean)
state_expected.events.avoided.from.model_CI.lower <- round(apply(state_expected.events.avoided_sims, c(2,3), quantile, probs = 0.025, na.rm=T), 0)
state_expected.events.avoided.from.model_CI.upper <- round(apply(state_expected.events.avoided_sims, c(2,3), quantile, probs = 0.975, na.rm=T), 0)

rownames(state_expected.events.avoided.from.model_mean) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.events.avoided.from.model_mean) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
rownames(state_expected.events.avoided.from.model_CI.lower) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.events.avoided.from.model_CI.lower) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
rownames(state_expected.events.avoided.from.model_CI.upper) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.events.avoided.from.model_CI.upper) <- paste(all.counterfactual.exposures, "miles (counterfactual)")

state_expected.events.avoided.from.model_CI <- copy(state_expected.events.avoided.from.model_CI.lower)
for (i in 1:nrow(state_expected.events.avoided.from.model_CI)){
  for (j in 1:ncol(state_expected.events.avoided.from.model_CI)){
    if (!is.na(state_expected.events.avoided.from.model_CI[i,j])){
      state_expected.events.avoided.from.model_CI[i,j] <- paste0("[", state_expected.events.avoided.from.model_CI.lower[i,j],
                                                                 ", ", state_expected.events.avoided.from.model_CI.upper[i,j], "]")
    }
  }
}
write.csv(state_expected.events.avoided.from.model_CI, "results/factual_vs_counterfactual/filtered_FFL/state_expected.events.avoided.from.model_95pctCI.csv")


#### Calculating number of people affected ####

# vector and dataframes to store results
state_factual.expected.ppl.affected <- rep(NA, length(all.factual.exposure.upper.bounds))
true.ppl.affected <- rep(NA, length(all.factual.exposure.upper.bounds))

state_counterfactual.expected.ppl.affected <- matrix(nrow = length(all.factual.exposure.upper.bounds),
                                               ncol = length(all.counterfactual.exposures)) # rows: factual 1-mile bins of exposure. columns: counterfactual exposures
rownames(state_counterfactual.expected.ppl.affected) <- paste0("[", all.factual.exposure.upper.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_counterfactual.expected.ppl.affected) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
state_expected.fewer.ppl.affected <- copy(state_counterfactual.expected.ppl.affected)
state_expected.fewer.ppl.affected.from.reality <- copy(state_counterfactual.expected.ppl.affected)

# function to get number of people affected
get_expected_ppl_affected <- function(factual_exposure_upper_bound, logit_p_hats, data){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  p_hats <- inv.logit(logit_p_hats)
  ppl <- data$total_population_2020[census_tracts_within_range]
  return(sum(p_hats * ppl))
}

get_true_ppl_affected <- function(factual_exposure_upper_bound, data){
  factual_exposure_lower_bound <- factual_exposure_upper_bound - 1
  census_tracts_within_range <- (factual_exposures >= factual_exposure_lower_bound) &
    (factual_exposures < factual_exposure_upper_bound)
  return(sum(data$y[census_tracts_within_range] * data$total_population_2020[census_tracts_within_range]))
}

# make factual calculations
for (i in all.factual.exposure.upper.bounds){
  state_logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, beta1 = beta1_state, factual_exposure_upper_bound = i)
  state_factual.expected.ppl.affected[i] <- get_expected_ppl_affected(i, state_logit.p.hats, data_with_state)
}
rm(state_logit.p.hats)

# calculate true number of people affected in each exposure bin
for (i in all.factual.exposure.upper.bounds){
  true.ppl.affected[i] <- get_true_ppl_affected(i, data_with_state)
}

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  for (j in 1:length(all.counterfactual.exposures)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    counterfactual_exposure <- all.counterfactual.exposures[j]
    if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
      state_logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
      state_counterfactual.expected.ppl.affected[i,j] <- get_expected_ppl_affected(factual_exposure_upper_bound, state_logit.p.hats, data_with_state)
      state_expected.fewer.ppl.affected[i,j] <- state_factual.expected.ppl.affected[i] - state_counterfactual.expected.ppl.affected[i,j]
      state_expected.fewer.ppl.affected.from.reality[i,j] <- true.ppl.affected[i] - state_counterfactual.expected.ppl.affected[i,j]
    }
  }
}
rm(state_logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
state_expected.fewer.ppl.affected <- round(state_expected.fewer.ppl.affected, 0)
state_expected.fewer.ppl.affected.from.reality <- round(state_expected.fewer.ppl.affected.from.reality, 0)
write.csv(state_expected.fewer.ppl.affected, "results/factual_vs_counterfactual/filtered_FFL/state_expected.fewer.ppl.affected.from.model.csv")
write.csv(state_expected.fewer.ppl.affected.from.reality, "results/factual_vs_counterfactual/filtered_FFL/state_expected.fewer.ppl.affected.from.reality.csv")

### Compute confidence intervals

set.seed(100)
n_sims <- 10^4

## set up matrices and arrays to store results
state_factual.expected.ppl.affected_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
state_counterfactual.expected.ppl.affected_sims <- array(dim = c(n_sims,
                                                           length(all.factual.exposure.upper.bounds),
                                                           length(all.counterfactual.exposures)))
state_expected.fewer.ppl.affected_sims <- copy(state_counterfactual.expected.ppl.affected_sims)
state_expected.fewer.ppl.affected.from.reality_sims <- copy(state_counterfactual.expected.ppl.affected_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_state_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in all.factual.exposure.upper.bounds){
    state_logit.p.hats <- get_factual_logit_p_hat(beta0 = betas_state_sim[1], beta1 = betas_state_sim[2], factual_exposure_upper_bound = i)
    state_factual.expected.ppl.affected_sims[sim, i] <- get_expected_ppl_affected(i, state_logit.p.hats, data_with_state)
  }
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    for (j in 1:length(all.counterfactual.exposures)){
      factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
      counterfactual_exposure <- all.counterfactual.exposures[j]
      if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
        state_logit.p.hats <- get_counterfactual_logit_p_hat(betas_state_sim[1], betas_state_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
        state_counterfactual.expected.ppl.affected_sims[sim,i,j] <- get_expected_ppl_affected(factual_exposure_upper_bound, state_logit.p.hats, data_with_state)
        state_expected.fewer.ppl.affected_sims[sim,i,j] <- state_factual.expected.ppl.affected_sims[sim,i] - state_counterfactual.expected.ppl.affected_sims[sim,i,j]
        state_expected.fewer.ppl.affected.from.reality_sims[sim,i,j] <- true.ppl.affected[i] - state_counterfactual.expected.ppl.affected_sims[sim,i,j]
      }
    }
  }
}
rm(state_logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval from simulations
state_counterfactual.expected.ppl.affected_mean <- apply(state_counterfactual.expected.ppl.affected_sims, c(2,3), mean)

state_expected.fewer.ppl.affected_mean <- apply(state_expected.fewer.ppl.affected_sims, c(2,3), mean)
state_expected.fewer.ppl.affected.from.reality_mean <- apply(state_expected.fewer.ppl.affected.from.reality_sims, c(2,3), mean)

state_expected.fewer.ppl.affected_CI.lower <- round(apply(state_expected.fewer.ppl.affected_sims, c(2,3), quantile, probs = 0.025, na.rm=T), 0)
state_expected.fewer.ppl.affected_CI.upper <- round(apply(state_expected.fewer.ppl.affected_sims, c(2,3), quantile, probs = 0.975, na.rm=T), 0)

state_expected.fewer.ppl.affected.from.reality_CI.lower <- round(apply(state_expected.fewer.ppl.affected.from.reality_sims, c(2,3), quantile, probs = 0.025, na.rm=T), 0)
state_expected.fewer.ppl.affected.from.reality_CI.upper <- round(apply(state_expected.fewer.ppl.affected.from.reality_sims, c(2,3), quantile, probs = 0.975, na.rm=T), 0)

rownames(state_counterfactual.expected.ppl.affected_mean) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_counterfactual.expected.ppl.affected_mean) <- paste(all.counterfactual.exposures, "miles (counterfactual)")

rownames(state_expected.fewer.ppl.affected_mean) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.fewer.ppl.affected_mean) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
rownames(state_expected.fewer.ppl.affected.from.reality_mean) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.fewer.ppl.affected.from.reality_mean) <- paste(all.counterfactual.exposures, "miles (counterfactual)")

rownames(state_expected.fewer.ppl.affected_CI.lower) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.fewer.ppl.affected_CI.lower) <- paste(all.counterfactual.exposures, "miles (counterfactual)")
rownames(state_expected.fewer.ppl.affected.from.reality_CI.lower) <- paste0("[", all.factual.exposure.lower.bounds, ",", all.factual.exposure.upper.bounds, ") miles", recycle0 = T)
colnames(state_expected.fewer.ppl.affected.from.reality_CI.lower) <- paste(all.counterfactual.exposures, "miles (counterfactual)")

# state_counterfactual.expected.ppl.affected_CI <- copy(state_expected.fewer.ppl.affected_CI.lower)
state_expected.fewer.ppl.affected_CI <- copy(state_expected.fewer.ppl.affected_CI.lower)
state_expected.fewer.ppl.affected.from.reality_CI <- copy(state_expected.fewer.ppl.affected.from.reality_CI.lower)
for (i in 1:nrow(state_expected.fewer.ppl.affected_CI)){
  for (j in 1:ncol(state_expected.fewer.ppl.affected_CI)){
    if (!is.na(state_expected.fewer.ppl.affected_CI[i,j])){
      state_expected.fewer.ppl.affected_CI[i,j] <- paste0("[", state_expected.fewer.ppl.affected_CI.lower[i,j],
                                                                 ", ", state_expected.fewer.ppl.affected_CI.upper[i,j], "]")
      state_expected.fewer.ppl.affected.from.reality_CI[i,j] <- paste0("[", state_expected.fewer.ppl.affected.from.reality_CI.lower[i,j],
                                                          ", ", state_expected.fewer.ppl.affected.from.reality_CI.upper[i,j], "]")
    }
  }
}
# write.csv(state_counterfactual.expected.ppl.affected_CI, "results/factual_vs_counterfactual/filtered_FFL/state_counterfactual.expected.ppl.affected_95pctCI.csv")
write.csv(state_expected.fewer.ppl.affected_CI, "results/factual_vs_counterfactual/filtered_FFL/state_expected.fewer.ppl.affected.from.model_95pctCI.csv")
write.csv(state_expected.fewer.ppl.affected.from.reality_CI, "results/factual_vs_counterfactual/filtered_FFL/state_expected.fewer.ppl.affected.from.reality_95pctCI.csv")
