## Load packages ----
library(MASS)
library(data.table)
library(ggplot2)
library(tidyr)

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "code/helper_functions.R"))
source(paste0(dir, "code/functions_for_factual_vs_counterfactual.R"))
source(paste0(dir, "code/functions_using_gps.R"))

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
#state.5.95_match.cap99.logistic
#vcov(state.5.95_match.cap99.logistic)
 
beta0_state <- state.5.95_match.cap99.logistic$coefficients[1,1] #-4.1514
beta1_state <- state.5.95_match.cap99.logistic$coefficients[2,1] #-0.0296
vcov_state <- vcov(state.5.95_match.cap99.logistic)# matrix(c(0.0001321, -0.00001798, -0.00001798, 0.000003337), nrow = 2)

#### Set up counterfactual and factual calculations ####

# parameters for counterfactual calculation
all.factual.exposure.lower.bounds <- seq(0, 4, by = 1)
all.factual.exposure.upper.bounds <- all.factual.exposure.lower.bounds + 1
all.counterfactual.exposures <- all.factual.exposure.upper.bounds + 1

# get n for each exposure bin
n.tracts.in.bins <- sapply(all.counterfactual.exposures, get_number_of_tracts_in_bin, factual_exposures = factual_exposures)

#### #### ####
#### Calculate number of events avoided
#### #### ####

# vector and dataframes to store results
factual.expected.events <- rep(NA, length(all.factual.exposure.upper.bounds))
# true.factual.events <- rep(NA, length(all.factual.exposure.upper.bounds))

counterfactual.expected.events <- matrix(nrow = length(all.factual.exposure.upper.bounds),
                                         ncol = length(all.counterfactual.exposures)) # rows: factual half-mile bins of exposure. columns: counterfactual exposures
rownames(counterfactual.expected.events) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(counterfactual.expected.events) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")
# counterfactual.expected.events[, 1] <- n.tracts.in.bins

expected.events.avoided <- copy(counterfactual.expected.events)
# expected.events.avoided.from.reality <- copy(counterfactual.expected.events)

# make factual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  logit.p.hats <- get_factual_logit_p_hat(
    beta0 = beta0_state, 
    beta1 = beta1_state, 
    factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
  factual.expected.events[i] <- get_expected_events(logit.p.hats)
}
rm(logit.p.hats)

# # calculate true number of events in each exposure bin
# for (i in 1:length(all.factual.exposure.upper.bounds)){
#   true.factual.events[i] <- get_true_factual_events(all.factual.exposure.upper.bounds[i])
# }

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  for (j in 1:length(all.counterfactual.exposures)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    counterfactual_exposure <- all.counterfactual.exposures[j]
    if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
      logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, 
                                                     beta1_state, 
                                                     factual_exposure_upper_bound, 
                                                     counterfactual_exposure)
      counterfactual.expected.events[i,j] <- get_expected_events(logit.p.hats)
      expected.events.avoided[i,j] <- factual.expected.events[i] - counterfactual.expected.events[i,j]
      # expected.events.avoided.from.reality[i,j] <- true.factual.events[i] - counterfactual.expected.events[i,j]
    }
  }
}
rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
expected.events.avoided <- round(expected.events.avoided, 2)
# expected.events.avoided.from.reality <- round(expected.events.avoided.from.reality, 2)
write.csv(expected.events.avoided, 
          paste0(dir, "results/factual_vs_counterfactual/expected.events.avoided.from.model.csv"))
# write.csv(expected.events.avoided.from.reality, "results/factual_vs_counterfactual/expected.events.avoided.from.reality.csv")

### compute confidence intervals

set.seed(100)
n_sims <- 10^4

## set up matrices and arrays to store results
factual.expected.events_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
counterfactual.expected.events_sims <- array(dim = c(n_sims,
                                                           length(all.factual.exposure.upper.bounds),
                                                           length(all.counterfactual.exposures)))
expected.events.avoided_sims <- copy(counterfactual.expected.events_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    logit.p.hats <- get_factual_logit_p_hat(beta0 = betas_sim[1], 
                                            beta1 = betas_sim[2], 
                                            factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
    factual.expected.events_sims[sim, i] <- get_expected_events(logit.p.hats)
  }
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    for (j in 1:length(all.counterfactual.exposures)){
      factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
      counterfactual_exposure <- all.counterfactual.exposures[j]
      if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
        logit.p.hats <- get_counterfactual_logit_p_hat(betas_sim[1], 
                                                       betas_sim[2], 
                                                       factual_exposure_upper_bound, 
                                                       counterfactual_exposure)
        counterfactual.expected.events_sims[sim, i,j] <- get_expected_events(logit.p.hats)
        expected.events.avoided_sims[sim, i,j] <- factual.expected.events_sims[sim, i] - counterfactual.expected.events_sims[sim, i,j]
      }
    }
  }
}
rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval (using quantiles) from simulations
counterfactual.expected.events.from.model_mean <- apply(counterfactual.expected.events_sims, c(2,3), mean)

expected.events.avoided.from.model_mean <- apply(expected.events.avoided_sims, c(2,3), mean)
expected.events.avoided.from.model_CI.lower <- round(apply(expected.events.avoided_sims, c(2,3), quantile, probs = 0.025, na.rm=T), 2)
expected.events.avoided.from.model_CI.upper <- round(apply(expected.events.avoided_sims, c(2,3), quantile, probs = 0.975, na.rm=T), 2)

rownames(expected.events.avoided.from.model_mean) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(expected.events.avoided.from.model_mean) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")
rownames(expected.events.avoided.from.model_CI.lower) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(expected.events.avoided.from.model_CI.lower) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")
rownames(expected.events.avoided.from.model_CI.upper) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(expected.events.avoided.from.model_CI.upper) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")

expected.events.avoided.from.model_CI <- copy(expected.events.avoided.from.model_CI.lower)
for (i in 1:nrow(expected.events.avoided.from.model_CI)){
  for (j in 1:ncol(expected.events.avoided.from.model_CI)){
    if (!is.na(expected.events.avoided.from.model_CI[i,j])){
      expected.events.avoided.from.model_CI[i,j] <- paste0("[", expected.events.avoided.from.model_CI.lower[i,j],
                                                                 ", ", expected.events.avoided.from.model_CI.upper[i,j], "]")
    }
  }
}
write.csv(expected.events.avoided.from.model_CI, 
          paste0("results/factual_vs_counterfactual/expected.events.avoided.from.model_95pctCI.csv"))

#### #### ####
#### Calculate number of people affected ####
#### #### ####

# vector and dataframes to store results
factual.expected.ppl.affected <- rep(NA, length(all.factual.exposure.upper.bounds))
# true.ppl.affected <- rep(NA, length(all.factual.exposure.upper.bounds))

counterfactual.expected.ppl.affected <- matrix(nrow = length(all.factual.exposure.upper.bounds),
                                               ncol = length(all.counterfactual.exposures)) # rows: factual half-mile bins of exposure. columns: counterfactual exposures
rownames(counterfactual.expected.ppl.affected) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(counterfactual.expected.ppl.affected) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")
expected.fewer.ppl.affected <- copy(counterfactual.expected.ppl.affected)
# expected.fewer.ppl.affected.from.reality <- copy(counterfactual.expected.ppl.affected)

# make factual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, beta1 = beta1_state, factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
  factual.expected.ppl.affected[i] <- get_expected_ppl_affected(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state)
}
rm(logit.p.hats)

# calculate true number of people affected in each exposure bin
# for (i in 1:length(all.factual.exposure.upper.bounds)){
#   true.ppl.affected[i] <- get_true_ppl_affected(all.factual.exposure.upper.bounds[i], data_with_state)
# }

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  for (j in 1:length(all.counterfactual.exposures)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    counterfactual_exposure <- all.counterfactual.exposures[j]
    if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
      logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
      counterfactual.expected.ppl.affected[i,j] <- get_expected_ppl_affected(factual_exposure_upper_bound, logit.p.hats, data_with_state)
      expected.fewer.ppl.affected[i,j] <- factual.expected.ppl.affected[i] - counterfactual.expected.ppl.affected[i,j]
      # expected.fewer.ppl.affected.from.reality[i,j] <- true.ppl.affected[i] - counterfactual.expected.ppl.affected[i,j]
    }
  }
}
rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
expected.fewer.ppl.affected <- round(expected.fewer.ppl.affected, 0)
# expected.fewer.ppl.affected.from.reality <- round(expected.fewer.ppl.affected.from.reality, 0)
write.csv(expected.fewer.ppl.affected, 
          paste0(dir, "results/factual_vs_counterfactual/expected.fewer.ppl.affected.from.model.csv"))
# write.csv(expected.fewer.ppl.affected.from.reality, "results/factual_vs_counterfactual/filtered_FFL/exposure_in_half_miles/expected.fewer.ppl.affected.from.reality.csv")

### Compute confidence intervals

set.seed(100)
n_sims <- 10^4

## set up matrices and arrays to store results
factual.expected.ppl.affected_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
counterfactual.expected.ppl.affected_sims <- array(dim = c(n_sims,
                                                           length(all.factual.exposure.upper.bounds),
                                                           length(all.counterfactual.exposures)))
expected.fewer.ppl.affected_sims <- copy(counterfactual.expected.ppl.affected_sims)
# expected.fewer.ppl.affected.from.reality_sims <- copy(counterfactual.expected.ppl.affected_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    logit.p.hats <- get_factual_logit_p_hat(beta0 = betas_sim[1], beta1 = betas_sim[2], factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
    factual.expected.ppl.affected_sims[sim, i] <- get_expected_ppl_affected(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state)
  }
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    for (j in 1:length(all.counterfactual.exposures)){
      factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
      counterfactual_exposure <- all.counterfactual.exposures[j]
      if(counterfactual_exposure >= factual_exposure_upper_bound + 1){
        logit.p.hats <- get_counterfactual_logit_p_hat(betas_sim[1], betas_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
        counterfactual.expected.ppl.affected_sims[sim,i,j] <- get_expected_ppl_affected(factual_exposure_upper_bound, logit.p.hats, data_with_state)
        expected.fewer.ppl.affected_sims[sim,i,j] <- factual.expected.ppl.affected_sims[sim,i] - counterfactual.expected.ppl.affected_sims[sim,i,j]
        # expected.fewer.ppl.affected.from.reality_sims[sim,i,j] <- true.ppl.affected[i] - counterfactual.expected.ppl.affected_sims[sim,i,j]
      }
    }
  }
}
rm(betas_sim, logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval from simulations
# counterfactual.expected.ppl.affected_mean <- apply(counterfactual.expected.ppl.affected_sims, c(2,3), mean)

expected.fewer.ppl.affected_mean <- apply(expected.fewer.ppl.affected_sims, c(2,3), mean)
# expected.fewer.ppl.affected.from.reality_mean <- apply(expected.fewer.ppl.affected.from.reality_sims, c(2,3), mean)
expected.fewer.ppl.affected_CI.lower <- round(apply(expected.fewer.ppl.affected_sims, c(2,3), quantile, probs = 0.025, na.rm=T), 0)
expected.fewer.ppl.affected_CI.upper <- round(apply(expected.fewer.ppl.affected_sims, c(2,3), quantile, probs = 0.975, na.rm=T), 0)

rownames(expected.fewer.ppl.affected_mean) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(expected.fewer.ppl.affected_mean) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")
rownames(expected.fewer.ppl.affected_CI.lower) <- paste0("[", all.factual.exposure.lower.bounds, ", ", all.factual.exposure.upper.bounds, ") half-miles", recycle0 = T)
colnames(expected.fewer.ppl.affected_CI.lower) <- paste(all.counterfactual.exposures, "half-miles (counterfactual)")

expected.fewer.ppl.affected_CI <- copy(expected.fewer.ppl.affected_CI.lower)
for (i in 1:nrow(expected.fewer.ppl.affected_CI)){
  for (j in 1:ncol(expected.fewer.ppl.affected_CI)){
    if (!is.na(expected.fewer.ppl.affected_CI[i,j])){
      expected.fewer.ppl.affected_CI[i,j] <- paste0("[", expected.fewer.ppl.affected_CI.lower[i,j],
                                                                 ", ", expected.fewer.ppl.affected_CI.upper[i,j], "]")
    }
  }
}
write.csv(expected.fewer.ppl.affected_CI, 
          paste0(dir, "results/factual_vs_counterfactual/expected.fewer.ppl.affected.from.model_95pctCI.csv"))


#### #### #### #### #### ####
##
## Calculate diagonal of matrix for all exposures (events and people avoided by moving only 1 half-mile) 
##
#### #### #### #### #### ####

### Set up counterfactual and factual calculations ###

# parameters for counterfactual calculation
all.factual.exposure.lower.bounds <- seq(0, ceiling(max(factual_exposures)), by = 1)
all.factual.exposure.upper.bounds <- all.factual.exposure.lower.bounds + 1
all.counterfactual.exposures <- all.factual.exposure.upper.bounds + 1

# get n for each exposure bin
n.tracts.in.bins <- sapply(all.counterfactual.exposures, get_number_of_tracts_in_bin)

#### #### ####
### Calculate number of events avoided ###
#### #### ####

# vector to store results
factual.expected.events <- rep(NA, length(all.factual.exposure.upper.bounds))
counterfactual.expected.events <- rep(NA, length(all.counterfactual.exposures))
expected.events.avoided <- copy(counterfactual.expected.events)

# make factual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, beta1 = beta1_state, factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
  factual.expected.events[i] <- get_expected_events(logit.p.hats)
}
rm(logit.p.hats)

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
  counterfactual_exposure <- all.counterfactual.exposures[i]
  logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
  counterfactual.expected.events[i] <- get_expected_events(logit.p.hats)
  expected.events.avoided[i] <- factual.expected.events[i] - counterfactual.expected.events[i]
}
rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
expected.events.avoided <- round(expected.events.avoided, 2)
cat("Total events avoided:", sum(expected.events.avoided))


### Compute confidence intervals for number of events avoided ###

set.seed(100)
n_sims <- 10^4

## set up matrices to store results
factual.expected.events_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
counterfactual.expected.events_sims <- matrix(nrow = n_sims, ncol = length(all.counterfactual.exposures))
expected.events.avoided_sims <- copy(counterfactual.expected.events_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    logit.p.hats.factual <- get_factual_logit_p_hat(betas_sim[1], betas_sim[2], factual_exposure_upper_bound)
    factual.expected.events_sims[sim, i] <- get_expected_events(logit.p.hats.factual)
    
    counterfactual_exposure <- all.counterfactual.exposures[i]
    logit.p.hats.counterfactual <- get_counterfactual_logit_p_hat(betas_sim[1], betas_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
    counterfactual.expected.events_sims[sim, i] <- get_expected_events(logit.p.hats.counterfactual)
    expected.events.avoided_sims[sim, i] <- factual.expected.events_sims[sim, i] - counterfactual.expected.events_sims[sim, i]
  }
}
rm(logit.p.hats.factual, logit.p.hats.counterfactual, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval (using quantiles) from simulations
expected.events.avoided.from.model_mean <- apply(expected.events.avoided_sims, 2, mean)
expected.events.avoided.from.model_CI.lower <- round(apply(expected.events.avoided_sims, 2, quantile, probs = 0.025, na.rm=T), 2)
expected.events.avoided.from.model_CI.upper <- round(apply(expected.events.avoided_sims, 2, quantile, probs = 0.975, na.rm=T), 2)

## calculate total events avoided in all census tracts (sum CI bounds to get conservative CI) ##
cat("Lower bound of 95% credible interval for total events avoided:", sum(expected.events.avoided.from.model_CI.lower))
cat("Upper bound of 95% credible interval for total events avoided:", sum(expected.events.avoided.from.model_CI.upper))

#### #### ####
### Calculate number of people affected ###
#### #### ####

# vector to store results
factual.expected.ppl.affected <- rep(NA, length(all.factual.exposure.upper.bounds))
counterfactual.expected.ppl.affected <- rep(NA, length(all.counterfactual.exposures))
expected.fewer.ppl.affected <- copy(counterfactual.expected.ppl.affected)

# make factual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, beta1 = beta1_state, factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
  factual.expected.ppl.affected[i] <- get_expected_ppl_affected(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state)
}
rm(logit.p.hats)

# make counterfactual calculations
for (i in 1:length(all.factual.exposure.upper.bounds)){
  factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
  counterfactual_exposure <- all.counterfactual.exposures[i]
  logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
  counterfactual.expected.ppl.affected[i] <- get_expected_ppl_affected(factual_exposure_upper_bound, logit.p.hats, data_with_state)
  expected.fewer.ppl.affected[i] <- factual.expected.ppl.affected[i] - counterfactual.expected.ppl.affected[i]
}
rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
expected.fewer.ppl.affected <- round(expected.fewer.ppl.affected, 0)


### Compute confidence intervals for fewer people affected ###

set.seed(100)
n_sims <- 10^4

## set up matrices to store results
factual.expected.ppl.affected_sims <- matrix(nrow = n_sims, ncol = length(all.factual.exposure.upper.bounds))
counterfactual.expected.ppl.affected_sims <- matrix(nrow = n_sims, ncol = length(all.counterfactual.exposures))
expected.fewer.ppl.affected_sims <- copy(counterfactual.expected.ppl.affected_sims)

## simulate
# to do: use sapply instead of for loop, for speed
for (sim in 1:n_sims){
  betas_sim <- mvrnorm(1, c(beta0_state, beta1_state), vcov_state)
  
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    logit.p.hats.factual <- get_factual_logit_p_hat(beta0 = betas_sim[1], beta1 = betas_sim[2], factual_exposure_upper_bound = factual_exposure_upper_bound)
    factual.expected.ppl.affected_sims[sim, i] <- get_expected_ppl_affected(all.factual.exposure.upper.bounds[i], logit.p.hats.factual, data_with_state)
    
    counterfactual_exposure <- all.counterfactual.exposures[i]
    logit.p.hats.counterfactual <- get_counterfactual_logit_p_hat(betas_sim[1], betas_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
    counterfactual.expected.ppl.affected_sims[sim,i] <- get_expected_ppl_affected(factual_exposure_upper_bound, logit.p.hats.counterfactual, data_with_state)
    expected.fewer.ppl.affected_sims[sim,i] <- factual.expected.ppl.affected_sims[sim,i] - counterfactual.expected.ppl.affected_sims[sim,i]
  }
}
rm(betas_sim, logit.p.hats.factual, logit.p.hats.counterfactual, factual_exposure_upper_bound, counterfactual_exposure)

## calculate mean and 95% credible interval from simulations
expected.fewer.ppl.affected_mean <- apply(expected.fewer.ppl.affected_sims, 2, mean)
expected.fewer.ppl.affected_CI.lower <- round(apply(expected.fewer.ppl.affected_sims, 2, quantile, probs = 0.025, na.rm=T), 0)
expected.fewer.ppl.affected_CI.upper <- round(apply(expected.fewer.ppl.affected_sims, 2, quantile, probs = 0.975, na.rm=T), 0)

## calculate total fewer people affected in all census tracts (sum CI bounds to get conservative CI) ##
cat("Lower bound of 95% credible interval for total fewer people affected:", sum(expected.fewer.ppl.affected_CI.lower))
cat("Upper bound of 95% credible interval for total fewer people affected:", sum(expected.fewer.ppl.affected_CI.upper))

