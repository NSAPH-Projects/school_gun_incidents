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
#state.5.95_match.cap99.logistic
#vcov(state.5.95_match.cap99.logistic)
 
beta0_state <- state.5.95_match.cap99.logistic$coefficients[1,1] #-4.1514
beta1_state <- state.5.95_match.cap99.logistic$coefficients[2,1] #-0.0296
vcov_state <- vcov(state.5.95_match.cap99.logistic)# matrix(c(0.0001321, -0.00001798, -0.00001798, 0.000003337), nrow = 2)

get_expected_data <- function(upper_bound, logit.p.hats, data_with_state, data_type_processed) {
  if(data_type_processed=="events.avoided") {
    return (get_expected_events(logit.p.hats))
  } else if(data_type_processed=="fewer.ppl.affected"){
    return (get_expected_ppl_affected(upper_bound, logit.p.hats, data_with_state))
  }
}

process_half_mile_data <- function(data_type_processed) {
  #### Set up counterfactual and factual calculations ####
  
  # parameters for counterfactual calculation
  all.factual.exposure.lower.bounds <- seq(0, 4, by = 1)
  all.factual.exposure.upper.bounds <- all.factual.exposure.lower.bounds + 1
  all.counterfactual.exposures <- all.factual.exposure.upper.bounds + 1
  
  # get n for each exposure bin
  n.tracts.in.bins <- sapply(all.counterfactual.exposures, get_number_of_tracts_in_bin, factual_exposures = factual_exposures)
  
  #### #### ####
  #### Calculate number of data_type_processed
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
    factual.expected.events[i] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
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
        counterfactual.expected.events[i,j] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
        expected.events.avoided[i,j] <- factual.expected.events[i] - counterfactual.expected.events[i,j]
        # expected.events.avoided.from.reality[i,j] <- true.factual.events[i] - counterfactual.expected.events[i,j]
      }
    }
  }
  rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
  expected.events.avoided <- round(expected.events.avoided, 2)
  # expected.events.avoided.from.reality <- round(expected.events.avoided.from.reality, 2)
  
  # write.csv(expected.events.avoided.from.reality, "results/factual_vs_counterfactual/expected.events.avoided.from.reality.csv")
  
  ### compute credible intervals
  
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
      factual.expected.events_sims[sim, i] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
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
          counterfactual.expected.events_sims[sim, i,j] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
          expected.events.avoided_sims[sim, i,j] <- factual.expected.events_sims[sim, i] - counterfactual.expected.events_sims[sim, i,j]
        }
      }
    }
  }
  rm(betas_sim, logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
  
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
            paste0(dir,"results/factual_vs_counterfactual/expected.",data_type_processed,".from.model_95pctCI.csv"))
  
}

process_one_half_mile_data <- function(data_type_processed) {
  ### Set up counterfactual and factual calculations ###
  
  # parameters for counterfactual calculation
  all.factual.exposure.lower.bounds <- seq(0, ceiling(max(factual_exposures)), by = 1)
  all.factual.exposure.upper.bounds <- all.factual.exposure.lower.bounds + 1
  all.counterfactual.exposures <- all.factual.exposure.upper.bounds + 1
  
  # get n for each exposure bin
  n.tracts.in.bins <- sapply(all.counterfactual.exposures, get_number_of_tracts_in_bin, factual_exposures = factual_exposures)
  
  #### #### ####
  ### Calculate number of data_type_processed ###
  #### #### ####
  
  # vector to store results
  factual.expected.events <- rep(NA, length(all.factual.exposure.upper.bounds))
  counterfactual.expected.events <- rep(NA, length(all.counterfactual.exposures))
  expected.events.avoided <- copy(counterfactual.expected.events)
  
  # make factual calculations
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    logit.p.hats <- get_factual_logit_p_hat(beta0 = beta0_state, 
                                            beta1 = beta1_state, 
                                            factual_exposure_upper_bound = all.factual.exposure.upper.bounds[i])
    factual.expected.events[i] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
  }
  rm(logit.p.hats)
  
  # make counterfactual calculations
  for (i in 1:length(all.factual.exposure.upper.bounds)){
    factual_exposure_upper_bound <- all.factual.exposure.upper.bounds[i]
    counterfactual_exposure <- all.counterfactual.exposures[i]
    logit.p.hats <- get_counterfactual_logit_p_hat(beta0_state, beta1_state, factual_exposure_upper_bound, counterfactual_exposure)
    counterfactual.expected.events[i] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats, data_with_state, data_type_processed)
    expected.events.avoided[i] <- factual.expected.events[i] - counterfactual.expected.events[i]
  }
  rm(logit.p.hats, factual_exposure_upper_bound, counterfactual_exposure)
  expected.events.avoided <- round(expected.events.avoided, 2)
  cat("Total" , paste(strsplit(data_type_processed, "\\.")[[1]], collapse=' ') ,":", sum(expected.events.avoided))
  
  
  ### Compute credible intervals for number of events avoided ###
  
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
      factual.expected.events_sims[sim, i] <- get_expected_data(all.factual.exposure.upper.bounds[i],
                                                                logit.p.hats.factual, 
                                                                data_with_state,
                                                                data_type_processed)
      
      counterfactual_exposure <- all.counterfactual.exposures[i]
      logit.p.hats.counterfactual <- get_counterfactual_logit_p_hat(betas_sim[1], betas_sim[2], factual_exposure_upper_bound, counterfactual_exposure)
      counterfactual.expected.events_sims[sim, i] <- get_expected_data(all.factual.exposure.upper.bounds[i], logit.p.hats.counterfactual, data_with_state, data_type_processed)
      expected.events.avoided_sims[sim, i] <- factual.expected.events_sims[sim, i] - counterfactual.expected.events_sims[sim, i]
    }
  }
  rm(logit.p.hats.factual, logit.p.hats.counterfactual, factual_exposure_upper_bound, counterfactual_exposure)
  
  ## calculate mean and 95% credible interval (using quantiles) from simulations
  expected.events.avoided.from.model_mean <- apply(expected.events.avoided_sims, 2, mean)
  expected.events.avoided.from.model_CI.lower <- round(apply(expected.events.avoided_sims, 2, quantile, probs = 0.025, na.rm=T), 2)
  expected.events.avoided.from.model_CI.upper <- round(apply(expected.events.avoided_sims, 2, quantile, probs = 0.975, na.rm=T), 2)
  
  ## calculate total data_type_processed in all census tracts (sum CI bounds to get conservative CI) ##
  cat("Lower bound of 95% credible interval for total", paste(strsplit(data_type_processed, "\\.")[[1]], collapse=' '), ":",
      sum(expected.events.avoided.from.model_CI.lower), 
      sep = " ",
      file=paste0(dir, "results/factual_vs_counterfactual/expected.",data_type_processed,".from.model_CI.lower"), 
      append=FALSE)
  cat("Upper bound of 95% credible interval for total", paste(strsplit(data_type_processed, "\\.")[[1]], collapse=' '), ":",
      sum(expected.events.avoided.from.model_CI.upper), 
      sep = " ",
      file=paste0(dir, "results/factual_vs_counterfactual/expected.",data_type_processed,".from.model_CI.lower"), 
      append=TRUE)
  
}

process_data <- function() {
  process_half_mile_data("events.avoided")
  process_half_mile_data("fewer.ppl.affected")
  process_one_half_mile_data("events.avoided")
  process_one_half_mile_data("fewer.ppl.affected")
}

process_data()

