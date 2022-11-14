rm(list=ls())
# set.seed(2022)

library(ggplot2)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
#setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get n0 and n1: 1 mi cutoff for binary exposure
data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 1)
n0 <- sum(data_binary_exposure$a == 0)
n1 <- sum(data_binary_exposure$a == 1)

# Get causal effect estimates: 1 mi cutoff for binary exposure, 1:1 nearest neighbors matching
formula_matching <- as.formula(paste("a ~", paste(all_confounder_names, collapse = "+", sep = "")))
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure, seed = 42, one_to_one = T)
outcome <- get_match.data_outcome_model(matched_pop)
results <- summary(outcome, robust = "vcovHC", digits = 5)
beta0hat <- results$coefficients[1,1]
beta1hat <- results$coefficients[2,1]
p0hat <- exp(beta0hat) / (1 + exp(beta0hat))
p1hat <- exp(beta0hat + beta1hat) / (1 + exp(beta0hat + beta1hat))

# Estimate number of events prevented by assigning control units to treatment
nEvents <- n0 * (p0hat-p1hat)
nEvents


# Get under-18 population for all control (<= 1 mi exposure) tracts
data_incl_under18 <- get_binary_exposure_continuous_confounders_dataset(exposure_name = "mean_total_miles", write_file = F, cutoff = 1,
                                                               confounder_names = c(all_confounder_names, "pop18plus"), alternate_data = "data/all_tracts_2020_more_vars.csv", na_omit = T)
controltracts_under18population <- data_incl_under18$total_population_2020[data_incl_under18$a == 0] - data_incl_under18$pop18plus[data_incl_under18$a == 0]
sum_under18population <- sum(controltracts_under18population)

# Estimate number of under-18 affected by assigning control units to treatment
nChildrenAffected <- sum_under18population * (p0hat-p1hat)
nChildrenAffected

##### Simulations #####

# Simulate number of events prevented by assigning control units to treatment
set.seed(120)
n_iters <- 10^4

factual_events <- rbinom(n_iters, n0, p0hat)
counterfactual_events <- rbinom(n_iters, n0, p1hat)
factual_df <- data.frame(Scenario = "Factual", n_events = factual_events)
counterfactual_df <- data.frame(Scenario = "Counterfactual", n_events = counterfactual_events)
events_df <- rbind(factual_df, counterfactual_df)

ggplot() +
  geom_density(data = events_df, aes(n_events, fill = Scenario),
               color = "black", alpha = 0.7) +
  xlab("School Shootings (Jan 2014-May 2022)\nin Control Census Tracts") + ylab("Density")


# Simulate number of under-18 affected by assigning control units to treatment
set.seed(120)
n_iters <- 10^4

# simulate events in census tracts
factual_events <- matrix(rbinom(n0*n_iters, 1, p0hat), nrow = n_iters)
counterfactual_events <- matrix(rbinom(n0*n_iters, 1, p1hat), nrow = n_iters)

# for each iteration (row), calculate number of under-18 affected
get_under18affected <- function(event_indices){
  return(sum(controltracts_under18population[event_indices]))
}
factual_under18 <- apply(factual_events, 1, get_under18affected)
counterfactual_under18 <- apply(counterfactual_events, 1, get_under18affected)

# combine into 1 dataframe and plot
factual_df <- data.frame(Scenario = "Factual", under18 = factual_under18)
counterfactual_df <- data.frame(Scenario = "Counterfactual", under18 = counterfactual_under18)
under18affected_df <- rbind(factual_df, counterfactual_df)

ggplot() +
  geom_density(data = under18affected_df, aes(under18, fill = Scenario),
               color = "black", alpha = 0.7) +
  xlab("Number of Under-18 Affected (Jan 2014-May 2022)\nin Control Census Tracts") + ylab("Density")



