rm(list=ls())
# set.seed(2022)

library(xtable)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
#setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get datasets
data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 1.609)

# Empirical Probabilities

ntreated <- length(which(data_binary_exposure$a == 1))
ncontrol <- length(which(data_binary_exposure$a == 0))
n <- ntreated + ncontrol

ytreated <- length(which(data_binary_exposure$a == 1 & data_binary_exposure$y == 1))
ycontrol <- length(which(data_binary_exposure$a == 0 & data_binary_exposure$y == 1))

emp_prob_treated <- ytreated/ntreated
emp_prob_control <- ycontrol/ncontrol

table(data_binary_exposure$y, data_binary_exposure$a)

emp_prob_treated * ntreated
emp_prob_control * ncontrol

data_binary_exposure$total_population_under_18 <- round(data_binary_exposure$total_population_2020 * (1 -  data_binary_exposure$prop_18plus))

nevents_emp=c()  
npeople_impacted_emp=c()
nevents_cft=c()  
npeople_impacted_cft=c()

for(i in 1:10000){
  data_binary_exposure$impute_emp = rep(NA, n)
  data_binary_exposure$impute_ctf = rep(NA, n)
  
  data_binary_exposure$impute_emp[data_binary_exposure$a == 0] <- rbinom(ncontrol, 1, emp_prob_control) # sample(data_binary_exposure$y[data_binary_expsure$a == 0], ncontrol, replace=T)
  data_binary_exposure$impute_emp[data_binary_exposure$a == 1] <- rbinom(ntreated, 1, emp_prob_treated) # sample(data_binary_exposure$y[data_binary_expsure$a == 1], ntreated, replace=T)
  
  data_binary_exposure$impute_ctf <- rbinom(n, 1, emp_prob_treated) # sample(data_binary_exposure$y[data_binary_expsure$a == 1], n, replace=T)
  
  nevents_emp = c(nevents_emp, length(which(data_binary_exposure$impute_emp == 1)))
  npeople_impacted_emp = c(npeople_impacted_emp, sum(data_binary_exposure$impute_emp *  data_binary_exposure$total_population_under_18))

  nevents_cft = c(nevents_cft, length(which(data_binary_exposure$impute_ctf == 1)))
  npeople_impacted_cft = c(npeople_impacted_cft, sum(data_binary_exposure$impute_ctf *  data_binary_exposure$total_population_under_18))
    
}

quantile(nevents_emp, probs = c(0.025,0.5, 0.975))
quantile(nevents_cft, probs = c(0.025,0.5, 0.975))

quantile(npeople_impacted_emp, probs = c(0.025, 0.5, 0.975))
quantile(npeople_impacted_cft, probs = c(0.025, 0.5, 0.975))

data_imputation <- as.data.frame(cbind(nevents_emp, nevents_cft, npeople_impacted_emp, npeople_impacted_cft))

# Plot Events
ggplot() +
  geom_density(data = data_imputation,
               aes(nevents_emp),
               fill = "#E69F00", color = "black", alpha = 0.7) +
  geom_density(data = data_imputation,
               aes(nevents_cft),
               fill = "#56B4E9", color = "black", alpha = 0.7) +
  xlab("Shooting Events (Factual Vs. Counterfactual)") + ylab("Density")

# Plot People Exposed
ggplot() +
  geom_density(data = data_imputation,
               aes(npeople_impacted_emp),
               fill = "#E69F00", color = "black", alpha = 0.7) +
  geom_density(data = data_imputation,
               aes(npeople_impacted_cft),
               fill = "#56B4E9", color = "black", alpha = 0.7) +
  xlab("People Impacted (Factual Vs. Counterfactual)") + ylab("Density")


