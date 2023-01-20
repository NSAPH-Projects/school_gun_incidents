## Load packages ----
library(gee)
library(MASS)
library(data.table)

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "code/helper_functions.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## Main body ----

# prepare dataset for main analysis
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates))

# get 95th and 99th percentiles of exposure
exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))

# get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", "urban_rural", quantitative_covariates))

## Get naive logistic regression results

models <- list()
results <- list()
models[["naivelogistic"]] <- list()
results[["naivelogistic"]] <- list()

models[["naivelogistic"]][["state.1.99"]] <- get_models(
  data_with_state[data_with_state$a >= exposure1.99[1] & 
                    data_with_state$a <= exposure1.99[2], ], 
  model = "logistic", 
  covariate_names = c("State_Name", quantitative_covariates)
  )

results[["naivelogistic"]][["state.1.99"]] <- concatenate_results(
  summary(models[["naivelogistic"]][["state.1.99"]])$coefficients["a",]
  )

models[["naivelogistic"]][["state.5.95"]] <- get_models(
  data_with_state[data_with_state$a >= exposure5.95[1] & 
                    data_with_state$a <= exposure5.95[2], ],
  model = "logistic", 
  covariate_names = c("State_Name", quantitative_covariates)
  )

results[["naivelogistic"]][["state.5.95"]] <- concatenate_results(
  summary(models[["naivelogistic"]][["state.5.95"]])$coefficients["a",]
  )

models[["naivelogistic"]][["state.urbanity.1.99"]] <- get_models(
  data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & 
                             data_with_urbanity_state$a <= exposure1.99[2], ],
  model = "logistic", 
  covariate_names = c("State_Name", "urban_rural", quantitative_covariates)
  )

results[["naivelogistic"]][["state.urbanity.1.99"]] <- concatenate_results(
  summary(models[["naivelogistic"]][["state.urbanity.1.99"]])$coefficients["a",]
  )

models[["naivelogistic"]][["state.urbanity.5.95"]] <- get_models(
  data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & 
                             data_with_urbanity_state$a <= exposure5.95[2], ],
  model = "logistic", 
  covariate_names = c("State_Name", "urban_rural", quantitative_covariates)
  )

results[["naivelogistic"]][["state.urbanity.5.95"]] <- concatenate_results(
  summary(models[["naivelogistic"]][["state.urbanity.5.95"]])$coefficients["a",]
  )

# Get naive negative binomial regression results (note though that our outcome is binary, not counts)
models[["naivenegbin"]][["state.1.99"]] <- get_models(
  data_with_state[data_with_state$a >= exposure1.99[1] & 
                    data_with_state$a <= exposure1.99[2], ],
  model = "negbin", 
  covariate_names = c("State_Name", quantitative_covariates)
  )

results[["naivenegbin"]][["state.1.99"]] <- concatenate_results(
  summary(models[["naivenegbin"]][["state.1.99"]])$coefficients["a",]
  )

models[["naivenegbin"]][["state.5.95"]] <- get_models(
  data_with_state[data_with_state$a >= exposure5.95[1] & 
                    data_with_state$a <= exposure5.95[2], ], 
  model = "negbin",
  covariate_names = c("State_Name", quantitative_covariates)
  )

results[["naivenegbin"]][["state.5.95"]] <- concatenate_results(
  summary(models[["naivenegbin"]][["state.5.95"]])$coefficients["a",]
  )

models[["naivenegbin"]][["state.urbanity.1.99"]] <- get_models(
  data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & 
                             data_with_urbanity_state$a <= exposure1.99[2], ],
  model = "negbin", 
  covariate_names = c("State_Name", "urban_rural", quantitative_covariates)
  )

results[["naivenegbin"]][["state.urbanity.1.99"]] <- concatenate_results(
  summary(models[["naivenegbin"]][["state.urbanity.1.99"]])$coefficients["a",]
  )

models[["naivenegbin"]][["state.urbanity.5.95"]] <- get_models(
  data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & 
                             data_with_urbanity_state$a <= exposure5.95[2], ],
  model = "negbin", 
  covariate_names = c("State_Name", "urban_rural", quantitative_covariates)
  )

results[["naivenegbin"]][["state.urbanity.5.95"]] <- concatenate_results(
  summary(models[["naivenegbin"]][["state.urbanity.5.95"]])$coefficients["a",])

print(models)
print(results)

## Check for spatial confounding
rm(models)

gee_model <- function(df){
  data_contiguous_clusters <- df[order(df$State_Name), ]
  outcome <- gee(formula = y ~ .,
                 family = "binomial",
                 data = data_contiguous_clusters[, c("y", "a", quantitative_covariates)],
                 id = data_contiguous_clusters$State_Name,
                 corstr = "exchangeable")
  return(summary(outcome)$coefficients)
}

state.5.95_gee <- gee_model(
  data_with_state[data_with_state$a >= exposure5.95[1] & 
                    data_with_state$a <= exposure5.95[2], ]
  )

print(state.5.95_gee)

# state.urbanity.5.95_gee <- gee_model(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ])
