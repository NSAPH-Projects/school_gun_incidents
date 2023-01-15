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
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))

# get 95th and 99th percentiles of exposure
exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))

# get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", "urban_rural", quantitative_confounders))

## Functions to get regression results ----

get_models <- function(df, model = "logistic", confounder_names){
  if (model == "logistic"){
    model <- glm(y ~ ., 
                 data = df[, c("y", "a", confounder_names)], 
                 family = "binomial")
  } else if (model == "negbin"){
    model <- glm.nb(y ~ .,
                    data = df[, c("y", "a", confounder_names)])
  } else message("model must be `logistic` or `negbin`")
  return(model)
}

get_all_models <- function(df){
  logistic_state <- get_models(df, "logistic", c("State_Name", quantitative_confounders))
  logistic_censusdiv <- get_models(df, "logistic", c("census_division_number", quantitative_confounders))
  negbin_state <- get_models(df, "negbin", c("State_Name", quantitative_confounders))
  negbin_censusdiv <- get_models(df, "negbin", c("census_division_number", quantitative_confounders))
  
  return(list(logistic_state, logistic_censusdiv, negbin_state, negbin_censusdiv))
}

get_all_models_with_urban_rural <- function(df){
  logistic_state <- get_models(df, "logistic", c("State_Name", quantitative_confounders, "urban_rural"))
  logistic_censusdiv <- get_models(df, "logistic", c("census_division_number", quantitative_confounders, "urban_rural"))
  negbin_state <- get_models(df, "negbin", c("State_Name", quantitative_confounders, "urban_rural"))
  negbin_censusdiv <- get_models(df, "negbin", c("census_division_number", quantitative_confounders, "urban_rural"))
  return(list(logistic_state, logistic_censusdiv, negbin_state, negbin_censusdiv))
}

get_coefs <- function(model, var_names){
  coefs <- summary(model)$coefficients
  coefs_subset <- coefs[var_names, ]
  coefs_codified <- apply(coefs_subset, 1, concatenate_results)
  return(coefs_codified)
}

## Get naive logistic regression results
# state.1.99_naivelogistic <- get_models(data_with_state[data_with_state$a >= exposure1.99[1] & data_with_state$a <= exposure1.99[2], ],
#                                        model = "logistic", c("State_Name", quantitative_confounders))
# state.1.99_naivelogistic_results <- concatenate_results(summary(state.1.99_naivelogistic)$coefficients["a",])

state.5.95_naivelogistic <- get_models(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ],
                                       model = "logistic", c("State_Name", quantitative_confounders))
state.5.95_naivelogistic_results <- concatenate_results(summary(state.5.95_naivelogistic)$coefficients["a",])

# state.urbanity.1.99_naivelogistic <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & data_with_urbanity_state$a <= exposure1.99[2], ],
#                                        model = "logistic", c("State_Name", "urban_rural", quantitative_confounders))
# state.urbanity.1.99_naivelogistic_results <- concatenate_results(summary(state.urbanity.1.99_naivelogistic)$coefficients["a",])

state.urbanity.5.95_naivelogistic <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ],
                                       model = "logistic", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.5.95_naivelogistic_results <- concatenate_results(summary(state.urbanity.5.95_naivelogistic)$coefficients["a",])


## Get naive negative binomial regression results (note though that our outcome is binary, not counts)
# state.1.99_naivenegbin <- get_models(data_with_state[data_with_state$a >= exposure1.99[1] & data_with_state$a <= exposure1.99[2], ],
#                                        model = "negbin", c("State_Name", quantitative_confounders))
# state.1.99_naivenegbin_results <- concatenate_results(summary(state.1.99_naivenegbin)$coefficients["a",])

state.5.95_naivenegbin <- get_models(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ],
                                       model = "negbin", c("State_Name", quantitative_confounders))
state.5.95_naivenegbin_results <- concatenate_results(summary(state.5.95_naivenegbin)$coefficients["a",])

# state.urbanity.1.99_naivenegbin <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & data_with_urbanity_state$a <= exposure1.99[2], ],
#                                                 model = "negbin", c("State_Name", "urban_rural", quantitative_confounders))
# state.urbanity.1.99_naivenegbin_results <- concatenate_results(summary(state.urbanity.1.99_naivenegbin)$coefficients["a",])

state.urbanity.5.95_naivenegbin <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ],
                                                model = "negbin", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.5.95_naivenegbin_results <- concatenate_results(summary(state.urbanity.5.95_naivenegbin)$coefficients["a",])


## Check for spatial confounding
gee_model <- function(df){
  data_contiguous_clusters <- df[order(df$State_Name), ]
  outcome <- gee(formula = y ~ .,
                 family = "binomial",
                 data = data_contiguous_clusters[, c("y", "a", quantitative_confounders)],
                 id = data_contiguous_clusters$State_Name,
                 corstr = "exchangeable")
  return(summary(outcome)$coefficients)
}

state.5.95_gee <- gee_model(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ])
# state.urbanity.5.95_gee <- gee_model(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ])
