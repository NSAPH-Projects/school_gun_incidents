library(xtable)
library(gee)

setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

# get data for continuous treatment
df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_with_state <- na.omit(data_with_state)
exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))

# get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", "urban_rural", quantitative_confounders))
data_with_urbanity_state <- na.omit(data_with_urbanity_state)

# get naive logistic regression results
state.1.99_naivelogistic <- get_models(data_with_state[data_with_state$a >= exposure1.99[1] & data_with_state$a <= exposure1.99[2], ],
                                       model = "logistic", c("State_Name", quantitative_confounders))
state.1.99_naivelogistic_results <- concatenate_results(summary(state.1.99_naivelogistic)$coefficients["a",])
all_results[Cat_Covariates == "State" & Model == "Naive Logistic" & Exposure_Range == formatted1.99, Results := state.1.99_naivelogistic_results]

state.5.95_naivelogistic <- get_models(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ],
                                       model = "logistic", c("State_Name", quantitative_confounders))
state.5.95_naivelogistic_results <- concatenate_results(summary(state.5.95_naivelogistic)$coefficients["a",])
all_results[Cat_Covariates == "State" & Model == "Naive Logistic" & Exposure_Range == formatted5.95, Results := state.5.95_naivelogistic_results]

state.urbanity.1.99_naivelogistic <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & data_with_urbanity_state$a <= exposure1.99[2], ],
                                       model = "logistic", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.1.99_naivelogistic_results <- concatenate_results(summary(state.urbanity.1.99_naivelogistic)$coefficients["a",])
all_results[Cat_Covariates == "State + Urbanity" & Model == "Naive Logistic" & Exposure_Range == formatted1.99, Results := state.urbanity.1.99_naivelogistic_results]

state.urbanity.5.95_naivelogistic <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ],
                                       model = "logistic", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.5.95_naivelogistic_results <- concatenate_results(summary(state.urbanity.5.95_naivelogistic)$coefficients["a",])
all_results[Cat_Covariates == "State + Urbanity" & Model == "Naive Logistic" & Exposure_Range == formatted5.95, Results := state.urbanity.5.95_naivelogistic_results]

# get naive negative binomial regression results (note though that our outcome is binary, not counts)
state.1.99_naivenegbin <- get_models(data_with_state[data_with_state$a >= exposure1.99[1] & data_with_state$a <= exposure1.99[2], ],
                                       model = "negbin", c("State_Name", quantitative_confounders))
state.1.99_naivenegbin_results <- concatenate_results(summary(state.1.99_naivenegbin)$coefficients["a",])
all_results[Cat_Covariates == "State" & Model == "Naive NegBin" & Exposure_Range == formatted1.99, Results := state.1.99_naivenegbin_results]

state.5.95_naivenegbin <- get_models(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ],
                                       model = "negbin", c("State_Name", quantitative_confounders))
state.5.95_naivenegbin_results <- concatenate_results(summary(state.5.95_naivenegbin)$coefficients["a",])
all_results[Cat_Covariates == "State" & Model == "Naive NegBin" & Exposure_Range == formatted5.95, Results := state.5.95_naivenegbin_results]

state.urbanity.1.99_naivenegbin <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure1.99[1] & data_with_urbanity_state$a <= exposure1.99[2], ],
                                                model = "negbin", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.1.99_naivenegbin_results <- concatenate_results(summary(state.urbanity.1.99_naivenegbin)$coefficients["a",])
all_results[Cat_Covariates == "State + Urbanity" & Model == "Naive NegBin" & Exposure_Range == formatted1.99, Results := state.urbanity.1.99_naivenegbin_results]

state.urbanity.5.95_naivenegbin <- get_models(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ],
                                                model = "negbin", c("State_Name", "urban_rural", quantitative_confounders))
state.urbanity.5.95_naivenegbin_results <- concatenate_results(summary(state.urbanity.5.95_naivenegbin)$coefficients["a",])
all_results[Cat_Covariates == "State + Urbanity" & Model == "Naive NegBin" & Exposure_Range == formatted5.95, Results := state.urbanity.5.95_naivenegbin_results]


# check for spatial confounding
# To Do: consider AR-M instead of independence (but then would have to order states so that closer states are correlated)
gee_model <- function(df){
  data_contiguous_clusters <- df[order(df$State_Name), ]
  outcome <- gee(formula = y ~ .,
                 family = "binomial",
                 data = data_contiguous_clusters[, c("y", "a", quantitative_confounders)],
                 id = data_contiguous_clusters$State_Name,
                 corstr = "independence")
  return(summary(outcome)$coefficients)
}

state.5.95_gee <- gee_model(data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ])
# state.urbanity.5.95_gee <- gee_model(data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ])


## Functions to get regression results

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

## Make tables of results

# for each table (i.e., for each data set)
model_names <- c("Logistic, factor(State)", "Logistic, factor(Census Div)",
                 "NegBin, factor(State)", "NegBin, factor(Census Div)")

# table for all data
var_names_for_table <- c("a", "total_crime_2021", "total_population_2020", "mental_health_index", "prop_hunted_with_shotgun_2021", "log_median_hh_income", "prop_black_only") # "urban_rural"
all_data_models <- get_all_models(data_analysis)
all_data_coefs <- sapply(all_data_models, get_coefs, var_names = var_names_for_table)
all_data_dispersion <- sapply(all_data_models, get_dispersion, n = nrow(data_analysis))
all_data_AIC <- sapply(all_data_models, function(model) round(summary(model)$aic, 1))
all_data_table <- rbind(all_data_coefs, all_data_dispersion, all_data_AIC, "n" = nrow(data_analysis))
colnames(all_data_table) <- model_names
xtable(all_data_table)

# table for all data including urban_rural_variable
var_names_for_urban_rural_table <- c("a", "total_crime_2021", "total_population_2020", "mental_health_index", "prop_hunted_with_shotgun_2021", "log_median_hh_income", "prop_black_only", "urban_rural")
all_data_models <- get_all_models_with_urban_rural(data_analysis)
all_data_coefs <- sapply(all_data_models, get_coefs, var_names = var_names_for_urban_rural_table)
all_data_dispersion <- sapply(all_data_models, get_dispersion, n = nrow(data_analysis))
all_data_AIC <- sapply(all_data_models, function(model) round(summary(model)$aic, 1))
all_data_table <- rbind(all_data_coefs, all_data_dispersion, all_data_AIC, "n" = nrow(data_analysis))
colnames(all_data_table) <- model_names
xtable(all_data_table)

