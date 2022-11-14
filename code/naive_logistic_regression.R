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

# get data stratified by prop_black_only
# Q1black <- quantile(data_analysis$prop_black_only, 0.25)
# Q3black <- quantile(data_analysis$prop_black_only, 0.75)
# belowQ1black <- data_analysis[data_analysis$prop_black_only <= Q1black, ]
# aboveQ3black <- data_analysis[data_analysis$prop_black_only >= Q3black, ]
# Q2black <- median(data_analysis$prop_black_only)
# belowQ2black <- data_analysis[data_analysis$prop_black_only <= Q2black, ]
# aboveQ2black <- data_analysis[data_analysis$prop_black_only >= Q2black, ]

## get data for continuous treatment trimmed at 95th percentile
# data_trim_exposure <- data_analysis[which(data_analysis$a < quantile(data_analysis$a, 0.95)), ]

## get data for binary treatment with multiple alternative cutoffs
# data_binary_exposure1mi <- copy(data_analysis)
# data_binary_exposure1mi$mean_total_miles <- 1 * (data_binary_exposure1mi$mean_total_miles  > 1)

## Oct 27 edit

# w_grid5.95 <- seq(exposure5.95[1], exposure5.95[2], length.out = 100)
# spline_state.5.95 <- mgcv::bam(y~ s(a, bs='cr', k=3) + State_Name + total_population_2020 +
#                                  housing_units_per_100_sq_miles + area_sq_miles + log_median_hh_income +
#                                  schools_per_100_sq_miles + log_median_hh_income_15to24 + total_crime_2021 +
#                                  dealers_per_100_sq_miles + mental_health_index + daytime_pop_2021 + prop_white_only +
#                                  prop_black_only + prop_asian_only + prop_multiracial + prop_hispanic_latino +
#                                  prop_food_stamps_2019 + prop_public_assist_income_2019 + prop_below_poverty_2019 +
#                                  prop_hunted_with_shotgun_2021 + prop_bachelor_deg_25plus_2021 + prop_grad_deg_25plus_2021 +
#                                  prop_unemployed_2021 + prop_unemployed_16to24_2021 + prop_institutional_group +
#                                  prop_noninstitutional_group + prop_18plus + prop_without_vehicles_2019,
#                                data = data_with_state,
#                                family=binomial(link="logit"))
# # To do: use this formula instead
# # as.formula(paste("y ~ s(a, bs='cr', k=3)", paste(c("State_Name", quantitative_confounders), collapse = " + ", sep = "")))
# plot(spline_state.5.95)

# # calculate global max (change point)
# plot <- plot(spline_state.5.95)
# cutoff <- plot[[1]]$x[which.max(plot[[1]]$fit)]

# pred.spline_state.5.95 <- predict(spline_state.5.95, newdata = list(a = w_grid5.95), se = T)
# pred.probability_state.5.95 <- inverse_logit(pred.spline_state.5.95$fit)

# png(file = paste0("results/splines/filtered_FFL/associational/assoc_state.5.95_3knots.logodds.png"))
# p <- plot(w_grid5.95, pred.spline_state.5.95$fit, type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (miles)", ylab = "Log odds (log(p/(1-p))")
# lines(w_grid5.95, pred.spline_state.5.95$fit + 2*pred.spline_state.5.95$se.fit, lty = "dashed", lwd = 2, col = "green")
# lines(w_grid5.95, pred.spline_state.5.95$fit - 2*pred.spline_state.5.95$se.fit, lty = "dashed", lwd = 2, col = "green")
# dev.off()
# 
# png(file = paste0("results/splines/filtered_FFL/associational/assoc_state.5.95_3knots.probability.png"))
# p <- plot(w_grid5.95, pred.probability_state.5.95, type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (miles)", ylab = "Probability (p)")
# dev.off()

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

# Neg Bin
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
  # } else if (model == "zinb"){
  #   model <- zeroinfl(y ~ . | . -schools_per_sq_meters -prop_below_poverty_2019 -prop_grad_deg_25plus_2021 -prop_unemployed16_to24_2021,
  #                     data = df[, c("y", "a", confounder_names)],
  #                     dist = "negbin")
  } else message("model must be `logistic` or `negbin`")
  return(model)
}

get_all_models <- function(df){
  logistic_state <- get_models(df, "logistic", c("State_Name", quantitative_confounders))
  logistic_censusdiv <- get_models(df, "logistic", c("census_division_number", quantitative_confounders))
  negbin_state <- get_models(df, "negbin", c("State_Name", quantitative_confounders))
  negbin_censusdiv <- get_models(df, "negbin", c("census_division_number", quantitative_confounders))
  # zinb_state <- get_models(df, "zinb", c("State_Name", quantitative_confounders))
  # zinb_censusdiv <- get_models(df, "zinb", c("census_division_number", quantitative_confounders))
  
  return(list(logistic_state, logistic_censusdiv, negbin_state, negbin_censusdiv))
}

get_all_models_with_urban_rural <- function(df){
  logistic_state <- get_models(df, "logistic", c("State_Name", quantitative_confounders, "urban_rural"))
  logistic_censusdiv <- get_models(df, "logistic", c("census_division_number", quantitative_confounders, "urban_rural"))
  negbin_state <- get_models(df, "negbin", c("State_Name", quantitative_confounders, "urban_rural"))
  negbin_censusdiv <- get_models(df, "negbin", c("census_division_number", quantitative_confounders, "urban_rural"))
  return(list(logistic_state, logistic_censusdiv, negbin_state, negbin_censusdiv))
}

# get_logistic_results <- function(df, confounder_names){
#   logistic_model <- glm(y ~ ., 
#                         data = df[, c("y", "a", confounder_names)], 
#                         family = "binomial")
#   return(summary(logistic_model)$coefficients)
# }
# 
# get_zibn_results <- function(df, confounder_names){
#   zinb_model <- zeroinfl(y ~ a | a,
#                    dist = "negbin",
#                    data = df[, c("y", "a", confounder_names)])
#   return(summary(zinb_model)$coefficients)
# }

get_coefs <- function(model, var_names){
  coefs <- summary(model)$coefficients
  coefs_subset <- coefs[var_names, ]
  coefs_codified <- apply(coefs_subset, 1, concatenate_results)
  return(coefs_codified)
}

# get_dispersion <- function(model, n){
#   ## p should not have +1 for logistic
#   p <- length(coef(model)) + 1 # +1 for variance (if negbin) or theta (if zero-inflated negbin)
#   dispersion <- sum(resid(model, type = "pearson")^2) / (n - p)
#   return(round(dispersion, 4))
# }

# get_key_logistic_results <- function(df, var_names = c("a", "total_crime_2021", "total_population_2020", "mental_health_index", "prop_hunted_with_shotgun_2021", "log_median_hh_income")){
#   logistic_results <- get_logistic_results(df)
#   logistic_results_subset <- logistic_results[var_names, ]
#   logistic_results_codified <- apply(logistic_results_subset, 1, concatenate_results)
#   return(logistic_results_codified)
# }

# get_n_n0_n1 <- function(exposure_vec){
#   n <- length(exposure_vec)
#   
#   if (length(unique(exposure_vec)) == 2){ # if exposure is binary
#     n0 <- sum(exposure_vec == 0)
#     n1 <- n - n0
#   } else{ # if exposure is continuous
#     n0 <- "N/A"
#     n1 <- "N/A"
#   }
#   
#   n_vec <- c(n, n0, n1)
#   names(n_vec) <- c("n", "n_0", "n_1")
#   return(n_vec)
# }

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

# # table for below 1st quartile of prop_black_only
# belowQ1black_models <- get_all_models(belowQ1black)
# belowQ1black_coefs <- sapply(belowQ1black_models, get_coefs, var_names = var_names_for_table)
# belowQ1black_dispersion <- sapply(belowQ1black_models, get_dispersion, n = nrow(belowQ1black))
# belowQ1black_AIC <- sapply(belowQ1black_models, function(model) round(summary(model)$aic, 1))
# belowQ1black_table <- rbind(belowQ1black_coefs, belowQ1black_dispersion, belowQ1black_AIC, "n" = nrow(belowQ1black))
# colnames(belowQ1black_table) <- model_names
# xtable(belowQ1black_table)
# 
# # table for above 3rd quartile of prop_black_only
# aboveQ3black_models <- get_all_models(aboveQ3black)
# aboveQ3black_coefs <- sapply(aboveQ3black_models, get_coefs, var_names = var_names_for_table)
# aboveQ3black_dispersion <- sapply(aboveQ3black_models, get_dispersion, n = nrow(aboveQ3black))
# aboveQ3black_AIC <- sapply(aboveQ3black_models, function(model) round(summary(model)$aic, 1))
# aboveQ3black_table <- rbind(aboveQ3black_coefs, aboveQ3black_dispersion, aboveQ3black_AIC, "n" = nrow(aboveQ3black))
# colnames(aboveQ3black_table) <- model_names
# xtable(aboveQ3black_table)



# all_analyses_results <- sapply(list(data_analysis, data_trim_exposure,
#                                     data_binary_exposure1km, data_binary_exposure1.609km, data_binary_exposure2km, data_binary_exposure3.218km),
#                                get_key_logistic_results)
# all_analyses_n_n0_n1 <- sapply(list(data_analysis$a, data_trim_exposure$a,
#                                     data_binary_exposure1km$a, data_binary_exposure1.609km$a, data_binary_exposure2km$a, data_binary_exposure3.218km$a),
#                                get_n_n0_n1)
# all_analyses_table <- rbind(all_analyses_results, all_analyses_n_n0_n1)
# colnames(all_analyses_table) <- c("Continuous", "Continuous trimmed", paste("geq", c("1 km", "1 mi", "2 km", "2 mi")))


# Convert to LaTeX
# xtable(all_analyses_table)

