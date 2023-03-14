# This file contains functions (and a couple constants) used in our causal analysis

##### Functions to set up analysis #####

quantitative_covariates <- c("total_population_2020", "housing_units_per_100_sq_miles", "area_sq_miles",
                          "log_median_hh_income", "schools_per_100_sq_miles",
                          "log_median_hh_income_15to24", "total_crime_2021", 
                          "dealers_per_100_sq_miles", "mental_health_index",
                          "daytime_pop_2021", "prop_white_only", "prop_black_only", 
                          "prop_asian_only", "prop_multiracial", "prop_hispanic_latino", 
                          "prop_food_stamps_2019", "prop_public_assist_income_2019",
                          "prop_below_poverty_2019", "prop_without_vehicles_2019",
                          "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021", 
                          "prop_grad_deg_25plus_2021", "prop_unemployed_2021",
                          "prop_unemployed_16to24_2021", "prop_institutional_group",
                          "prop_noninstitutional_group", "prop_18plus")

factorize_cat_vars <- function(data){
  if ("census_division" %in% colnames(data)){
    data$census_division <- as.factor(data$census_division)
  }
  if ("census_division_number" %in% colnames(data)){
    data$census_division_number <- as.factor(data$census_division_number)
  }
  if ("census_region" %in% colnames(data)){
    data$census_region <- as.factor(data$census_region)
  }
  if ("GEOID" %in% colnames(data)){
    data$GEOID <- as.factor(data$GEOID)
  }
  if ("State_Name" %in% colnames(data)){
    data$State_Name <- as.factor(data$State_Name)
  }
  if ("County_Name" %in% colnames(data)){
    data$County_Name <- as.factor(data$County_Name)
  }
  if ("urban_rural" %in% colnames(data)){
    data$urban_rural <- as.factor(data$urban_rural)
  }
  if ("state_fips" %in% colnames(data)){
    data$state_fips <- ifelse(nchar(data$state_fips) == 1, paste0("0", data$state_fips), data$state_fips)
  }
  if ("county_fips" %in% colnames(data)){
    data$county_fips <- ifelse(nchar(data$county_fips) == 1, paste0("0", data$county_fips), data$county_fips)
  }
  return(data)
}

get_analysis_df <- function(data, exposure = "mean_total_miles", covariate_names){
  data <- as.data.frame(data)
  y <- data[, "binary_shooting_incident"]
  a <- data[, exposure]
  x <- factorize_cat_vars(data[, covariate_names])
  a <- a / 0.5 # convert exposure to half-miles
  new_df <- cbind(y, a, x)
  new_df <- na.omit(new_df)
  return(new_df)
}

## Functions to get regression results ----

get_models <- function(df, model = "logistic", covariate_names){
  if (model == "logistic"){
    model <- glm(y ~ ., 
                 data = df[, c("y", "a", covariate_names)], 
                 family = "binomial")
  } else if (model == "negbin"){
    model <- glm.nb(y ~ .,
                    data = df[, c("y", "a", covariate_names)])
  } else message("model must be `logistic` or `negbin`")
  return(model)
}

## Correlation functions ----

abs_pt_biserial_cor <- function(w, binary_cov){
  return(abs(cor(w, binary_cov)))
}

# Calculate mean of absolute point-biserial correlation between continuous exposure and each binary indicator for an unordered categorical covariate 
# params: w is the vector of continuous exposure, unordered_var is vector of unordered categorical covariate 
cor_unordered_var <- function(w, unordered_var){
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  abs_cor_pb <- lapply(binary_indicators, abs_pt_biserial_cor, w = w)
  return(mean(unlist(abs_cor_pb)))
}

weighted_cor_unordered_var <- function(w, unordered_var, weights){
  library(wCorr)
  
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  weighted_cor <- lapply(binary_indicators, weightedCorr, y = w, method = "Pearson", weights = weights)
  abs_weighted_cor <- lapply(weighted_cor, abs)
  return(mean(unlist(abs_weighted_cor)))
}

make_correlation_plot <- function(abs_cor_table){
  ggplot(abs_cor_table, aes(x = `Absolute Correlation`, y = Covariate, color = Dataset, group = Dataset)) +
    geom_point() +
    geom_line(orientation = "y")
}

# make_correlation_plot2 <- function(pseudo_pop, ci, cat_confounder_names, w_orig, unordered_vars_orig){
#   if (ci == "matching"){
#     abs_cor <- get_matched_correlation_plot(pseudo_pop, cat_confounder_names, w_orig, unordered_vars_orig)
#   } else if (ci == "weighting"){
#     abs_cor <- get_weighted_correlation_plot(pseudo_pop, cat_confounder_names, w_orig, unordered_vars_orig)
#   }
#   p <- ggplot(abs_cor, aes(x = `Absolute Correlation`, y = Covariate, color = Dataset, group = Dataset)) +
#     geom_point() +
#     geom_line(orientation = "y")
#   return(p)
# }
