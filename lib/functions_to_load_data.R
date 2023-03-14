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