##### Functions to set up analysis #####

quantitative_covariates <- c("P0010001",                           "populationtotals_DPOP_CY",
                             "hu_per_100_sqmi",                    "schools_per_100_sqmi",
                             "area_sq_mile",                       "groupquarters_GQINST20_P",
                             "prop_adult",                         "householdincome_ACSSNAP_P",
                             "households_ACSPUBAI_P",              "households_ACSHHBPOV_P",
                             "EmploymentUnemployment_UNEMPRT_CY",  "EmploymentUnemployment_UNEMRT16CY",
                             "vehiclesavailable_ACSOVEH0_P",       "crime_CRMCYTOTC",
                             "MHLTH_CrudePrev",                    "DEPRESSION_CrudePrev",
                             "educationalattainment_BACHDEG_CY_P", "educationalattainment_GRADDEG_CY_P",
                             "firearm_retailers_per_100sqmi",      "sports_MP33018a_B_P",
                             "prop_white",                         "prop_black",
                             "prop_asian",                         "prop_multiracial",
                             "prop_hispanic",                      "log_med_HH_income",
                             "log_avg_HH_income_15to24")

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
  if ("STATE_ABBR" %in% colnames(data)){
    data$STATE_ABBR <- as.factor(data$STATE_ABBR)
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

get_analysis_df <- function(data, exposure, covariate_names){
  data <- as.data.frame(data)
  y <- data[, "binary_shooting"]
  a <- data[, exposure]
  x <- factorize_cat_vars(data[, covariate_names])
  a <- a / 0.5 # convert exposure to half-miles
  new_df <- cbind(y, a, x)
  new_df <- na.omit(new_df)
  return(new_df)
}