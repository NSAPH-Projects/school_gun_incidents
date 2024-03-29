##### Constants and functions to set up analysis #####

covariates_list = list() # to do: should rename this variable to quant_covariates_list
covariates_list[["demographic"]] <- c("populationtotals_TOTPOP20",
                                      "populationtotals_DPOP_CY",
                                      "pop_institutionalized_groupquarters",
                                      "housing_per_100sqmi",
                                      "schools_per_100sqmi",
                                      "area_sq_miles",                         
                                      "percent_adult")
covariates_list[["socioeconomic"]] <- c("log_med_HH_income",
                                        "log_med_HH_income_15to24",
                                        "householdincome_ACSSNAP_P",
                                        "householdincome_ACSPUBAI_P",
                                        "households_ACSHHBPOV_P",
                                        "EmploymentUnemployment_UNEMP_CY_P",
                                        "EmploymentUnemployment_UNAGE16CY_P",
                                        "vehiclesavailable_ACSOVEH0_P",
                                        "educationalattainment_ACSBACHDEG_P",
                                        "educationalattainment_ACSMASTDEG_P")
covariates_list[["mental_health"]] <- c("mean_depression",
                                     "mean_distress")
covariates_list[["crime"]] <- c("crime_CRMCYTOTC")
covariates_list[["gun_affinity"]] <- c("firearm_retailers_per_100sqmi",
                                       "sports_MP33018a_B_P",
                                       "CompositeIndex2014to2021")
covariates_list[["racioethnic"]] <- c("raceandhispanicorigin_WHITE20_P",      "raceandhispanicorigin_BLACK20_P",
                                      "raceandhispanicorigin_ASIAN20_P",      "raceandhispanicorigin_RACE2UP20_P",
                                      "hispanicorigin_HISPPOP20_P")
quantitative_covariates <- unlist(covariates_list)

quant_covars_full_names <- c("Total population",
                             "Daytime population",
                             "Population in institutionalized group quarters",
                             "Housing units per 100 square miles",
                             "Schools per 100 square miles",
                             "Census tract area in square miles",  
                             "Percent aged 18+",
                             "Log median household income (log $)",
                             "Log median household income, ages 16-24 (log $)",
                             "Percent of households receiving food stamps/SNAP",
                             "Percent of households with public assistance income",
                             "Percent of households below poverty line",
                             "Percent unemployed",
                             "Percent unemployed, ages 16-24",
                             "Percent without a vehicle",
                             "Percent with bachelor's degree, ages 25+",
                             "Percent with master's degree, ages 25+",
                             "Depression, crude prevalence (%)",
                             "Frequent mental health distress, crude prevalence (%)",
                             "Total crime index",
                             "Firearms retailers per 100 square miles",
                             "Percent who hunted with a shotgun",
                             "Composite index of state firearms laws",
                             "Percent white",
                             "Percent Black",
                             "Percent Asian",
                             "Percent multiracial",
                             "Percent Hispanic/Latino")

categorical_covariates <- c("State_Name", "urbanicity")
cat_covars_full_names <- c("State", "Urbanicity")

# Data Sources
# 1 = National Center for Education Statistics (NCES) Education Demographic and Geographic Estimate (EDGE) database
# 2 = Listing of Federal Firearms Licensees (FFL)
# 3 = K-12 School Shooting Database
# 4 = U.S. Public Law 94-171
# 5 = U.S. Centers for Disease Control and Prevention (CDC) PLACES dataset
# 6 = RAND State Firearm Law Database
# 7 = 2013 National Center for Health Statistics (NCHS) Urban-Rural Classification Scheme for Counties
data_outcome_and_exposure_sources <- c("1", "3", "1,2")
quant_covars_sources <- c("4", "4", "4", "4",
                          "1",
                          rep("4", 2 +
                                length(covariates_list[["socioeconomic"]])),
                          "5", "5", # mental health
                          "4", # crime rate
                          "2", "4", "6", # gun affinity
                          rep("4", length(covariates_list[["racioethnic"]])))
cat_covars_sources <- c("", "7")


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
  if ("urbanity" %in% colnames(data)){
    data$urbanity <- as.factor(data$urbanity)
  }
  if ("urbanicity" %in% colnames(data)){
    data$urbanicity <- as.factor(data$urbanicity)
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
  y <- data[, "SGI"]
  a <- data[, exposure]
  x <- factorize_cat_vars(subset(data, select = covariate_names))
  new_df <- cbind(y, a, x)
  new_df <- na.omit(new_df)
  return(new_df)
}