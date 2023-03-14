## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)

# define parser arguements ----
parser <- ArgumentParser()

parser$add_argument("-c", "--covariate_name",
                    help="Covariates 'demographic' or 'socioeconomic' or 'gun_affinity' or 'racioethnic'", 
                    type="character")
args = parser$parse_args()

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## Main body ----

## Classify covariates (for exclusion, to calculate resulting e-values) ----

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

covariates_list = list()
covariates_list[["demographic"]] <- c("total_population_2020", "daytime_pop_2021", "housing_units_per_100_sq_miles", "schools_per_100_sq_miles",
                                      "area_sq_miles", "prop_institutional_group", "prop_noninstitutional_group", "prop_18plus")
covariates_list[["socioeconomic"]] <- c("log_median_hh_income", "log_median_hh_income_15to24", "prop_food_stamps_2019", "prop_public_assist_income_2019",
                                        "prop_below_poverty_2019", "prop_unemployed_2021", "prop_unemployed_16to24_2021", "total_crime_2021",
                                        "mental_health_index", "prop_without_vehicles_2019", "prop_bachelor_deg_25plus_2021", "prop_grad_deg_25plus_2021")
covariates_list[["gun_affinity"]] <- c("dealers_per_100_sq_miles", "prop_hunted_with_shotgun_2021")
covariates_list[["racioethnic"]] <- c("prop_white_only", "prop_black_only", "prop_asian_only", "prop_multiracial", "prop_hispanic_latino")

covariate = args$covariate_name
print(covariate)

## Get data, excluding classes of covariates ----
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates))
data_without_covariate <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates[!(quantitative_covariates %in% covariates_list[[covariate]])]))

## GPS matching ----

# get GPS matching results excluding classes of covariates
# state.5.95_match <- all_matching_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
match_without_covariate <- all_matching_results_1model(100, data_without_covariate, c(0.05, 0.95), "State_Name",
                                                       quantitative_covariates[!(quantitative_covariates %in% covariates_list[[covariate]])])

# check covariate balance for GPS-matched pseudopopulations
ggsave(paste0(dir, "results/e_value/", covariate, "_correlation_plot.png"),
       make_correlation_plot(match_without_covariate$cov_bal.capped0.99))

# get logistic regression results from GPS-matched pseudopopulations
match_without_covariate$logistic_regression_estimated_odds
