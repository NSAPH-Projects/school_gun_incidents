## Load packages ----

library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)


# define parser arguments ----

parser <- ArgumentParser()

parser$add_argument("-c", "--covariate_name",
                    help="Covariates 'demographic' or 'socioeconomic' or 'mental_health' or 'crime' or 'gun_affinity' or 'racioethnic'", 
                    type="character")
args = parser$parse_args()


## Load functions ----

dir <- "../../" # repository folder; this file is in code/sensitivity_analyses

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))


## Load datasets ----

df <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))


## Main body ----

## Get data, excluding classes of covariates ----
data_without_covariate <- get_analysis_df(df, "mean_dist_commercial_dealers", c(categorical_covariates, quantitative_covariates[!(quantitative_covariates %in% covariates_list[[args$covariate_name]])]))

## GPS matching ----

# get GPS matching results excluding classes of covariates
match_without_covariate <- all_matching_results_1model(seed = 100,
                                                       data = data_without_covariate,
                                                       trim = c(0.05, 0.95),
                                                       cat_covariate_names = categorical_covariates,
                                                       run_gee_model = F,
                                                       quant_covariates = quantitative_covariates[!(quantitative_covariates %in% covariates_list[[args$covariate_name]])])

# check covariate balance for GPS-matched pseudopopulations and save as png
ggsave(paste0(dir, "results/sensitivity_analyses/e_value/", args$covariate_name, "_correlation_plot.png"),
       make_correlation_plot(match_without_covariate$cov_bal.capped0.99))

# get logistic regression results from GPS-matched pseudopopulations and save results as csv file
results_as_table <- data.table(Exposure = "mean_dist_commercial_dealers",
                               Model = "Matching without covariates for e-value",
                               Cat_Confounder = "state.urbanicity",
                               Excluded_Confounder_Class = args$covariate_name,
                               Trim = "5.95",
                               Effect = match_without_covariate$logistic_regression_estimated_odds,
                               CI_95ct_lower = NA,
                               CI_95ct_upper = NA,
                               CI_90ct_lower = NA,
                               CI_90ct_upper = NA,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
fwrite(results_as_table, file = paste0(dir, "results/sensitivity_analyses/e_value/gps_matching_without_", args$covariate_name, "_variables.csv"))
