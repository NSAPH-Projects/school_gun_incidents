## Load packages ----

library(ggplot2)
library(tidyr)
library(data.table)
library(argparse)


# define parser arguments ----

parser <- ArgumentParser()

parser$add_argument("-c", "--covariate_name",
                    help="Covariates 'demographic' or 'socioeconomic' or 'gun_affinity' or 'racioethnic'", 
                    type="character")
args = parser$parse_args()


## Load functions ----

dir <- "../../" # repository folder; this file is in code/sensitivity_analyses

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_measure_covariate_balance.R"))
source(paste0(dir, "lib/functions_using_gps.R"))


## Load datasets ----

df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))


## Main body ----

## Get data, excluding classes of covariates ----
data_without_covariate <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates[!(quantitative_covariates %in% covariates_list[[args$covariate_name]])]))

## GPS matching ----

# get GPS matching results excluding classes of covariates
match_without_covariate <- all_matching_results_1model(seed = 100,
                                                       data = data_without_covariate,
                                                       trim = c(0.05, 0.95),
                                                       cat_covariate_names = "State_Name",
                                                       run_gee_model = F,
                                                       quant_covariates = quantitative_covariates[!(quantitative_covariates %in% covariates_list[[args$covariate_name]])])

# check covariate balance for GPS-matched pseudopopulations and save as png
ggsave(paste0(dir, "results/sensitivity_analyses/e_value/", args$covariate_name, "_correlation_plot.png"),
       make_correlation_plot(match_without_covariate$cov_bal.capped0.99))

# get logistic regression results from GPS-matched pseudopopulations and save results as txt file
cat(paste("Excluded covariate:", args$covariate_name, "\n",
          "Logistic regression estimated odds:", match_without_covariate$logistic_regression_estimated_odds),
    sep = "\n",
    file=paste0(dir, "results/sensitivity_analyses/e_value/gps_matching_without_", args$covariate_name,"_variables.txt"),
    append=TRUE)
