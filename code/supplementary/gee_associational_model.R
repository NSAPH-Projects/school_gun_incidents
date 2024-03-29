##### Note: GEE model may take up to 96 GB to run #####

## Install packages ----
if (!("gee" %in% rownames(installed.packages()))) install.packages("gee")
if (!("MASS" %in% rownames(installed.packages()))) install.packages("MASS")
if (!("data.table" %in% rownames(installed.packages()))) install.packages("data.table")

## Load packages ----
library(gee)
library(MASS)
library(data.table)

## Filepaths ----
dir <- paste0(here::here(), "/") # repository folder; this file is in code/sensitivity_analyses
results_path <- paste0(dir, "results/sensitivity_analyses/gee_associational_model/")

## Load functions ----
source(paste0(dir, "lib/functions_to_load_data.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))
data_main_analysis <- get_analysis_df(df, "mean_dist_commercial_dealers", c(categorical_covariates, quantitative_covariates))
exposure5.95 <- quantile(data_main_analysis$a, c(0.05, 0.95))
data_trimmed <- data_main_analysis[data_main_analysis$a >= exposure5.95[1] &
                                     data_main_analysis$a <= exposure5.95[2], ]

## Check for spatial confounding ----

# function to run GEE model, using state as cluster and exchangeability correlation structure
gee_model <- function(df){
  data_contiguous_clusters <- df[order(df$State_Name), ]
  outcome <- gee(formula = y ~ .,
                 family = "binomial",
                 data = data_contiguous_clusters[, c("y", "a", "urbanicity", quantitative_covariates)],
                 id = data_contiguous_clusters$State_Name,
                 corstr = "exchangeable")
  return(summary(outcome)$coefficients)
}

# run GEE model on main analysis (commercial exposure trimmed at 5th and 95th exposures)
gee_results <- gee_model(data_trimmed)

# save results as csv file
results_as_table <- data.table(Exposure = "mean_dist_commercial_dealers",
                               Model = "GEE associational model",
                               Cat_Confounder = "state.urbanicity",
                               Trim = "5.95",
                               Effect = round(exp(gee_results["a",]["Estimate"]), 4),
                               CI_95ct_lower = round(exp(gee_results["a",]["Estimate"] - 1.96 * gee_results["a",]["Robust S.E."]), 4),
                               CI_95ct_upper = round(exp(gee_results["a",]["Estimate"] + 1.96 * gee_results["a",]["Robust S.E."]), 4),
                               CI_90ct_lower = round(exp(gee_results["a",]["Estimate"] - 1.645 * gee_results["a",]["Robust S.E."]), 4),
                               CI_90ct_upper = round(exp(gee_results["a",]["Estimate"] + 1.645 * gee_results["a",]["Robust S.E."]), 4),
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
print(results_as_table)
if (!dir.exists(results_path)) dir.create(results_path, recursive = T)
fwrite(results_as_table, file = paste0(results_path, "gee_associational_model.csv"))
