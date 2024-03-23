##### Note: GEE model (line 65) may take up to 96 GB for trim 5/95, or up to 250 GB for trim 1/99, to run #####

## Load packages ----
library(ggplot2)
library(tidyr)
library(data.table)
library(argparse, lib.loc = "../../../R/4.3/")

library(logger, lib.loc = "../../../R/4.3/")
library(CausalGPS, lib.loc = "../../../R/4.3/")
library(findpython, lib.loc = "../../../R/4.3/")
library(wCorr, lib.loc = "../../../R/4.3/")
library(SuperLearner, lib.loc = "../../../R/4.3/")
library(gee, lib.loc = "../../../R/4.3/")
library(Ecume, lib.loc = "../../../R/4.3/")
library(gnm, lib.loc = "../../../R/4.3/")

# Define parser arguments ----
parser <- ArgumentParser()
parser$add_argument("-e", "--exposure", default="mean_distance_all_persistent_dealers",
                    help="Exposure variable 'mean_distance_all_persistent_dealers' or 'mean_dist_commercial_dealers'", type="character")
parser$add_argument("-sd", "--seed", default=100,
                    help="seed value", type="integer")
parser$add_argument("-s", "--sensitivity_analysis", default="state",
                    help="Sensitivity analysis 'state' or 'state.urbanicity'", type="character")
parser$add_argument("-p", "--percentiles", default="5.95",
                    help="Percentiles of exposure '5.95' or '1.99'", type="character") 
args = parser$parse_args()


## Load functions ----
dir <- here::here() # location of repository

source(here::here(dir, "lib/functions_to_load_data.R"))
source(here::here(dir, "lib/functions_to_measure_covariate_balance.R"))
source(here::here(dir, "lib/functions_using_gps.R"))

## Load datasets ----
df <- fread(here::here(dir, "data/intermediate/final_data_sep2023.csv"))

## Get datasets for main analysis ----
data <- vector("list", 2)
names(data) <- c("mean_distance_all_persistent_dealers", "mean_dist_commercial_dealers")
data[["mean_distance_all_persistent_dealers"]] <- vector("list", 2)
data[["mean_dist_commercial_dealers"]] <- vector("list", 2)
names(data[["mean_distance_all_persistent_dealers"]]) <- c("state", "state.urbanicity")
names(data[["mean_dist_commercial_dealers"]]) <- c("state", "state.urbanicity")

data[["mean_distance_all_persistent_dealers"]][["state"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("State_Name", quantitative_covariates))
data[["mean_distance_all_persistent_dealers"]][["state.urbanicity"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("State_Name", "urbanicity", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state.urbanicity"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("State_Name", "urbanicity", quantitative_covariates))


## Set up directories and filepaths for causal analysis ----
seed_ = args$seed
trim_ = list("5.95"=c(0.05, 0.95), "1.99"=c(0.01, 0.99))[[args$percentiles]]
data_ = data[[args$exposure]][[args$sensitivity_analysis]]
covars_ = list("state"="State_Name", "state.urbanicity"=c("State_Name", "urbanicity"))[[args$sensitivity_analysis]]

main_causal_dir <- here::here(dir, "results/causal_analyses")
if (!dir.exists(main_causal_dir)) dir.create(main_causal_dir, recursive = T)


## Matching CausalGPS  ----

results_match <- all_matching_results_1model(
  seed_,
  data_,
  trim_,
  covars_,
  run_gee_model = F
)

var_arg_a_p_match = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_match")

# save covariate balance as csv and plot as png
fwrite(results_match$cov_bal.capped0.99, here::here(main_causal_dir, paste0(var_arg_a_p_match, "_correlation.csv")))
ggsave(here::here(main_causal_dir, paste0(var_arg_a_p_match, "_correlation_plot.png")),
       make_correlation_plot(results_match$cov_bal.capped0.99))

# get mean and max AC of matched (number of matches capped at 99th percentile) and unadjusted pseudopopulation
results_match$mean_AC_matched <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
)
results_match$mean_AC_unadjusted <- mean(
  results_match$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_match$max_AC_matched <- max(
  results_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]
)
results_match$max_AC_unadjusted <- max(
  results_match$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_match[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_match[["cov_bal.capped1"]] <- NULL

# save results as csv file
results_as_table <- data.table(Exposure = args$e,
                               Model = "Match",
                               Cat_Confounder = args$s,
                               Trim = args$p,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
results_as_table <- cbind(results_as_table, as.data.table(results_match))
fwrite(results_as_table, here::here(main_causal_dir, paste0(var_arg_a_p_match, ".csv")))


## Weighted CausalGPS ----

var_arg_a_p_weight = paste0(args$exposure, ".", args$sensitivity_analysis, ".", args$percentiles,"_weight")

results_weight <- all_weighting_results_1model(
  seed_,
  data_,
  trim_,
  covars_
)

# save covariate balance as csv and plot as png
fwrite(results_weight$cov_bal.capped0.99, here::here(main_causal_dir, paste0(var_arg_a_p_weight, "_correlation.csv")))
ggsave(here::here(main_causal_dir, paste0(var_arg_a_p_weight, "_correlation_plot.png")),
       make_correlation_plot(results_weight$cov_bal.capped0.99))

# get mean and max AC of weighted (capped at 99th percentile) and unadjusted pseudopopulation
results_weight$mean_AC_weighted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
)
results_weight$mean_AC_unadjusted <- mean(
  results_weight$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_weight$max_AC_weighted <- max(
  results_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]
)
results_weight$max_AC_unadjusted <- max(
  results_weight$cov_bal.capped0.99[Dataset == "Unadjusted", `Absolute Correlation`]
)
results_weight[["cov_bal.capped0.99"]] <- NULL

# remove results from uncapped pseudopopulation (not used)
results_weight[["cov_bal.capped1"]] <- NULL

# save results as csv file
results_as_table <- data.table(Exposure = args$e,
                               Model = "Weight",
                               Cat_Confounder = args$s,
                               Trim = args$p,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
results_as_table <- cbind(results_as_table, as.data.table(results_weight))
fwrite(results_as_table, here::here(main_causal_dir, paste0(var_arg_a_p_weight, ".csv")))
