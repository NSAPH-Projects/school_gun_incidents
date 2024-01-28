## Load packages ----
library(MASS)
library(data.table)
# library(argparse)
# 
# # Define parser arguments ----
# parser <- ArgumentParser()
# parser$add_argument("-e", "--exposure", default="mean_distance_all_persistent_dealers",
#                     help="Exposure variable 'mean_distance_all_persistent_dealers' or 'mean_dist_commercial_dealers'", type="character")
# parser$add_argument("-m", "--model", default="naivelogistic",
#                     help="Model to run 'naivelogistic' or 'naivenegbin'", type="character")
# parser$add_argument("-s", "--sensitivity_analysis", default="state",
#                     help="Sensitivity analysis 'state' or 'state.urbanicity'", type="character")
# parser$add_argument("-p", "--percentiles", default="5.95",
#                     help="Percentiles of exposure '5.95' or '1.99'", type="character")                    
# args = parser$parse_args()

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_get_associational_models.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/sensitivity_analyses/intermediate/final_data_sep2023.csv"))

## Get dataset for main analysis ----
data <- get_analysis_df(df, "mean_euclidean_dist_commercial_dealers", c(categorical_covariates, quantitative_covariates))

## Get 95th and 99th percentiles of exposure ----
percentile_exposure <- quantile(data$a, c(0.05, 0.95))

# to do: args$m


#### #### ####
## Run baseline models
#### #### ####

# get data
m_ = c("naivelogistic" = "logistic", "naivenegbin" = "negbin")[args$m]
data_ = data[data$a >= percentile_exposure[1] & 
                        data$a <= percentile_exposure[2], ]
covars_ = c(categorical_covariates, quantitative_covariates)

# get model
model <- get_models(
  data_, 
  model = m_, 
  covariate_names = covars_
  )

# get results
results <- summary(model)$coefficients["a",]
effect <- round(exp(results["Estimate"]), 4)
lb_95ci <- round(exp( results["Estimate"] - 1.96 * results["Std. Error"]), 4)
ub_95ci <- round(exp( results["Estimate"] + 1.96 * results["Std. Error"]), 4)
lb_90ci <- round(exp( results["Estimate"] - 1.645 * results["Std. Error"]), 4)
ub_90ci <- round(exp( results["Estimate"] + 1.645 * results["Std. Error"]), 4)

# save results as csv file
results_as_table <- data.table(Exposure = "Euclidean Distance to Commercial Dealer",
                               Model = args$m,
                               Cat_Confounder = args$s,
                               Trim = "5.95",
                               Effect = effect,
                               CI_95ct_lower = lb_95ci,
                               CI_95ct_upper = ub_95ci,
                               CI_90ct_lower = lb_90ci,
                               CI_90ct_upper = ub_90ci,
                               Exposure_Unit = "Mile",
                               Effect_Unit = "Odds")
fwrite(results_as_table, file = paste0(dir, "results/sensitivity_analyses/euclidean_distance_to_commercial_dealers/",
                                       "associational_results.csv"))
