## Load packages ----
library(MASS)
library(data.table)

## File paths ----
dir <- paste0(here::here(), "/") # repository path

results_path <- paste0(dir, "results/sensitivity_analyses/euclidean_distance_to_commercial_dealers/")
if (!dir.exists(results_path)) dir.create(results_path, recursive = T)

## Load functions ----
source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_get_associational_models.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/sensitivity_analyses/final_data_sep2023.csv"))

## Get dataset for main analysis ----
data <- get_analysis_df(df, "mean_euclidean_dist_commercial_dealers", c(categorical_covariates, quantitative_covariates))

## Get 95th and 99th percentiles of exposure ----
percentile_exposure <- quantile(data$a, c(0.05, 0.95))


#### #### ####
## Run baseline models
#### #### ####

# get data
data_ = data[data$a >= percentile_exposure[1] & 
                        data$a <= percentile_exposure[2], ]
covars_ = c(categorical_covariates, quantitative_covariates)

# get model
for (m_ in c("logistic", "negbin")){
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
                                 Model = m_,
                                 Cat_Confounder = "state.urbanicity",
                                 Trim = "5.95",
                                 Effect = effect,
                                 CI_95ct_lower = lb_95ci,
                                 CI_95ct_upper = ub_95ci,
                                 CI_90ct_lower = lb_90ci,
                                 CI_90ct_upper = ub_90ci,
                                 Exposure_Unit = "Mile",
                                 Effect_Unit = "Odds")
  fwrite(results_as_table, file = paste0(results_path, "associational_results.csv"),
         append = T)
}
