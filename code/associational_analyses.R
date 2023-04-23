## Load packages ----
library(MASS)
library(data.table)
library(argparse)

# define parser arguments ----
parser <- ArgumentParser()
parser$add_argument("-m", "--model", default="naivelogistic",
                    help="Model to run 'naivelogistic' or 'naivenegbin'", type="character")
parser$add_argument("-s", "--sensitivity_analysis", default="state",
                    help="Sensitivity analysis 'state' or 'state.urbanity'", type="character")
parser$add_argument("-p", "--percentiles", default="5.95",
                    help="Percentiles of exposure '5.95' or '1.99'", type="character")                    
args = parser$parse_args()

## Load functions ----
dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R"))
source(paste0(dir, "lib/functions_to_get_associational_models.R"))

## Load datasets ----
df <- fread(paste0(dir, "data/intermediate/all_tracts_2020_subset_vars_revised.csv"))

## prepare datasets for main analysis ----
data <- list()
data[["state"]] <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_covariates))
data[["state.urbanity"]] <- get_analysis_df(df, "mean_total_miles", c("State_Name", "urban_rural", quantitative_covariates))

## get 95th and 99th percentiles of exposure ----
percentile_exposure <- list()
percentile_exposure[["1.99"]] <- quantile(data[["state"]]$a, c(0.01, 0.99))
percentile_exposure[["5.95"]] <- quantile(data[["state"]]$a, c(0.05, 0.95))

#### #### ####
## Run baseline models
#### #### ####

m_ = c("naivelogistic" = "logistic", "naivenegbin" = "negbin")[args$m]
data_ = data[[args$s]][data[[args$s]]$a >= percentile_exposure[[args$p]][1] & 
                        data[[args$s]]$a <= percentile_exposure[[args$p]][2], ]
covars_ = list(
  "state" = c("State_Name", quantitative_covariates),
  "state.urbanity" = c("State_Name", "urban_rural", quantitative_covariates)
)[[args$s]]

model <- get_models(
  data_, 
  model = m_, 
  covariate_names = covars_
  )

results <- summary(model)$coefficients["a",]
effect <- round(exp(results["Estimate"]), 4)
lb_95ci <- round(exp( results["Estimate"] - 1.96 * results["Std. Error"]), 4)
ub_95ci <- round(exp( results["Estimate"] + 1.96 * results["Std. Error"]), 4)
lb_90ci <- round(exp( results["Estimate"] - 1.645 * results["Std. Error"]), 4)
ub_90ci <- round(exp( results["Estimate"] + 1.645 * results["Std. Error"]), 4)

cat("Baseline model", 
    args$m, args$s, args$p, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"), 
    append=TRUE)
cat("Effect: ", 
    effect, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("95% CI lower bound: ", 
    lb_95ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("95% CI upper bound: ", 
    ub_95ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("90% CI lower bound: ", 
    lb_90ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("90% CI upper bound: ", 
    ub_90ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)