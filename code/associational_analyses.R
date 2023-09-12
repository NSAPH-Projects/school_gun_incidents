## Load packages ----
library(MASS)
library(data.table)
library(argparse)

# Define parser arguments ----
parser <- ArgumentParser()
parser$add_argument("-e", "--exposure", default="mean_distance_all_persistent_dealers",
                    help="Exposure variable 'mean_distance_all_persistent_dealers' or 'mean_dist_commercial_dealers'", type="character")
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
df <- fread(paste0(dir, "data/intermediate/final_data_aug2023.csv"))

## Get datasets for main analysis ----
data <- vector("list", 2)
names(data) <- c("mean_distance_all_persistent_dealers", "mean_dist_commercial_dealers")
data[["mean_distance_all_persistent_dealers"]] <- vector("list", 2)
data[["mean_dist_commercial_dealers"]] <- vector("list", 2)
names(data[["mean_distance_all_persistent_dealers"]]) <- c("state", "state.urbanity")
names(data[["mean_dist_commercial_dealers"]]) <- c("state", "state.urbanity")

data[["mean_distance_all_persistent_dealers"]][["state"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("STATE_ABBR", quantitative_covariates))
data[["mean_distance_all_persistent_dealers"]][["state.urbanity"]] <- get_analysis_df(df, "mean_distance_all_persistent_dealers", c("STATE_ABBR", "urban_rural", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("STATE_ABBR", quantitative_covariates))
data[["mean_dist_commercial_dealers"]][["state.urbanity"]] <- get_analysis_df(df, "mean_dist_commercial_dealers", c("STATE_ABBR", "urban_rural", quantitative_covariates))

## Get 95th and 99th percentiles of exposure ----
percentile_exposure <- vector("list", 2)
names(percentile_exposure) <- c("mean_distance_all_persistent_dealers", "mean_dist_commercial_dealers")
percentile_exposure[["mean_distance_all_persistent_dealers"]] <- vector("list", 2)
percentile_exposure[["mean_dist_commercial_dealers"]] <- vector("list", 2)
names(percentile_exposure[["mean_distance_all_persistent_dealers"]]) <- c("5.95", "1.99")
names(percentile_exposure[["mean_dist_commercial_dealers"]]) <- c("5.95", "1.99")

percentile_exposure[["mean_distance_all_persistent_dealers"]][["5.95"]] <- quantile(data[["mean_distance_all_persistent_dealers"]][["state"]]$a, c(0.05, 0.95))
percentile_exposure[["mean_distance_all_persistent_dealers"]][["1.99"]] <- quantile(data[["mean_distance_all_persistent_dealers"]][["state"]]$a, c(0.01, 0.99))
percentile_exposure[["mean_dist_commercial_dealers"]][["5.95"]] <- quantile(data[["mean_dist_commercial_dealers"]][["state"]]$a, c(0.05, 0.95))
percentile_exposure[["mean_dist_commercial_dealers"]][["1.99"]] <- quantile(data[["mean_dist_commercial_dealers"]][["state"]]$a, c(0.01, 0.99))

#### #### ####
## Run baseline models
#### #### ####

m_ = c("naivelogistic" = "logistic", "naivenegbin" = "negbin")[args$m]
data_ = data[[args$e]][[args$s]][data[[args$e]][[args$s]]$a >= percentile_exposure[[args$e]][[args$p]][1] & 
                        data[[args$e]][[args$s]]$a <= percentile_exposure[[args$e]][[args$p]][2], ]
covars_ = list(
  "state" = c("STATE_ABBR", quantitative_covariates),
  "state.urbanity" = c("STATE_ABBR", "urban_rural", quantitative_covariates)
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

cat(args$e, args$m, args$s, args$p, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"), 
    append=TRUE)
cat("Effect: ", 
    effect, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("95% CI lower bound: ", 
    lb_95ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("95% CI upper bound: ", 
    ub_95ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("90% CI lower bound: ", 
    lb_90ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)
cat("90% CI upper bound: ", 
    ub_90ci, 
    sep = "\n",
    file=paste0(dir, "results/associational_analyses/", 
                args$e, ".", args$m, ".", args$s, ".", args$p, ".txt"),
    append=TRUE)