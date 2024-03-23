## Load packages ----

library(data.table)
library(ggplot2)


## Load filepaths for results ----

dir <- here::here() # location of repository

source(here::here(dir, "lib/functions_to_load_data.R")) # used to get variables' full names

causal_results_paths <- list.files(here::here(dir, "results/causal_analyses/"),
                                   pattern = "correlation.csv",
                                   full.names = T)


## Read in results ----

# note that "exposure" is called "intervention" in the paper
read_one_causal_txt <- function(path){
  results <- fread(path)
  
  # rename variables using full names
  for (i in 1:length(quantitative_covariates)){
    results[Covariate == quantitative_covariates[i], Covariate := quant_covars_full_names[i]]
  }
  results[Covariate == "State_Name", Covariate := "State"]
  if ("urbanicity" %in% results$Covariate){
    results[Covariate == "urbanicity", Covariate := "Urbanicity"]
  }
  
  if (grepl(pattern = "mean_distance_all_persistent_dealers", x = path)){
    results$Exposure <- "Distance to Any Firearms Dealer"
  } else results$Exposure <- "Distance to Commercial Firearms Dealer"
  
  if (grepl(pattern = "5.95", x = path)){
    results$`Exposure Trimming` <- "Data trimmed at\n5th, 95th percentiles"
  } else results$`Exposure Trimming` <- "Data trimmed at\n1st, 99th percentiles"
  
  if (grepl(pattern = "state.urbanicity", x = path)){
    results$`Categorical Confounder(s)` <- "Controlling for state and urbanicity"
  } else results$`Categorical Confounder(s)` <- "Controlling for state"
  
  return(results)
}

# compile all results into one table
causal_results <- rbindlist(lapply(causal_results_paths, read_one_causal_txt))

# remove the analyses that don't use urbanicity as a (categorical) variable
causal_results <- causal_results[`Categorical Confounder(s)` == "Controlling for state and urbanicity"]

# setorder(causal_results, Exposure, Trim, Cat_Confounder) 
# all_dealers_table <- causal_results[Exposure == "mean_distance_all_persistent_dealers"]
# commercial_dealers_table <- causal_results[Exposure == "mean_dist_commercial_dealers"]

# fwrite(x = causal_results, file = here::here(dir, "results/causal_analyses/all_causal_models_covariate_balance.csv"))
# fwrite(x = all_dealers_table, file = here::here(dir, "results/causal_analyses/all_dealers_all_causal_models_covariate_balance.csv"))
# fwrite(x = commercial_dealers_table, file = here::here(dir, "results/causal_analyses/commercial_dealers_all_causal_models_covariate_balance.csv"))


# Make covariate balance plots ----

# check that directories for results exist; if not, create them
main_causal_results_dir <- here::here(dir, "results/causal_analyses")
if (!dir.exists(main_causal_results_dir)) dir.create(main_causal_results_dir, recursive = T)
sensitivity_results_dir <- here::here(dir, "results/sensitivity_analyses")
if (!dir.exists(sensitivity_results_dir)) dir.create(sensitivity_results_dir, recursive = T)

# plot covariate balance for main analysis
# note that "exposure" is called "intervention" in the paper
main_models <- ggplot(causal_results[Exposure == "Distance to Commercial Firearms Dealer" &
                                       `Exposure Trimming` == "Data trimmed at\n5th, 95th percentiles" &
                                       `Categorical Confounder(s)` == "Controlling for state and urbanicity"],
                      aes(x = `Absolute Correlation`, y = reorder(Covariate, `Absolute Correlation`),
                          group = Dataset, color = Dataset, shape = Dataset)) +
                 geom_point() +
                 geom_line(orientation = "y") +
                 geom_vline(xintercept = 0) +
                 geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
                 theme_minimal() +
                 labs(x = "Absolute Correlation with Intervention",
                      y = "",
                      title = "Covariate Balance for Main Causal Models")
ggsave(filename = here::here(main_causal_results_dir, "main_causal_models_covariate_balance.png"),
       plot = main_models)

# plot covariate balance for sensitivity analyses (faceted)
# note that "exposure" is called "intervention" in the paper
p <- ggplot(causal_results,
            aes(x = `Absolute Correlation`, y = reorder(Covariate, `Absolute Correlation`),
                group = Dataset, color = Dataset, shape = Dataset)) +
  geom_point() +
  geom_line(orientation = "y") +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(x = "Absolute Correlation with Intervention",
       y = "",
       title = paste0("Covariate Balance in Sensitivity Analyses")) +
  facet_grid(rows = vars(`Exposure`),
             cols = vars(`Exposure Trimming`))
ggsave(filename = here::here(sensitivity_results_dir, "Sensitivity Analyses Covariate Balance.png"),
       plot = p)
