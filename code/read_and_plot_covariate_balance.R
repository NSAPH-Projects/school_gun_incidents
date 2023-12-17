## Load packages ----

library(data.table)
library(ggplot2)


## Load filepaths for results ----

dir <- "../" # run code in the script location

source(paste0(dir, "lib/functions_to_load_data.R")) # used to get variables' full names

causal_results_paths <- list.files(paste0(dir, "results/causal_analyses/"),
                                   pattern = "correlation.csv",
                                   full.names = T)


## Read in results ----

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
    results$`Exposure Trimming` <- "Exposure trimmed at\n5th and 95th percentiles"
  } else results$`Exposure Trimming` <- "Exposure trimmed at\n1st and 99th percentiles"
  
  if (grepl(pattern = "state.urbanicity", x = path)){
    results$`Categorical Confounder(s)` <- "Controlling for state and urbanicity"
  } else results$`Categorical Confounder(s)` <- "Controlling for state"
  
  return(results)
}

# compile all results into one table
causal_results <- rbindlist(lapply(causal_results_paths, read_one_causal_txt))

# setorder(causal_results, Exposure, Trim, Cat_Confounder) 
# all_dealers_table <- causal_results[Exposure == "mean_distance_all_persistent_dealers"]
# commercial_dealers_table <- causal_results[Exposure == "mean_dist_commercial_dealers"]

# fwrite(x = causal_results, file = paste0(dir, "results/causal_analyses/all_causal_models_covariate_balance.csv"))
# fwrite(x = all_dealers_table, file = paste0(dir, "results/causal_analyses/all_dealers_all_causal_models_covariate_balance.csv"))
# fwrite(x = commercial_dealers_table, file = paste0(dir, "results/causal_analyses/commercial_dealers_all_causal_models_covariate_balance.csv"))


# Make covariate balance plots ----

main_models <- ggplot(causal_results[Exposure == "Distance to Commercial Firearms Dealer" &
                                       `Exposure Trimming` == "Exposure trimmed at\n5th and 95th percentiles" &
                                       `Categorical Confounder(s)` == "Controlling for state and urbanicity"],
                      aes(x = `Absolute Correlation`, y = reorder(Covariate, `Absolute Correlation`),
                          group = Dataset, color = Dataset, shape = Dataset)) +
                 geom_point() +
                 geom_line(orientation = "y") +
                 geom_vline(xintercept = 0) +
                 geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
                 theme_minimal() +
                 labs(x = "Absolute Correlation with Exposure",
                      y = "",
                      title = "Covariate Balance Plot for Main Causal Models")
ggsave(filename = paste0(dir, "results/causal_analyses/main_causal_models_covariate_balance.png"),
       plot = main_models)

for (exposure in c("Distance to Any Firearms Dealer", "Distance to Commercial Firearms Dealer")){
  p <- ggplot(causal_results[Exposure == exposure],
              aes(x = `Absolute Correlation`, y = reorder(Covariate, `Absolute Correlation`),
                  group = Dataset, color = Dataset, shape = Dataset)) +
    geom_point() +
    geom_line(orientation = "y") +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
    theme_minimal() +
    labs(x = "Absolute Correlation with Exposure",
         y = "",
         title = paste0("Covariate Balance, exposure defined as\n", exposure)) +
    facet_grid(rows = vars(`Categorical Confounder(s)`),
               cols = vars(`Exposure Trimming`),
               labeller = )
  ggsave(filename = paste0(dir, "results/causal_analyses/", exposure, " Covariate Balance.png"),
         plot = p)
}
