## Load packages ----

library(data.table)
library(readr)
library(xtable)


## Load filepaths for results ----

dir <- here::here() # location of repository

associational_results_paths <- list.files(here::here(dir, "results/associational_analyses/"),
                                          pattern = ".*.csv",
                                          full.names = T)
causal_results_paths <- list.files(here::here(dir, "results/causal_analyses/"),
                                   pattern = "(.*match.csv)|(.*weight.csv)", # don't want to get _correlation.csv files
                                   full.names = T)


## Read in results ----

read_one_associational_txt <- function(path){
  results <- fread(path)
  results <- results[, .(Exposure,
                         Trim,
                         Cat_Confounder,
                         Model,
                         Effect,
                         CI_95ct_lower,
                         CI_95ct_upper)]
  results[, Model := ifelse(Model %in% c("logistic", "naivelogistic"), "Logistic",
                            ifelse(Model %in% c("negbin", "naivenegbin"), "Negative Binomial",
                                   Model))]
  # results[, Model := fcase(Model %in% c("logistic", "naivelogistic"), "Logistic",
  #                          Model %in% c("negbin", "naivenegbin"), "Negative Binomial",
  #                          default = Model)]
  return(results)
}

read_one_causal_txt <- function(path){
  results <- fread(path)
  if (!("Cat_Confounder" %in% colnames(results))){
    if (grepl(pattern = "state.urbanicity", x = path)){
      results$Cat_Confounder <- "state.urbanicity"
    } else{
      results$Cat_Confounder <- "state"
    }
  }
  
  if (results$Model == "Match"){
    results_CRSE <- results[, .(Exposure,
                                Trim,
                                Cat_Confounder,
                                Model = "GPS Matching (CRSE)",
                                Effect = logistic_regression_estimated_odds, # CR_Effect
                                CI_95ct_lower = cl_sd_lb_95ci,
                                CI_95ct_upper = cl_sd_ub_95ci)]
    if ("GEE_Effect" %in% colnames(results)){
      results_GEE_SE <- results[, .(Exposure,
                                    Trim,
                                    Cat_Confounder,
                                    Model = "GPS Matching (GEE)",
                                    Effect = GEE_estimated_odds,
                                    CI_95ct_lower = GEE_lb_95ci,
                                    CI_95ct_upper = GEE_ub_95ci)]
      results <- rbind(results_CRSE, results_GEE_SE)
    } else results <- results_CRSE
    
  } else{ # if (results$Model == "Weight")
    results <- results[, .(Exposure,
                           Trim,
                           Cat_Confounder,
                           Model = "GPS Weighting",
                           Effect = logistic_regression_estimated_odds,
                           CI_95ct_lower = lb_95ci,
                           CI_95ct_upper = ub_95ci)]
  }
  return(results)
}

# compile all results into one table
associational_results <- rbindlist(lapply(associational_results_paths, read_one_associational_txt))
causal_results <- rbindlist(lapply(causal_results_paths, read_one_causal_txt))
all_results <- rbind(associational_results, causal_results)

# order and split the table
setorder(all_results, Exposure, Trim, Cat_Confounder, Model) 
all_dealers_table <- all_results[Exposure == "mean_distance_all_persistent_dealers"]
commercial_dealers_table <- all_results[Exposure == "mean_dist_commercial_dealers"]


## Create LaTeX table(s), if desired ----

# xtable(all_dealers_table, include.rownames = F)
# xtable(commercial_dealers_table, include.rownames = F)


## Save table(s) as csv ----

results_dir <- here::here(dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = T)
fwrite(x = all_dealers_table, file = here::here(results_dir, "all_dealers_association_and_causal_results.csv"))
fwrite(x = commercial_dealers_table, file = here::here(results_dir, "commercial_dealers_association_and_causal_results.csv"))
