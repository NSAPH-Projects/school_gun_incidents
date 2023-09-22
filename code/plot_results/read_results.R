## Load packages ----
library(data.table)
library(readr)
# library(ggplot2)
# library(xtable)

## Load filepaths for results ----
dir <- "../" # run code in the script location

associational_results_paths <- list.files(paste0(dir, "results/associational_analyses/"),
                                          pattern = ".*.txt",
                                          full.names = T)
causal_results_paths <- list.files(paste0(dir, "results/causal_analyses/"),
                                   pattern = ".*.txt",
                                   full.names = T)


## Read in results ----

read_one_associational_txt <- function(path){
  results <- fread(path, header = F)
  results_formatted <- data.frame("Exposure" = results[1, 1],
                                  "Model" = results[2, 1],
                                  "Cat_Confounder" = results[3, 1],
                                  "Trim" = results[4, 1],
                                  "Effect" = results[6, 1],
                                  "CI_lower" = results[8, 1],
                                  "CI_upper" = results[10, 1])
}

read_one_causal_txt <- function(path){
  results <- fread(path, header = F, sep = " ", fill = T)
  if (results[2, 1] == "match"){ # if model is GPS matching with CRSE
    results_formatted <- data.frame("Exposure" = results[1, 1],
                                    "Model" = results[2, 1],
                                    "Cat_Confounder" = results[3, 1],
                                    "Trim" = results[4, 1],
                                    "Effect" = results[12, 1],
                                    "CI_lower" = results[14, 1],
                                    "CI_upper" = results[16, 1],
                                    "Count/Weight_Max" = results[8, 1],
                                    "Count/Weight_Cap" = parse_number(as.character(results[10, 1])))
  } else{
    results_formatted <- data.frame("Exposure" = results[1, 1],
                                    "Model" = results[2, 1],
                                    "Cat_Confounder" = results[3, 1],
                                    "Trim" = results[4, 1],
                                    "Effect" = results[10, 1],
                                    "CI_lower" = results[12, 1],
                                    "CI_upper" = results[14, 1],
                                    "Count/Weight_Max" = results[6, 1],
                                    "Count/Weight_Cap" = parse_number(as.character(results[8, 1])))
  }
}

associational_results <- rbindlist(lapply(associational_results_paths, read_one_associational_txt))
colnames(associational_results) <- c("Exposure", "Model", "Cat_Confounder", "Trim", "Effect", "CI_lower", "CI_upper")

causal_results <- rbindlist(lapply(causal_results_paths, read_one_causal_txt))
colnames(causal_results) <- c("Exposure", "Model", "Cat_Confounder", "Trim", "Effect", "CI_lower", "CI_upper", "Count/Weight_Max", "Count/Weight_Cap")

