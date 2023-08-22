## Load packages ----
library(ggplot2)
library(data.table)

## Load filepaths for results ----
dir <- "../" # run code in the script location

associational_results_paths <- list.files(paste0(dir, "results/associational_analyses/"),
                                          pattern = ".*.txt",
                                          full.names = T)
causal_results_paths <- list.files(paste0(dir, "results/causal_analyses/"),
                                   pattern = ".*.txt",
                                   full.names = T)


## Read in results ----

## to do: edit
read_one_associational_txt <- function(path){
  results <- fread(path, header = F)
  results_formatted <- data.frame("Model" = results[2, ],
                                  "Exposure" = 1,
                                  "Cat_Confounder" = results[3, ],
                                  "Trim" = results[4, ],
                                  "Effect" = results[6, ],
                                  "CI_lower" = results[8, ],
                                  "CI_upper" = results[10, ])
}

## to do: edit
read_one_causal_txt <- function(path){
  results <- fread(path, header = F, fill = T)
  results_formatted <- data.frame("Model" = results[2, 2],
                                  "Exposure" = 1,
                                  "Cat_Confounder" = results[3, 2],
                                  "Trim" = results[4, 2],
                                  "Effect" = results[6, 2],
                                  "CI_lower" = results[8, 2],
                                  "CI_upper" = results[10, 2])
}

associational_results <- rbindlist(lapply(associational_results_paths, read_one_associational_txt))
causal_results <- rbindlist(lapply(causal_results_paths, read_one_associational_txt))



