## Load packages ----

library(data.table)
library(readr)
# library(ggplot2)
# library(xtable)


## Load filepaths for results ----

dir <- "../" # run code in the script location

tuning_results_paths <- list.files(paste0(dir, "results/causal_analyses/tune_matching/"),
                                   pattern = ".*.txt",
                                   full.names = T)


## Read in results ----

read_one_tuning_txt <- function(path){
  results <- fread(path, header = F, sep = " ", fill = T)
  results_formatted <- data.table("Caliper" = round(as.numeric(results[5, 1]), 3),
                                  "Mean_AC" = round(as.numeric(results[23, 1]), 3),
                                  "Max_AC" = round(as.numeric(results[27, 1]), 3),
                                  "Effect" = results[13, 1],
                                  "CI_lower" = results[15, 1],
                                  "CI_upper" = results[17, 1],
                                  "Count_Max" = signif(as.numeric(results[9, 1]), digits = 4),
                                  "Count_Cap" = results[11, 1])
  # "Exposure" = results[1, 1],
  # "Trim" = results[4, 1],
  # "Cat_Confounder" = results[3, 1],
  # "Model" = results[2, 1],
  return(results_formatted)
}

tuning_results <- rbindlist(lapply(tuning_results_paths, read_one_tuning_txt))
colnames(tuning_results) <- c("Caliper", "Mean_AC", "Max_AC", "Effect", "CI_lower", "CI_upper", "Count_Max", "Count_Cap")
