print("## Load packages ----")

library(data.table)
library(xtable)


print("## Get data ----")

dir <- "../" # run code in the script location

full_data <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))

source(paste0(dir, "lib/functions_to_load_data.R"))

trim_quantiles <- quantile(full_data$mean_dist_commercial_dealers, c(0.05, 0.95))
trimmed_data <- full_data[mean_dist_commercial_dealers >= trim_quantiles[1] &
                            mean_dist_commercial_dealers <= trim_quantiles[2]]


print("## Set up Table 1 ----")

# to do: rename "exposure" to "intervention" to match terminology in our paper
table1 <- data.table(Variable = c("Data size, outcome, and exposure",
                                  "Number of (school-containing) census tracts",
                                  "Number of census tracts that had at least one SGI",
                                  "Median distance to closest commercial dealer (miles)",
                                  "Demographic variables by census tract",
                                  covariates_list[["demographic"]],
                                  "Socio-economic variables by census tract",
                                  covariates_list[["socioeconomic"]],
                                  "Mental health variables by census tract",
                                  covariates_list[["mental_health"]],
                                  "Crime rate variables by census tract",
                                  covariates_list[["crime"]],
                                  "Gun-affinity variables by census tract",
                                  covariates_list[["gun_affinity"]],
                                  "Racial-ethnic variables by census tract",
                                  covariates_list[["racioethnic"]],
                                  "Geographic variables",
                                  categorical_covariates),
                     FullDataMean = 0, # placeholder values
                     FullDataSD = 0,
                     TrimmedDataMean = 0,
                     TrimmedDataSD = 0,
                     `Full Data` = "",
                     `Trimmed Data` = "",
                     Source = "")


print("## Calculate means and standard deviations for quantitative covariates in Table 1 ----")

for (var in quantitative_covariates){
  table1[Variable == var, `:=`(FullDataMean = mean(full_data[[var]]),
                               FullDataSD = sd(full_data[[var]]),
                               TrimmedDataMean = mean(trimmed_data[[var]]),
                               TrimmedDataSD = sd(trimmed_data[[var]]))]
}

table1[, `:=`(FullDataMean = ifelse(FullDataMean < 100,
                                    round(FullDataMean, 2), # if less than 100, round to 2 decimal digits
                                    formatC(FullDataMean, digits = 0, big.mark = ",", format = "f")), # else, round to integer, formatted with commas
              FullDataSD = ifelse(FullDataSD < 100,
                                  round(FullDataSD, 2),
                                  formatC(FullDataSD, digits = 0, big.mark = ",", format = "f")),
              TrimmedDataMean = ifelse(TrimmedDataMean < 100,
                                       round(TrimmedDataMean, 2),
                                       formatC(TrimmedDataMean, digits = 0, big.mark = ",", format = "f")),
              TrimmedDataSD = ifelse(TrimmedDataSD < 100,
                                     round(TrimmedDataSD, 2),
                                     formatC(TrimmedDataSD, digits = 0, big.mark = ",", format = "f")))]


print("## Reformat columns of Table 1 ----")

# combine mean column and SD column into a single column
table1[, `:=`(`Full Data` = paste0(FullDataMean, " (", FullDataSD, ")"),
              `Trimmed Data` = paste0(TrimmedDataMean, " (", TrimmedDataSD, ")"))]
table1[, `:=`(FullDataMean = NULL, FullDataSD = NULL, TrimmedDataMean = NULL, TrimmedDataSD = NULL)]

# put NA in header rows
table1[Variable %in% c("Data size, outcome, and exposure",
                       "Demographic variables by census tract",
                       "Socio-economic variables by census tract",
                       "Mental health variables by census tract",
                       "Crime rate variables by census tract",
                       "Gun-affinity variables by census tract",
                       "Racial-ethnic variables by census tract",
                       "Geographic variables"), `:=`(`Full Data` = NA,
                                                     `Trimmed Data` = NA,
                                                     Source = NA)]


print("## Fill out data size, outcome, and exposure summaries in Table 1 ----")

table1[Variable == "Number of (school-containing) census tracts", `:=`(`Full Data` = formatC(nrow(full_data), digits = 0, big.mark = ",", format = "f"),
                                                                       `Trimmed Data` = formatC(nrow(trimmed_data), digits = 0, big.mark = ",", format = "f"))]
table1[Variable == "Number of census tracts that had at least one SGI", `:=`(`Full Data` = formatC(sum(full_data$SGI), digits = 0, big.mark = ",", format = "f"),
                                                                             `Trimmed Data` = formatC(sum(trimmed_data$SGI), digits = 0, big.mark = ",", format = "f"))]
table1[Variable == "Median distance to closest commercial dealer (miles)", `:=`(`Full Data` = round(median(full_data$mean_dist_commercial_dealers), 2),
                                                                                `Trimmed Data` = round(median(trimmed_data$mean_dist_commercial_dealers), 2))]


print("## Calculate number of unique values for categorical variables in Table 1 ----")

for (var in categorical_covariates){
  table1[Variable == var, `:=`(`Full Data` = uniqueN(full_data[[var]]),
                               `Trimmed Data` = uniqueN(trimmed_data[[var]]))]
}


print("## Fill out variable names and sources in Table 1 ----")

for (i in 1:length(quantitative_covariates)){
  table1[Variable == quantitative_covariates[i], `:=`(Variable = quant_covars_full_names[i],
                                                      Source = quant_covars_sources[i])]
}

for (i in 1:length(categorical_covariates)){
  table1[Variable == categorical_covariates[i], `:=`(Variable = cat_covars_full_names[i],
                                                     Source = cat_covars_sources[i])]
}

table1[Variable == "Number of (school-containing) census tracts", Source := data_outcome_and_exposure_sources[1]]
table1[Variable == "Number of census tracts that had at least one SGI", Source := data_outcome_and_exposure_sources[2]]
table1[Variable == "Median distance to closest commercial dealer (miles)", Source := data_outcome_and_exposure_sources[3]]


print("## Print Table 1 as LaTeX table ----")

print(xtable(table1), include.rownames = F)
# finally, in overleaf: add \textbf{}, \toprule[2pt], \bottomrule[2pt], \midrule, and caption


print("## Save Table 1 as csv ----")

fwrite(table1, paste0(dir, "results/table1.csv"))
