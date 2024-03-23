## Load packages ----

library(data.table)


## Read all results ----

dir <- here::here() # location of repository

all_dealers_table <- fread(here::here(dir, "results/all_dealers_association_and_causal_results.csv"))
commercial_dealers_table <- fread(here::here(dir, "results/commercial_dealers_association_and_causal_results.csv"))


## Get results of interest ----

# get (intervention) trim values
df <- fread(here::here(dir, "data/intermediate/final_data_sep2023.csv"))
commercial_trim_5_95 <- quantile(df$mean_dist_commercial_dealers, c(0.05, 0.95))
commercial_trim_1_99 <- quantile(df$mean_dist_commercial_dealers, c(0.01, 0.99))
commercial_trim_5_95_rounded <- signif(commercial_trim_5_95, 3)
commercial_trim_1_99_rounded <- signif(commercial_trim_1_99, 3)
any_dealer_trim_5_95 <- quantile(df$mean_distance_all_persistent_dealers, c(0.05, 0.95))
any_dealer_trim_1_99 <- quantile(df$mean_distance_all_persistent_dealers, c(0.01, 0.99))
any_dealer_trim_5_95_rounded <- signif(any_dealer_trim_5_95, 3)
any_dealer_trim_1_99_rounded <- signif(any_dealer_trim_1_99, 3)

# remove analyses that didn't include urbanicity as a covariate
all_results <- rbind(commercial_dealers_table, all_dealers_table)
all_results <- all_results[Cat_Confounder == "state.urbanicity"][, Cat_Confounder := NULL]


## Make table entries more understandable ----

all_results[, Trim := as.character(Trim)]
all_results[Exposure == "mean_dist_commercial_dealers", Trim := ifelse(Trim == "5.95",
                                                                     paste0("[", commercial_trim_5_95_rounded[1], ", ", commercial_trim_5_95_rounded[2], "]"),
                                                                     paste0("[", commercial_trim_1_99_rounded[1], ", ", commercial_trim_1_99_rounded[2], "]"))]
all_results[Exposure == "mean_distance_all_persistent_dealers", Trim := ifelse(Trim == "5.95",
                                                                             paste0("[", any_dealer_trim_5_95_rounded[1], ", ", any_dealer_trim_5_95_rounded[2], "]"),
                                                                             paste0("[", any_dealer_trim_1_99_rounded[1], ", ", any_dealer_trim_1_99_rounded[2], "]"))]
all_results[, `:=`(Exposure = ifelse(Exposure == "mean_dist_commercial_dealers",
                                     "Distance to commercial firearms retailer",
                                     "Distance to any firearms retailer"),
                   CI = paste0("[", CI_95ct_lower, ", ", CI_95ct_upper, "]"))]
all_results[, `:=`(CI_95ct_lower = NULL, CI_95ct_upper = NULL)]

## Make column names more understandable ----

colnames(all_results) <- c("Intervention Definition", "Intervention Range (Miles)", "Model", "Odds Ratio of SGI", "95% Confidence Interval for Odds Ratio")


## Save table ----

fwrite(x = all_results, file = here::here(dir, "results/sensitivity_analyses/TableS2.csv"))
