setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, cutoff = 1.609, confounder_names = c("GEOID", all_confounder_names))
data_discretized <- decilize_quantitative_confounders(data_binary_exposure)

GEOID <- data_binary_exposure$GEOID

formula_matching <- as.formula(paste("a ~", paste(decilized_confounder_names, collapse = "+", sep = "")))
formula_main_eq <-  as.formula(paste("y ~ a +", paste(decilized_confounder_names, collapse = "+", sep = "")))
formula_main_eq_cont_confounders <-  as.formula(paste("y ~ a +", paste(all_confounder_names, collapse = "+", sep = "")))

set.seed(42)
matched_pop <- matchit(formula_matching,
                       data = data_discretized,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number"))

matches <- data.frame(control = rownames(matched_pop$match.matrix), treated = matched_pop$match.matrix)
matches$control_GEOID <- GEOID[as.numeric(matches$control)]
matches$treated_GEOID <- GEOID[as.numeric(matches$treated)]
write.csv(matches, "results/binary_exposure_cutoff1.609km_main_analysis_matches.csv")
