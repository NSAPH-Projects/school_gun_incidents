rm(list=ls())
# set.seed(2022)

library(xtable)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
#setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get dataset(s)
# all_confounder_names <- all_confounder_names[-1] # remove census_division_number from all_confounder_names
data_binary_exposure1mi <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 1, confounder_names = c(all_confounder_names), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv") # State_Name
data_binary_exposure2mi <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 2, confounder_names = c(all_confounder_names), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv") # State_Name
data_binary_exposure_states_1mi <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 1, confounder_names = c("State_Name", quantitative_confounders), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")
data_binary_exposure_states_2mi <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 2, confounder_names = c("State_Name", quantitative_confounders), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")

# Get dataset(s) for sensitivity analyses
data_1mi_incl_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 1, confounder_names = c(all_confounder_names, "urban_rural"), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")
data_2mi_incl_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 2, confounder_names = c(all_confounder_names, "urban_rural"), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")
data_1mi_incl_states_and_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 1, confounder_names = c("State_Name", all_confounder_names, "urban_rural"), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")
data_2mi_incl_states_and_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_miles", cutoff = 2, confounder_names = c("State_Name", all_confounder_names, "urban_rural"), alternate_data = "data/all_tracts_2020_subset_vars_revised.csv")

# Initialize Formulas
formula_matching <- as.formula(paste("a ~", paste(c(all_confounder_names), collapse = "+", sep = "")))
formula_matching_states <- as.formula(paste("a ~", paste(c("State_Name", quantitative_confounders), collapse = "+", sep = "")))
formula_matching_incl_urban_rural <- as.formula(paste("a ~", paste(c(all_confounder_names, "urban_rural"), collapse = "+", sep = "")))
formula_matching_incl_states_and_urban_rural <- as.formula(paste("a ~", paste(c("State_Name", all_confounder_names, "urban_rural"), collapse = "+", sep = "")))

# Initialize data frames tracking results from all analyses and Std Error calculations
HC1_results <- data.frame(matrix(nrow = 2, ncol = 7)) # Heteroskedasticity Robust SE (MacKinnon and White, 1985)
vcovHC_results <- data.frame(matrix(nrow = 2, ncol = 7)) # Heteroskedasticity Consistent SE (Zeileis, 2006)
vcovCL_results <- data.frame(matrix(nrow = 2, ncol = 7)) # Clustered SE
colnames(HC1_results) <- c("ExposureCutoff", "n_0", "n_1", "NN + Replace (with State)", "NN + Replace + Exact State", "NN + Replace", "NN + Replace + Exact Census Div")
colnames(vcovHC_results) <- c("ExposureCutoff", "n_0", "n_1", "NN + Replace (with State)", "NN + Replace + Exact State", "NN + Replace", "NN + Replace + Exact Census Div")
colnames(vcovCL_results) <- c("ExposureCutoff", "n_0", "n_1", "NN + Replace (with State)", "NN + Replace + Exact State", "NN + Replace", "NN + Replace + Exact Census Div")

HC1_results$ExposureCutoff <- c(1, 2)
vcovHC_results$ExposureCutoff <- c(1, 2)
vcovCL_results$ExposureCutoff <- c(1, 2)

HC1_results$n_0 <- c(sum(data_binary_exposure1mi$a == 0), sum(data_binary_exposure2mi$a == 0))
vcovHC_results$n_0 <- c(sum(data_binary_exposure1mi$a == 0), sum(data_binary_exposure2mi$a == 0))
vcovCL_results$n_0 <- c(sum(data_binary_exposure1mi$a == 0), sum(data_binary_exposure2mi$a == 0))

HC1_results$n_1 <- c(sum(data_binary_exposure1mi$a == 1), sum(data_binary_exposure2mi$a == 1))
vcovHC_results$n_1 <- c(sum(data_binary_exposure1mi$a == 1), sum(data_binary_exposure2mi$a == 1))
vcovCL_results$n_1 <- c(sum(data_binary_exposure1mi$a == 1), sum(data_binary_exposure2mi$a == 1))


################################################################################
## Matching to Find the Average Treatment Effect on the Control (ATC)
## CONTINUOUS confounders, no caliper
################################################################################

# 1 mi cutoff for binary exposure, 1:1 nearest neighbors matching
# matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1mi, seed = 42, one_to_one = T)
# HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
# vcovHC_results$`1:1 NN`[vcovHC_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
# vcovCL_results$`1:1 NN`[vcovCL_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
# loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), 1:1 NN Matching")

# 1 mi cutoff for binary exposure, nearest neighbors matching with replacement (using State as covariate instead of Census division)
matched_pop <- match_binary_exposure(formula_matching_states, data_binary_exposure_states_1mi, seed = 42, one_to_one = F)
HC1_results$`NN + Replace (with State)`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace (with State)`[vcovHC_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace (with State)`[vcovCL_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement (State as covariate)")

# 1 mi cutoff for binary exposure, nearest neighbors matching state exactly
matched_pop <- match_binary_exposure(formula_matching_states, data_binary_exposure_states_1mi, seed = 42, one_to_one = F, exact_vars = "State_Name")
HC1_results$`NN + Replace + Exact State`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact State`[vcovHC_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact State`[vcovCL_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nMatching State Exactly")

# 1 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1mi, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[vcovHC_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[vcovCL_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement")

# 1 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1mi, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[vcovHC_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[vcovCL_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nMatching Census Division Exactly")


# # 2 mi cutoff for binary exposure, 1:1 nearest neighbors matching
# matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2mi, seed = 42, one_to_one = T)
# HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
# vcovHC_results$`1:1 NN`[vcovHC_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
# vcovCL_results$`1:1 NN`[vcovCL_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
# loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), 1:1 NN Matching")

# 2 mi cutoff for binary exposure, nearest neighbors matching with replacement (with State as covariate)
matched_pop <- match_binary_exposure(formula_matching_states, data_binary_exposure_states_2mi, seed = 42, one_to_one = F)
HC1_results$`NN + Replace (with State)`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace (with State)`[vcovHC_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace (with State)`[vcovCL_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement (State as covariate)")

# 2 mi cutoff for binary exposure, nearest neighbors matching State exactly
matched_pop <- match_binary_exposure(formula_matching_states, data_binary_exposure_states_2mi, seed = 42, one_to_one = F, exact_vars = "State_Name")
HC1_results$`NN + Replace + Exact State`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact State`[vcovHC_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact State`[vcovCL_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nMatching State Exactly")

# 2 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2mi, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[vcovHC_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[vcovCL_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement")

# 2 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2mi, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[vcovHC_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[vcovCL_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nMatching Census Division Exactly")


# LaTeX table
xtable(HC1_results)
xtable(vcovHC_results)
xtable(vcovCL_results)

################################################################################
## Sensitivity Analysis - including 2013 NCHS Urban-Rural classification (1-6)

urban_rural_results_1mi <- data.frame(matrix(nrow = 3, ncol = 6))
colnames(urban_rural_results_1mi) <- c("StdErr", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")
urban_rural_results_1mi$StdErr <- c("HC1", "vcovHC", "vcovCL")
urban_rural_results_1mi$n_0 <- sum(data_1mi_incl_urban_rural$a == 0)
urban_rural_results_1mi$n_1 <- sum(data_1mi_incl_urban_rural$a == 1)

# # 1 mi cutoff for binary exposure, 1:1 nearest neighbors matching
# matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1mi_incl_urban_rural, seed = 42, one_to_one = T)
# urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
# urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
# urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
# loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), 1:1 NN Matching\nIncluding Urban-Rural Classification (1-6)")

# 1 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1mi_incl_urban_rural, seed = 42, one_to_one = F)
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nIncluding Urban-Rural Classification (1-6)")

# 1 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1mi_incl_urban_rural, seed = 42, one_to_one = F, exact_vars = "census_division_number")
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nMatching Census Division Exactly\nIncluding Urban-Rural Classification (1-6)")

# LaTeX table
xtable(urban_rural_results_1mi)

## Using 2 mi as binary exposure cutoff

urban_rural_results_2mi <- data.frame(matrix(nrow = 3, ncol = 6))
colnames(urban_rural_results_2mi) <- c("StdErr", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")
urban_rural_results_2mi$StdErr <- c("HC1", "vcovHC", "vcovCL")
urban_rural_results_2mi$n_0 <- sum(data_2mi_incl_urban_rural$a == 0)
urban_rural_results_2mi$n_1 <- sum(data_2mi_incl_urban_rural$a == 1)

# # 2 mi cutoff for binary exposure, 1:1 nearest neighbors matching
# matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2mi_incl_urban_rural, seed = 42, one_to_one = T)
# urban_rural_results_2mi$`1:1 NN`[urban_rural_results_2mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
# urban_rural_results_2mi$`1:1 NN`[urban_rural_results_2mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
# urban_rural_results_2mi$`1:1 NN`[urban_rural_results_2mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
# loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), 1:1 NN Matching\nIncluding Urban-Rural Classification (1-6)")

# 2 mi cutoff for binary exposure, nearest neighbors matching with replacement using State as covariate instead of Census Division
matched_pop <- match_binary_exposure(formula_matching_incl_states_and_urban_rural, data_2mi_incl_states_and_urban_rural, seed = 42, one_to_one = F)
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nIncluding State and Urban-Rural Classification (1-6)")

# 2 mi cutoff for binary exposure, nearest neighbors matching State exactly
matched_pop <- match_binary_exposure(formula_matching_incl_states_and_urban_rural, data_2mi_incl_states_and_urban_rural, seed = 42, one_to_one = F, exact_vars = "census_division_number")
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nMatching State Exactly\nIncluding Urban-Rural Classification (1-6)")

# 2 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2mi_incl_urban_rural, seed = 42, one_to_one = F)
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2mi$`NN + Replace`[urban_rural_results_2mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nIncluding Urban-Rural Classification (1-6)")

# 2 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2mi_incl_urban_rural, seed = 42, one_to_one = F, exact_vars = "census_division_number")
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2mi$`NN + Replace + Exact Census Div`[urban_rural_results_2mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement\nMatching Census Division Exactly\nIncluding Urban-Rural Classification (1-6)")

# LaTeX table
xtable(urban_rural_results_2mi)

## Combine urban_rural analyses for all cutoffs
urban_rural_results_1mi$Cutoff <- "1 mi"
urban_rural_results_1mi <- urban_rural_results_1mi[, c(7, 1, 2, 3, 4, 5, 6)]
urban_rural_results_2mi$Cutoff <- "2 mi"
urban_rural_results_2mi <- urban_rural_results_2mi[, c(7, 1, 2, 3, 4, 5, 6)]
all_urban_rural_results <- rbind(urban_rural_results_1mi, urban_rural_results_2mi)
all_urban_rural_results <- all_urban_rural_results[c(1, 4, 2, 5, 3, 6), ]
xtable(all_urban_rural_results)

