rm(list=ls())
# set.seed(2022)

library(xtable)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
#setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get dataset(s)
data_binary_exposure1km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1)
data_binary_exposure1.609km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1.609)
data_binary_exposure2km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2)
data_binary_exposure3.218km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 3.218)

# Get dataset(s) for sensitivity analyses
data_1.609km_incl_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 1.609, confounder_names = c(all_confounder_names, "urban_rural"),
                                                                                       alternate_data = "data/all_tracts_2020_subset_vars_incl_urban_rural.csv")
data_2km_incl_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 2, confounder_names = c(all_confounder_names, "urban_rural"),
                                                                                alternate_data = "data/all_tracts_2020_subset_vars_incl_urban_rural.csv")
# Initialize Formulas
formula_matching <- as.formula(paste("a ~", paste(all_confounder_names, collapse = "+", sep = "")))
formula_matching_incl_urban_rural <- as.formula(paste("a ~", paste(all_confounder_names, collapse = "+", sep = "")))

# Initialize data frames tracking results from all analyses and Std Error calculations
HC1_results <- data.frame(matrix(nrow = 4, ncol = 6)) # Heteroskedasticity Robust SE (MacKinnon and White, 1985)
vcovHC_results <- data.frame(matrix(nrow = 4, ncol = 6)) # Heteroskedasticity Consistent SE (Zeileis, 2006)
vcovCL_results <- data.frame(matrix(nrow = 4, ncol = 6)) # Clustered SE
colnames(HC1_results) <- c("ExposureCutoff", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")
colnames(vcovHC_results) <- c("ExposureCutoff", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")
colnames(vcovCL_results) <- c("ExposureCutoff", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")

HC1_results$ExposureCutoff <- c(1, 1.609, 2, 3.218)
vcovHC_results$ExposureCutoff <- c(1, 1.609, 2, 3.218)
vcovCL_results$ExposureCutoff <- c(1, 1.609, 2, 3.218)

HC1_results$n_0 <- c(9326, 20265, 26237, 38969)
vcovHC_results$n_0 <- c(9326, 20265, 26237, 38969)
vcovCL_results$n_0 <- c(9326, 20265, 26237, 38969)

HC1_results$n_1 <- c(47909, 36970, 30998, 18266)
vcovHC_results$n_1 <- c(47909, 36970, 30998, 18266)
vcovCL_results$n_1 <- c(47909, 36970, 30998, 18266)


################################################################################
## Matching to Find the Average Treatment Effect on the Control (ATC)
## CONTINUOUS confounders, no caliper
################################################################################

# 1 km cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1km, seed = 42, one_to_one = T)
HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`1:1 NN`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`1:1 NN`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 km), 1:1 NN Matching")

# 1 km cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1km, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 km), NN Matching with Replacement")

# 1 km cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1km, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 km), NN Matching with Replacement\nMatching Census Division Exactly")

# 1 mi cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1.609km, seed = 42, one_to_one = T)
HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`1:1 NN`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`1:1 NN`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), 1:1 NN Matching")

# 1 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1.609km, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement")

# 1 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure1.609km, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 1.609] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nMatching Census Division Exactly")

# 2 km cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2km, seed = 42, one_to_one = T)
HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`1:1 NN`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`1:1 NN`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), 1:1 NN Matching")

# 2 km cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2km, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), NN Matching with Replacement")

# 2 km cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure2km, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 2] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), NN Matching with Replacement\nMatching Census Division Exactly")

# 2 mi cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure3.218km, seed = 42, one_to_one = T)
HC1_results$`1:1 NN`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`1:1 NN`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`1:1 NN`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), 1:1 NN Matching")

# 2 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure3.218km, seed = 42, one_to_one = F)
HC1_results$`NN + Replace`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 mi), NN Matching with Replacement")

# 2 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching, data_binary_exposure3.218km, seed = 42, one_to_one = F, exact_vars = "census_division_number")
HC1_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_HC1_results(matched_pop))
vcovHC_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovHC_results(matched_pop))
vcovCL_results$`NN + Replace + Exact Census Div`[HC1_results$ExposureCutoff == 3.218] <- concatenate_results(get_vcovCL_results(matched_pop))
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
urban_rural_results_1mi$n_0 <- sum(data_1.609km_incl_urban_rural$a == 0)
urban_rural_results_1mi$n_1 <- sum(data_1.609km_incl_urban_rural$a == 1)

# 1 mi cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1.609km_incl_urban_rural, seed = 42, one_to_one = T)
urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_1mi$`1:1 NN`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), 1:1 NN Matching\nIncluding Urban-Rural Classification (1-6)")

# 1 mi cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1.609km_incl_urban_rural, seed = 42, one_to_one = F)
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_1mi$`NN + Replace`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nIncluding Urban-Rural Classification (1-6)")

# 1 mi cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_1.609km_incl_urban_rural, seed = 42, one_to_one = F, exact_vars = "census_division_number")
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_1mi$`NN + Replace + Exact Census Div`[urban_rural_results_1mi$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 1 mi), NN Matching with Replacement\nMatching Census Division Exactly\nIncluding Urban-Rural Classification (1-6)")

# LaTeX table
xtable(urban_rural_results_1mi)

## Using 2 km as binary exposure cutoff

urban_rural_results_2km <- data.frame(matrix(nrow = 3, ncol = 6))
colnames(urban_rural_results_2km) <- c("StdErr", "n_0", "n_1", "1:1 NN", "NN + Replace", "NN + Replace + Exact Census Div")
urban_rural_results_2km$StdErr <- c("HC1", "vcovHC", "vcovCL")
urban_rural_results_2km$n_0 <- sum(data_2km_incl_urban_rural$a == 0)
urban_rural_results_2km$n_1 <- sum(data_2km_incl_urban_rural$a == 1)

# 2 km cutoff for binary exposure, 1:1 nearest neighbors matching
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2km_incl_urban_rural, seed = 42, one_to_one = T)
urban_rural_results_2km$`1:1 NN`[urban_rural_results_2km$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2km$`1:1 NN`[urban_rural_results_2km$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2km$`1:1 NN`[urban_rural_results_2km$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), 1:1 NN Matching\nIncluding Urban-Rural Classification (1-6)")

# 2 km cutoff for binary exposure, nearest neighbors matching with replacement
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2km_incl_urban_rural, seed = 42, one_to_one = F)
urban_rural_results_2km$`NN + Replace`[urban_rural_results_2km$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2km$`NN + Replace`[urban_rural_results_2km$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2km$`NN + Replace`[urban_rural_results_2km$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), NN Matching with Replacement\nIncluding Urban-Rural Classification (1-6)")

# 2 km cutoff for binary exposure, nearest neighbors matching Census division exactly
matched_pop <- match_binary_exposure(formula_matching_incl_urban_rural, data_2km_incl_urban_rural, seed = 42, one_to_one = F, exact_vars = "census_division_number")
urban_rural_results_2km$`NN + Replace + Exact Census Div`[urban_rural_results_2km$StdErr == "HC1"] <- concatenate_results(get_HC1_results(matched_pop))
urban_rural_results_2km$`NN + Replace + Exact Census Div`[urban_rural_results_2km$StdErr == "vcovHC"] <- concatenate_results(get_vcovHC_results(matched_pop))
urban_rural_results_2km$`NN + Replace + Exact Census Div`[urban_rural_results_2km$StdErr == "vcovCL"] <- concatenate_results(get_vcovCL_results(matched_pop))
loveplot_star_raw(matched_pop, "Binary Exposure (Cutoff: 2 km), NN Matching with Replacement\nMatching Census Division Exactly\nIncluding Urban-Rural Classification (1-6)")

# LaTeX table
xtable(urban_rural_results_2km)

## Combine urban_rural analyses for all cutoffs
urban_rural_results_1mi$Cutoff <- "1 mi"
urban_rural_results_1mi <- urban_rural_results_1mi[, c(7, 1, 2, 3, 4, 5, 6)]
urban_rural_results_2km$Cutoff <- "2 km"
urban_rural_results_2km <- urban_rural_results_2km[, c(7, 1, 2, 3, 4, 5, 6)]
all_urban_rural_results <- rbind(urban_rural_results_1mi, urban_rural_results_2km)
all_urban_rural_results <- all_urban_rural_results[c(1, 4, 2, 5, 3, 6), ]
xtable(all_urban_rural_results)

