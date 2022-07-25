rm(list=ls())
# set.seed(2022)

library(xtable)

# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get datasets (continuous and discrete confounders)
data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km")
data_discretized <- decilize_quantitative_confounders(data_binary_exposure)

# Explore exposure distribution
table(data_discretized$a)

# Initialize data frames tracking results from all analyses
all_analyses_weights <- data.frame(GEOID = data_discretized$GEOID)
all_analyses_results <- data.frame(matrix(nrow = 0, ncol = 10))
colnames(all_analyses_results) <- c("analysis", "estimate", "SE", "significance")

################################################################################
## Matching to Find the Average Treatment Effect on the Control (ATC)

## Main Analysis - Nearest neighbor matching with replacement - DISCRETE confounders

match_div_dealers_medinc <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                              exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                              caliper = NULL, seed = 42, confounders_in_regression = T)
# match_div_dealers_medinc <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                              # exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                              # caliper = NULL, seed = 42)


summary(match_div_dealers_medinc[["match.weights"]])
all_analyses_weights$match_div_dealers_medinc <- match_div_dealers_medinc[["match.weights"]] # save weights in all_analyses_weights data frame

match_div_dealers_medinc[["cov_bal_plots"]][["0"]]
match_div_dealers_medinc[["model_summary"]]
all_analyses_results[1,] <- c("match_div_dealers_medinc", match_div_dealers_medinc[["model_summary"]]$coefficients[2, 1],
                              match_div_dealers_medinc[["model_summary"]]$coefficients[2, 2], match_div_dealers_medinc[["model_summary"]]$coefficients[2, 4])


################################################################################
## Sensitivity Analysis - match only Census division exactly

match_division_only <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                         exact_matches = c("census_division_number"),
                                         caliper = NULL, seed = 42, confounders_in_regression = T)
# match_division_only <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
#                                          exact_matches = c("census_division_number"),
#                                          caliper = NULL, seed = 42)


summary(match_division_only[["match.weights"]])
all_analyses_weights$match_division_only <- match_division_only[["match.weights"]]

match_division_only[["cov_bal_plots"]][["0"]]
match_division_only[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("match_division_only", match_division_only[["model_summary"]]$coefficients[2, 1],
                                                      match_division_only[["model_summary"]]$coefficients[2, 2], match_division_only[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Sensitivity Analysis - Caliper = 0.2

caliper0.2 <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                caliper = 0.2, seed = 42, confounders_in_regression = T)
# caliper0.2 <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
#                                 exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
#                                 caliper = 0.2, seed = 42)


summary(caliper0.2[["match.weights"]])
all_analyses_weights$caliper0.2 <- caliper0.2[["match.weights"]]

caliper0.2[["cov_bal_plots"]][["0"]]
caliper0.2[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("caliper0.2", caliper0.2[["model_summary"]]$coefficients[2, 1],
                              caliper0.2[["model_summary"]]$coefficients[2, 2], caliper0.2[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Sensitivity Analysis - Caliper = 0.1

caliper0.1 <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                caliper = 0.1, seed = 42, confounders_in_regression = T)
# caliper0.1 <- match_discretized(data_discretized, control_name = "1", confounder_names = decilized_confounder_names,
#                                 exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
#                                 caliper = 0.1, seed = 42)


summary(caliper0.1[["match.weights"]])
all_analyses_weights$caliper0.1 <- caliper0.1[["match.weights"]]

caliper0.1[["cov_bal_plots"]][["0"]]
caliper0.1[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("caliper0.1", caliper0.1[["model_summary"]]$coefficients[2, 1],
                                                      caliper0.1[["model_summary"]]$coefficients[2, 2], caliper0.1[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Sensitivity Analysis - GPS matching with GPS trimming, exposure? and weight winsorization

# match by GPS, trimming GPS at 5th, 95th percentiles
set.seed(42)
data_gps <- decilize_quantitative_confounders(data_analysis)
gps_match <- get_gps_matched_pseudo_pop(data_gps$y, data_gps$a, data_gps[, decilized_confounder_names])

# winsorize GPS-matched weights at 95th percentile
unwinsorized_gps_matched_weights <- gps_match$pseudo_pop$counter
summary(unwinsorized_gps_matched_weights)
cutoff <- quantile(unwinsorized_gps_matched_weights, 0.95)
cutoff
gps_match$pseudo_pop$counter <- ifelse(unwinsorized_gps_matched_weights > cutoff, cutoff, unwinsorized_gps_matched_weights)

# plot covariate balance (with winsorized GPS-matched weights)
adjusted_corr_obj <- check_covar_balance(gps_match$pseudo_pop,
                                         ci_appr="matching",
                                         nthread=1,
                                         covar_bl_method = "absolute",
                                         covar_bl_trs = 0.1,
                                         covar_bl_trs_type = "maximal",
                                         optimized_compile=T)
gps_match$adjusted_corr_results <-  adjusted_corr_obj$corr_results
make_matched_correlation_plot(gps_match, decilized_confounder_names)

# save winsorized GPS-matched weights in all_analyses_weights
all_analyses_weights$gps_match_winsorized <- 0 # set weight=0 for observations trimmed by 5th/95th GPS percentiles
all_analyses_weights$gps_match_winsorized[gps_match$pseudo_pop$row_index] <- gps_match$pseudo_pop$counter

# save unwinsorized GPS-matched weights in all_analyses_weights
all_analyses_weights$gps_match_unwinsorized <- 0
all_analyses_weights$gps_match_unwinsorized[gps_match$pseudo_pop$row_index] <-unwinsorized_gps_matched_weights

# get logistic results (with winsorized GPS-matched weights)
# to do: add this to slides
gps_results <- get_gps_matched_logistic_results(gps_match)$coefficients[2,]


################################################################################
## Sensitivity Analysis - Grocery

data_grocery <- get_binary_exposure_continuous_confounders_dataset("mean_grocery_km")

# Explore binary exposure distribution
table(data_grocery$a)

# Discretize confounders
data_grocery_discretized <- decilize_quantitative_confounders(data_grocery)
dim(data_grocery_discretized)

# Run discretized analysis
grocery_analysis <- match_discretized(data_grocery_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                   exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                   caliper = NULL, seed = 42)
summary(grocery_analysis[["match.weights"]])

# Save weights in all_analyses_weights
tracts_with_grocery_data <- data_discretized$GEOID %in% data_grocery$GEOID
all_analyses_weights$grocery_analysis <- NA # for tracts without grocery distance data
all_analyses_weights$grocery_analysis[tracts_with_grocery_data] <- grocery_analysis[["match.weights"]]

# Covariate balance plots and save results
grocery_analysis[["cov_bal_plots"]][["0"]]
grocery_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("grocery_analysis", grocery_analysis[["model_summary"]]$coefficients[2, 1],
                                                      grocery_analysis[["model_summary"]]$coefficients[2, 2], grocery_analysis[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Sensitivity Analysis - Pharmacy

data_pharmacy <- get_binary_exposure_continuous_confounders_dataset("mean_pharmacy_km")

# Explore binary exposure distribution
table(data_pharmacy$a)

# Discretize confounders
data_pharmacy_discretized <- decilize_quantitative_confounders(data_pharmacy)
dim(data_pharmacy_discretized)

# Run discretized analysis
pharmacy_analysis <- match_discretized(data_pharmacy_discretized, control_name = "1", confounder_names = decilized_confounder_names,
                                       exact_matches = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                       caliper = NULL, seed = 42)
summary(pharmacy_analysis[["match.weights"]])

# Save weights in all_analyses_weights
tracts_with_pharmacy_data <- data_discretized$GEOID %in% data_pharmacy$GEOID
all_analyses_weights$pharmacy_analysis <- NA # for tracts without grocery distance data
all_analyses_weights$pharmacy_analysis[tracts_with_pharmacy_data] <- pharmacy_analysis[["match.weights"]]

# Covariate balance plots and save results
pharmacy_analysis[["cov_bal_plots"]][["0"]]
pharmacy_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("pharmacy_analysis", pharmacy_analysis[["model_summary"]]$coefficients[2, 1],
                                                      pharmacy_analysis[["model_summary"]]$coefficients[2, 2], pharmacy_analysis[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Stratified analysis: Census regions

census_regions <- rep(NA, nrow(data_discretized))
census_regions[which(data_discretized$census_division_number == 1 | data_discretized$census_division_number == 2)] <- 1
census_regions[which(data_discretized$census_division_number == 3 | data_discretized$census_division_number == 4)] <- 2
census_regions[which(data_discretized$census_division_number == 5 | data_discretized$census_division_number == 6 | data_discretized$census_division_number == 7)] <- 3
census_regions[which(data_discretized$census_division_number == 8 | data_discretized$census_division_number == 9)] <- 4
table(census_regions)

data_discretized_regions <- copy(data_discretized)
data_discretized_regions$census_region_number <- census_regions
data_discretized_regions$census_division_number <- NULL
data_discretized_NE <- data_discretized[data_discretized_regions$census_region_number == 1, ]
data_discretized_MW <- data_discretized[data_discretized_regions$census_region_number == 2, ]
data_discretized_S <- data_discretized[data_discretized_regions$census_region_number == 3, ]
data_discretized_W <- data_discretized[data_discretized_regions$census_region_number == 4, ]
regional_decilized_confounder_names <- decilized_confounder_names[decilized_confounder_names != "census_division_number"]
all_analyses_weights$stratified_by_4_regions <- NA

## Northeast region ("1")
NE_analysis <- match_discretized(data_discretized_NE, "1", regional_decilized_confounder_names,
                                   c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                   caliper = NULL, seed = 42)

summary(NE_analysis[["match.weights"]])
all_analyses_weights$stratified_by_4_regions[data_discretized_regions$census_region_number == 1] <- NE_analysis[["match.weights"]]
NE_analysis[["cov_bal_plots"]][["0"]]
NE_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("Northeast_region", NE_analysis[["model_summary"]]$coefficients[2, 1],
                                                      NE_analysis[["model_summary"]]$coefficients[2, 2], NE_analysis[["model_summary"]]$coefficients[2, 4]))

## Midwest region ("2")
MW_analysis <- match_discretized(data_discretized_MW, "1", regional_decilized_confounder_names,
                                 c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                 caliper = NULL, seed = 42)

summary(MW_analysis[["match.weights"]])
all_analyses_weights$stratified_by_4_regions[data_discretized_regions$census_region_number == 2] <- MW_analysis[["match.weights"]]
MW_analysis[["cov_bal_plots"]][["0"]]
MW_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("Midwest_region", MW_analysis[["model_summary"]]$coefficients[2, 1],
                                                      MW_analysis[["model_summary"]]$coefficients[2, 2], MW_analysis[["model_summary"]]$coefficients[2, 4]))

## South region ("3")
S_analysis <- match_discretized(data_discretized_S, "1", regional_decilized_confounder_names,
                                 c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                 caliper = NULL, seed = 42)

summary(S_analysis[["match.weights"]])
all_analyses_weights$stratified_by_4_regions[data_discretized_regions$census_region_number == 3] <- S_analysis[["match.weights"]]
S_analysis[["cov_bal_plots"]][["0"]]
S_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("South_region", S_analysis[["model_summary"]]$coefficients[2, 1],
                                                      S_analysis[["model_summary"]]$coefficients[2, 2], S_analysis[["model_summary"]]$coefficients[2, 4]))

## West region ("4")
W_analysis <- match_discretized(data_discretized_W, "1", regional_decilized_confounder_names,
                                 c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                 caliper = NULL, seed = 42)

summary(W_analysis[["match.weights"]])
all_analyses_weights$stratified_by_4_regions[data_discretized_regions$census_region_number == 4] <- W_analysis[["match.weights"]]
W_analysis[["cov_bal_plots"]][["0"]]
W_analysis[["model_summary"]]
all_analyses_results <- rbind(all_analyses_results, c("West_region", pharmacy_analysis[["model_summary"]]$coefficients[2, 1],
                                                      pharmacy_analysis[["model_summary"]]$coefficients[2, 2], pharmacy_analysis[["model_summary"]]$coefficients[2, 4]))


################################################################################
## Export all results as csv

# export weights
write.csv(all_analyses_weights, "results/all_binary_analyses_weights.csv")

# round and export results
all_analyses_results_rounded <- copy(all_analyses_results)
all_analyses_results_rounded[, 2:4] <- sapply(all_analyses_results_rounded[, 2:4], function(vec) format(round(as.numeric(vec), 4), scientific = F))
write.csv(all_analyses_results_rounded, "results/all_binary_analyses_results.csv")

# codify significance and save as latex table
codify_significance <- function(significance){
  significance <- as.numeric(significance)
  
  if (significance < 0.001){
    return("***")
  } else if (significance < 0.01){
    return("**")
  } else if (significance < 0.05){
    return("*")
  } else if (significance < 0.1){
    return(".")
  } else{
    return(" ")
  }
}

all_analyses_results_rounded$significance <- sapply(all_analyses_results_rounded$significance, codify_significance)

# Create LaTeX table of results
all_analyses_results_table <- data.frame(Analysis = all_analyses_results_rounded$analysis)
all_analyses_results_table$Results <- paste0(all_analyses_results_rounded$estimate, " (", all_analyses_results_rounded$SE, ") ", all_analyses_results_rounded$significance)
all_analyses_results_table$Analysis <- c("Main Analysis", "Match exactly: Division only", "Caliper = 0.2", "Caliper = 0.1",
                                         "Grocery distance as exposure", "Pharmacy distance as exposure", "Northeast (stratum)", "Midwest (stratum)", "South (stratum)", "West (stratum)")

xtable(all_analyses_results_table, caption = "Results of Main Analysis and Sensitivity Analyses", digits = 5)

# print GPS sensitivity analysis results in separate table (continuous exposure)
gps_results[c(1,2,4)]

