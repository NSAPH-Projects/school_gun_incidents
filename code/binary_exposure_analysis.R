rm(list=ls())
# set.seed(2022)

library(xtable)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
#setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get datasets
data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 2)
data_discretized <- decilize_quantitative_confounders(data_binary_exposure)

# Get datasets for sensitivity analyses
data_incl_urban_rural <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 2, confounder_names = c(all_confounder_names, "urban_rural"),
                                                                                       alternate_data = "data/all_tracts_2020_subset_vars_incl_urban_rural.csv")
data_discr_incl_urban_rural <- decilize_quantitative_confounders(data_incl_urban_rural)
data_incl_na_mental_health <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 2, confounder_names = all_confounder_names[all_confounder_names != "mental_health_index"])
data_discr_incl_na_mental_health <- decilize_quantitative_confounders(data_incl_na_mental_health, decilized_confounders = quantitative_confounders[quantitative_confounders != "mental_health_index"])
data_incl_state_name <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", cutoff = 2, confounder_names = c(quantitative_confounders, "State_Name")) # doesn't include: census_division_number
data_CA <- data_incl_state_name[data_incl_state_name$State_Name == "California", ]
data_CA$State_Name <- NULL
data_discr_CA <- decilize_quantitative_confounders(data_CA)
data_NY <- data_incl_state_name[data_incl_state_name$State_Name == "New York", ]
data_NY$State_Name <- NULL
data_discr_NY <- decilize_quantitative_confounders(data_NY)
data_TX <- data_incl_state_name[data_incl_state_name$State_Name == "Texas", ]
data_TX$State_Name <- NULL
data_discr_TX <- decilize_quantitative_confounders(data_TX)

# Explore exposure distribution
table(data_discretized$a)

# Initialize Formulas
formula_matching <- as.formula(paste("a ~", paste(decilized_confounder_names, collapse = "+", sep = "")))
formula_main_eq <-  as.formula(paste("y ~ a +", paste(decilized_confounder_names, collapse = "+", sep = "")))
formula_matching_incl_urban_rural <- as.formula(paste("a ~", paste(decilized_confounder_names_incl_urban_rural, collapse = "+", sep = "")))
formula_main_eq_incl_urban_rural <-  as.formula(paste("y ~ a +", paste(decilized_confounder_names_incl_urban_rural, collapse = "+", sep = "")))
formula_matching_incl_na_mental_health <-as.formula(paste("a ~", paste(decilized_confounder_names[decilized_confounder_names != "mental_health_index_decile"], collapse = "+", sep = "")))
formula_main_eq_incl_na_mental_health <-  as.formula(paste("y ~ a +", paste(decilized_confounder_names[decilized_confounder_names != "mental_health_index_decile"], collapse = "+", sep = "")))
formula_matching_state_level <- as.formula(paste("a ~", paste(decilized_confounder_names_state_level, collapse = "+", sep = "")))
formula_main_eq_state_level <-  as.formula(paste("y ~ a +", paste(decilized_confounder_names_state_level, collapse = "+", sep = "")))

formula_main_eq_cont_confounders <-  as.formula(paste("y ~ a +", paste(all_confounder_names, collapse = "+", sep = "")))
formula_main_eq_incl_urban_rural_cont_confounders <-  as.formula(paste("y ~ a +", paste(confounders_incl_urban_rural, collapse = "+", sep = "")))
formula_main_eq_incl_na_mental_health_cont_confounders <-  as.formula(paste("y ~ a +", paste(all_confounder_names[all_confounder_names != "mental_health_index"], collapse = "+", sep = "")))
formula_main_eq_state_level_cont_confounders <-  as.formula(paste("y ~ a +", paste(quantitative_confounders, collapse = "+", sep = "")))

# Initialize data frames tracking results from all analyses
# all_analyses_weights <- data.frame(GEOID = data_discretized$GEOID)
all_analyses_results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(all_analyses_results) <- c("analysis", "model", "estimate", "SE", "significance")

# Preliminary Analysis with Logistic Regression (Census Division FE)
logistic <- summary(glm(formula_main_eq_cont_confounders,
                       data = data_binary_exposure), # Aug 1 edit: not data_discretized
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
logistic
all_analyses_results[1, ] <- c("main", "naive_logistic", logistic$coefficients[2,1], logistic$coefficients[2,2], logistic$coefficients[2, 4])

logistic_urban_rural <- summary(glm(formula_main_eq_incl_urban_rural_cont_confounders,
                        data = data_incl_urban_rural),
                    family = binomial(link = "logit"), robust = "HC1", digits = 5)
logistic_urban_rural
all_analyses_results <- rbind(all_analyses_results, c("urban_rural", "naive_logistic", logistic_urban_rural$coefficients[2,1], logistic_urban_rural$coefficients[2,2], logistic_urban_rural$coefficients[2, 4]))

logistic_na_m_health <- summary(glm(formula_main_eq_incl_na_mental_health_cont_confounders,
                        data = data_incl_na_mental_health),
                    family = binomial(link = "logit"), robust = "HC1", digits = 5)
logistic_na_m_health
all_analyses_results <- rbind(all_analyses_results, c("NA_mental_health", "naive_logistic", logistic_na_m_health$coefficients[2,1], logistic_na_m_health$coefficients[2,2], logistic_na_m_health$coefficients[2, 4]))

logisticCA <- summary(glm(formula_main_eq_state_level_cont_confounders,
                         data = data_CA),
                     family = binomial(link = "logit"), robust = "HC1", digits = 5)
logisticCA
all_analyses_results <- rbind(all_analyses_results, c("CA_only", "naive_logistic", logisticCA$coefficients[2,1], logisticCA$coefficients[2,2], logisticCA$coefficients[2, 4]))

logisticNY <- summary(glm(formula_main_eq_state_level_cont_confounders,
                         data = data_NY),
                     family = binomial(link = "logit"), robust = "HC1", digits = 5)
logisticNY
all_analyses_results <- rbind(all_analyses_results, c("NY_only", "naive_logistic", logisticNY$coefficients[2,1], logisticNY$coefficients[2,2], logisticNY$coefficients[2, 4]))

logisticTX <- summary(glm(formula_main_eq_state_level_cont_confounders,
                         data = data_TX),
                     family = binomial(link = "logit"), robust = "HC1", digits = 5)
logisticTX
all_analyses_results <- rbind(all_analyses_results, c("TX_only", "naive_logistic", logisticTX$coefficients[2,1], logisticTX$coefficients[2,2], logisticTX$coefficients[2, 4]))

################################################################################
## Matching to Find the Average Treatment Effect on the Control (ATC)
## Nearest neighbor matching with replacement - DISCRETE confounders

################################################################################
## Match only Census division exactly

set.seed(42)
matched_pop <- matchit(formula_matching,
                       data = data_discretized,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("main", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_urban_rural,
                       data = data_discr_incl_urban_rural,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("urban_rural", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_na_mental_health,
                       data = data_discr_incl_na_mental_health,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NA_mental_health", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_CA,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("CA_only", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_NY,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NY_only", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_TX,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("TX_only", "match_division_only", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))


################################################################################
## Match Exactly Census Division, Dealers Density, Income
set.seed(42)
matched_pop_exact <- matchit(formula_matching,
                       data = data_discretized,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number", "dealers_per_sq_meter", "log_median_hh_income"),
                       caliper = NULL)
summary(matched_pop_exact$weights)

loveplot_star_raw(matched_pop_exact)
matched.data.exact <- match.data(matched_pop_exact)
n_matched_pop_exact <- nrow(matched.data.exact)
n_matched_pop_exact

n_imbalanced_exact <- length(which(abs(summary(matched_pop_exact)[4]$sum.matched[,3]) > 0.1))
n_imbalanced_exact

outcome <- summary(glm(y ~ a,
                       data = matched.data.exact,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("main", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_urban_rural,
                       data = data_discr_incl_urban_rural,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("urban_rural", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_na_mental_health,
                       data = data_discr_incl_na_mental_health,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NA_mental_health", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_CA,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("CA_only", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_NY,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NY_only", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_TX,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("TX_only", "match_div_dealers_medinc", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

################################################################################
## Exact + Housing Density
set.seed(42)
matched_pop_exact_housing <- matchit(formula_matching,
                             data = data_discretized,
                             estimand = "ATC",
                             method = "nearest",
                             replace = TRUE,
                             exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile", "housing_units_per_sq_meter_decile"),
                             caliper = NULL)
summary(matched_pop_exact_housing$weights)

loveplot_star_raw(matched_pop_exact_housing)
matched.data.exact.housing <- match.data(matched_pop_exact_housing)
n_matched_pop_exact_housing <- nrow(matched.data.exact.housing)
n_matched_pop_exact_housing

n_imbalanced_exact_housing <- length(which(abs(summary(matched_pop_exact_housing)[4]$sum.matched[,3]) > 0.1))
n_imbalanced_exact_housing

outcome <- summary(glm(y ~ a,
                       data = matched.data.exact.housing,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("main", "match_housing", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_urban_rural,
                       data = data_discr_incl_urban_rural,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile", "housing_units_per_sq_meter_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("urban_rural", "match_housing", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_CA,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile", "housing_units_per_sq_meter_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("CA_only", "match_housing", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_NY,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile", "housing_units_per_sq_meter_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NY_only", "match_housing", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_TX,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile", "housing_units_per_sq_meter_decile"))

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("TX_only", "match_housing", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))


################################################################################
## Sensitivity Analysis - Caliper = 0.2

set.seed(42)
matched_pop_caliper_0.2 <- matchit(formula_matching,
                                   data = data_discretized,
                                   method = "nearest",
                                   estimand = "ATC",
                                   replace = TRUE,
                                   exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                                   caliper = 0.2)

summary(matched_pop_caliper_0.2$weights)
loveplot_star_raw(matched_pop_caliper_0.2)

matched.data.caliper.0.2 <- match.data(matched_pop_caliper_0.2)
n_matched_pop_caliper_0.2 <- nrow(matched.data.caliper.0.2)
n_matched_pop_caliper_0.2

n_imbalanced_caliper_0.2 <- length(which(abs(summary(matched_pop_caliper_0.2)[4]$sum.matched[,3]) > 0.1))
n_imbalanced_caliper_0.2


outcome <- summary(glm(y ~ a,
                       data = matched.data.caliper.0.2,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("main", "caliper0.2", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_incl_urban_rural,
                       data = data_discr_incl_urban_rural,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number", "dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                       caliper = 0.2)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("urban_rural", "caliper0.2", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_CA,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                       caliper = 0.2)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("CA_only", "caliper0.2", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_NY,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                       caliper = 0.2)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("NY_only", "caliper0.2", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))

# Sensitivity analysis
set.seed(42)
matched_pop <- matchit(formula_matching_state_level,
                       data = data_discr_TX,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("dealers_per_sq_meter_decile", "log_median_hh_income_decile"),
                       caliper = 0.2)

summary(matched_pop$weights)
# all_analyses_weights$match_division_only <- matched_pop$weights

loveplot_star_raw(matched_pop)
matched.data <- match.data(matched_pop)
n_matched_pop <- nrow(matched.data)
n_matched_pop

n_imbalanced <- length(which(abs(summary(matched_pop)[4]$sum.matched[,3]) > 0.1))
n_imbalanced

outcome <- summary(glm(y ~ a,
                       data = matched.data,
                       weights = weights),
                   family = binomial(link = "logit"), robust = "HC1", digits = 5)
outcome
all_analyses_results <- rbind(all_analyses_results, c("TX_only", "caliper0.2", outcome$coefficients[2,1], outcome$coefficients[2,2], outcome$coefficients[2, 4]))


################################################################################
## Export all results as csv

# export weights
# write.csv(all_analyses_weights, "results/all_binary_analyses_weights.csv")

# round and export results
all_analyses_results_rounded <- copy(all_analyses_results)
all_analyses_results_rounded[, 3:5] <- sapply(all_analyses_results_rounded[, 3:5], function(vec) format(round(as.numeric(vec), 4), scientific = F))
# write.csv(all_analyses_results_rounded, "results/all_binary_analyses_results.csv")

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
all_analyses_results_table <- data.table(Analysis = all_analyses_results_rounded$analysis, Model = all_analyses_results_rounded$model, Results = paste0(all_analyses_results_rounded$estimate, " (", all_analyses_results_rounded$SE, ") ", all_analyses_results_rounded$significance))
all_analyses_results_table <- dcast(all_analyses_results_table, Analysis ~ Model, value.var = "Results")
all_analyses_results_table <- all_analyses_results_table[, c(1, 7, 5, 3, 6, 2)] # reorder columns, remove extra analysis
all_analyses_results_table <- all_analyses_results_table[c(5, 6, 2, 1, 3, 4), ] # reorder rows, remove NA mental health
xtable(all_analyses_results_table)

