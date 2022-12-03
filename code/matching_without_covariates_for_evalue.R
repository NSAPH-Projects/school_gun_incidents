# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

##### Classify covariates (for exclusion, to calculate resulting e-values)

quantitative_confounders <- c("total_population_2020", "housing_units_per_100_sq_miles", "area_sq_miles",
                              "log_median_hh_income", "schools_per_100_sq_miles",
                              "log_median_hh_income_15to24", "total_crime_2021", 
                              "dealers_per_100_sq_miles", "mental_health_index",
                              "daytime_pop_2021", "prop_white_only", "prop_black_only", 
                              "prop_asian_only", "prop_multiracial", "prop_hispanic_latino", 
                              "prop_food_stamps_2019", "prop_public_assist_income_2019",
                              "prop_below_poverty_2019", "prop_without_vehicles_2019",
                              "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021", 
                              "prop_grad_deg_25plus_2021", "prop_unemployed_2021",
                              "prop_unemployed_16to24_2021", "prop_institutional_group",
                              "prop_noninstitutional_group", "prop_18plus")
demographic_confounders <- c("total_population_2020", "daytime_pop_2021", "housing_units_per_100_sq_miles", "schools_per_100_sq_miles",
                             "area_sq_miles", "prop_institutional_group", "prop_noninstitutional_group", "prop_18plus")
socioeconomic_confounders <- c("log_median_hh_income", "log_median_hh_income_15to24", "prop_food_stamps_2019", "prop_public_assist_income_2019",
                          "prop_below_poverty_2019", "prop_unemployed_2021", "prop_unemployed_16to24_2021", "total_crime_2021",
                          "mental_health_index", "prop_without_vehicles_2019", "prop_bachelor_deg_25plus_2021", "prop_grad_deg_25plus_2021")
gun_affinity_confounders <- c("dealers_per_100_sq_miles", "prop_hunted_with_shotgun_2021")
racioethnic_confounders <- c("prop_white_only", "prop_black_only", "prop_asian_only", "prop_multiracial", "prop_hispanic_latino")


##### Get data and functions

# get data, excluding classes of covariates
df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_without_demographic <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders[!(quantitative_confounders %in% demographic_confounders)]))
data_without_socioeconomic <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders[!(quantitative_confounders %in% socioeconomic_confounders)]))
data_without_gun_affinity <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders[!(quantitative_confounders %in% gun_affinity_confounders)]))
data_without_racioethnic <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders[!(quantitative_confounders %in% racioethnic_confounders)]))
# data_with_state <- na.omit(data_with_state) # unnecessary since there are no NAs at this point

# get exposure in half-miles
data_with_state$a <- data_with_state$a / 0.5
data_without_demographic$a <- data_without_demographic$a / 0.5
data_without_socioeconomic$a <- data_without_socioeconomic$a / 0.5
data_without_gun_affinity$a <- data_without_gun_affinity$a / 0.5
data_without_racioethnic$a <- data_without_racioethnic$a / 0.5


##### CausalGPS matching

all_matching_results_1model <- function(seed, data, trim,
                                        cat_confounder_names,
                                        quant_confounders = quantitative_confounders){
  set.seed(seed)
  results_list <- list()
  
  # GPS matching
  matched_pop <- get_gps_matched_pseudo_pop(data$y,
                                            data$a,
                                            data[, c(cat_confounder_names, quant_confounders)],
                                            trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(matched_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(matched_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th)")
  
  # Create alternate pseudopopulation where counter is capped
  matched_pop_capped99 <- copy(matched_pop)
  matched_pop_capped99$pseudo_pop$counter_weight[which(matched_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Store logistic regression output
  results_list[["logistic_regression_output"]] <- concatenate_results(get_gps_matched_logistic_results(matched_pop)$coefficients["w", ])
  results_list[["logistic_regression_output_capped99"]] <- concatenate_results(get_gps_matched_logistic_results(matched_pop_capped99)$coefficients["w", ])
  
  # Save covariate balance plots and splines
  for (capped in c(1, .99)){ # quantiles of counter # c(1, .99, .95)
    if (capped == 1){
      pseudo_pop <- matched_pop
      model_name_addition <- ""
    } else if (capped == .99){
      pseudo_pop <- matched_pop_capped99
      model_name_addition <- ".capped99"
    }
    
    # for capped pseudopopulations, recalculate covariate balance
    if (capped < 1){
      adjusted_corr_obj <- check_covar_balance(w = as.data.table(pseudo_pop$pseudo_pop$w),
                                               c = subset(pseudo_pop$pseudo_pop, select = quant_confounders),
                                               ci_appr = "matching",
                                               counter_weight = as.data.table(pseudo_pop$pseudo_pop$counter_weight),
                                               nthread = 1,
                                               covar_bl_method = "absolute",
                                               covar_bl_trs = 0.1,
                                               covar_bl_trs_type = "mean",
                                               optimized_compile=TRUE)
      pseudo_pop$adjusted_corr_results <- adjusted_corr_obj$corr_results
    }
    
    # save covariate balance plot
    results_list[[paste0("cov_bal.capped", capped)]] <- get_matched_correlation_plot(pseudo_pop, cat_confounder_names, data$a, subset(data, select = cat_confounder_names), quant_confounders)
    # cov_bal <- make_correlation_plot(pseudo_pop, "matching", cat_confounder_names, data$a, subset(data, select = cat_confounder_names))
    # ggsave(paste0("results/covariate_balance_plots/filtered_FFL/continuous_exposure/", model_name, model_name_addition, ".png"), cov_bal)
  }
  
  return(results_list) # numerical output, to be stored in results table
}

# get GPS matching results excluding classes of covariates
# state.5.95_match <- all_matching_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
match_without_demographic <- all_matching_results_1model(100, data_without_demographic, c(0.05, 0.95), "State_Name",
                                                         quantitative_confounders[!(quantitative_confounders %in% demographic_confounders)])
match_without_socioeconomic <- all_matching_results_1model(100, data_without_socioeconomic, c(0.05, 0.95), "State_Name",
                                                         quantitative_confounders[!(quantitative_confounders %in% socioeconomic_confounders)])
match_without_gun_affinity <- all_matching_results_1model(100, data_without_gun_affinity, c(0.05, 0.95), "State_Name",
                                                         quantitative_confounders[!(quantitative_confounders %in% gun_affinity_confounders)])
match_without_racioethnic <- all_matching_results_1model(100, data_without_racioethnic, c(0.05, 0.95), "State_Name",
                                                         quantitative_confounders[!(quantitative_confounders %in% racioethnic_confounders)])

# check covariate balance for GPS-matched pseudopopulations
make_correlation_plot(match_without_demographic$cov_bal.capped0.99) # total_crime_2021 has AC ~0.15
make_correlation_plot(match_without_socioeconomic$cov_bal.capped0.99) # schools_per_100_sq_miles has AC ~0.15
make_correlation_plot(match_without_gun_affinity$cov_bal.capped0.99) # all AC below 0.15
make_correlation_plot(match_without_racioethnic$cov_bal.capped0.99) # total_crime_2021 has AC ~0.15

# get logistic regression results from GPS-matched pseudopopulations
match_without_demographic$logistic_regression_output_capped99
match_without_socioeconomic$logistic_regression_output_capped99
match_without_gun_affinity$logistic_regression_output_capped99
match_without_racioethnic$logistic_regression_output_capped99

