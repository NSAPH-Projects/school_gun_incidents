# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")
source("code/functions_using_gps.R")

##### Get data and functions

df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_with_state <- na.omit(data_with_state)
data_with_state$a <- data_with_state$a / 0.5 # get exposure in half-miles

# for sensitivity analysis: get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders, "urban_rural"))
data_with_urbanity_state <- na.omit(data_with_urbanity_state)
data_with_urbanity_state$a <- data_with_urbanity_state$a / 0.5 # get exposure in half-miles

# get 1st/99th and 5th/95th percentiles of exposure (for trimming)
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))
# exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))


##### CausalGPS matching

# Get numerical results
state.5.95_match <- all_matching_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
make_correlation_plot(state.5.95_match$cov_bal.capped0.99)
mean(state.5.95_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
state.5.95_match$logistic_regression_output_capped99
state.5.95_match$counter99
# state.1.99_match <- all_matching_results_1model(100, data_with_state, c(0.01, 0.99), "State_Name")
# make_correlation_plot(state.1.99_match$cov_bal.capped0.99)
# mean(state.1.99_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
# state.1.99_match$logistic_regression_output_capped99
# state.1.99_match$counter99

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.05, 0.95), c("State_Name", "urban_rural"))
make_correlation_plot(state.urbanity.5.95_match$cov_bal.capped0.99)
mean(state.urbanity.5.95_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
state.urbanity.5.95_match$logistic_regression_output_capped99
state.urbanity.5.95_match$counter99
# state.urbanity.1.99_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.01, 0.99), c("State_Name", "urban_rural"))
# make_correlation_plot(state.urbanity.1.99_match$cov_bal.capped0.99)
# mean(state.urbanity.1.99_match$cov_bal.capped0.99[Dataset == "Matched", `Absolute Correlation`]) # get mean AC of matched pseudopopulation
# state.urbanity.1.99_match$logistic_regression_output_capped99
# state.urbanity.1.99_match$counter99


##### Weight by GPS

# Get numerical results
state.5.95_weight <- all_weighting_results_1model(100, data_with_state, c(0.05, 0.95), "State_Name")
make_correlation_plot(state.5.95_weight$cov_bal.capped0.99)
mean(state.5.95_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
state.5.95_weight$logistic_regression_output_capped99
state.5.95_weight$counter99
# state.1.99_weight <- all_weighting_results_1model(100, data_with_state, c(0.01, 0.99), "State_Name")
# make_correlation_plot(state.1.99_weight$cov_bal.capped0.99)
# mean(state.1.99_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
# state.1.99_weight$logistic_regression_output_capped99
# state.1.99_weight$counter99

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.05, 0.95), c("State_Name", "urban_rural"))
make_correlation_plot(state.urbanity.5.95_weight$cov_bal.capped0.99)
mean(state.urbanity.5.95_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
state.urbanity.5.95_weight$logistic_regression_output_capped99
state.urbanity.5.95_weight$counter99
# state.urbanity.1.99_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.01, 0.99), c("State_Name", "urban_rural"))
# make_correlation_plot(state.urbanity.1.99_weight$cov_bal.capped0.99)
# mean(state.urbanity.1.99_weight$cov_bal.capped0.99[Dataset == "Weighted", `Absolute Correlation`]) # get mean AC of weighted pseudopopulation
# state.urbanity.1.99_weight$logistic_regression_output_capped99
# state.urbanity.1.99_weight$counter99

