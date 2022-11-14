# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

##### Get data and functions

df <- fread("data/all_tracts_2020_subset_vars_revised.csv")
data_with_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders))
data_with_state <- na.omit(data_with_state)
# data_with_censusdiv <- get_analysis_df(df, "mean_total_miles", c("census_division_number", quantitative_confounders))
# data_with_censusdiv <- na.omit(data_with_censusdiv)
# data_state_trimmed <- data_with_state[which(data_with_state$a <= 16.2 ),] # get 99th-exposure-percentile-trimmed data
# data_censusdiv_trimmed <- data_with_censusdiv[which(data_with_censusdiv$a <= 16.2 ),] # get 99th-exposure-percentile-trimmed data

# for sensitivity analysis: get data including urban_rural variable
data_with_urbanity_state <- get_analysis_df(df, "mean_total_miles", c("State_Name", quantitative_confounders, "urban_rural"))
data_with_urbanity_state <- na.omit(data_with_urbanity_state)
# data_with_urbanity_censusdiv <- get_analysis_df(df, "mean_total_miles", c("census_division_number", quantitative_confounders, "urban_rural"))
# data_with_urbanity_censusdiv <- na.omit(data_with_urbanity_censusdiv)

# prepare to store data
all_results <- as.data.table(data.frame(Cat_Covariates = rep(rep(c("State", "State + Urbanity"), each = 5), 2),
                          Exposure_Range = "",
                          Counter_Max = "",
                          Counter_Cap = "",
                          Model = rep(c("Naive Logistic", "Naive NegBin", "Match", "Weight", "Adjust"), 4),
                          Results = ""))

# get 1st/99th and 5th/95th percentiles of exposure (for trimming)
exposure1.99 <- quantile(data_with_state$a, c(0.01, 0.99))
exposure5.95 <- quantile(data_with_state$a, c(0.05, 0.95))
formatted1.99 <- paste0("[", round(exposure1.99, 2)[1], ", ", round(exposure1.99, 2)[2], "]")
formatted5.95 <- paste0("[", round(exposure5.95, 2)[1], ", ", round(exposure5.95, 2)[2], "]")
all_results$Exposure_Range <- rep(c(formatted1.99, formatted5.95), each = 10)


##### CausalGPS matching

all_matching_results_1model <- function(seed, data, trim, model_name, cat_confounder_names, quant_confounders = quantitative_confounders){
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

# Get numerical results
state.5.95_match <- all_matching_results_1model(100, data_with_state, c(0.05, 0.95),
                                          "state.5.95", "State_Name")
state.1.99_match <- all_matching_results_1model(100, data_with_state, c(0.01, 0.99),
                                                   "state.1.99", "State_Name")
# censusdiv.5.95_match <- all_matching_results_1model(100, data_with_censusdiv, c(0.05, 0.95),
#                                                    "censusdiv.5.95", "census_division_number")
# censusdiv.1.99_match <- all_matching_results_1model(100, data_with_censusdiv, c(0.01, 0.99),
#                                                  "censusdiv.1.99", "census_division_number")

# Sensitivity Analysis: including urban-rural
state.urbanity.1.99_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.01, 0.99),
                                                   "state.urbanity.1.99", c("State_Name", "urban_rural"))
state.urbanity.5.95_match <- all_matching_results_1model(100, data_with_urbanity_state, c(0.05, 0.95),
                                                 "state.urbanity.5.95", c("State_Name", "urban_rural"))
# censusdiv.urbanity.5.95_match <- all_matching_results_1model(100, data_with_urbanity_censusdiv, c(0.05, 0.95),
#                                                      "censusdiv.urbanity.5.95", c("census_division_number", "urban_rural"))
# censusdiv.urbanity.1.99_match <- all_matching_results_1model(100, data_with_urbanity_censusdiv, c(0.01, 0.99),
#                                                        "censusdiv.urbanity.1.99", c("census_division_number", "urban_rural"))

### m out of n bootstrap

data_with_state.5.95 <- data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ]

n_boot <- 10^3
n <- nrow(data_with_state.5.95)
m <- 10

# a.vals <- seq(min(data_with_state.5.95$a), max(data_with_state.5.95$a), length.out = 50)
# delta_n <- a.vals[2] - a.vals[1]



##### Weight by GPS

all_weighting_results_1model <- function(seed, data, trim, model_name, cat_confounder_names, quant_confounders = quantitative_confounders){
  set.seed(seed)
  results_list <- list()
  
  # GPS weighting
  weighted_pop <- get_gps_weighted_pseudo_pop(data$y,
                                            data$a,
                                            data[, c(cat_confounder_names, quant_confounders)],
                                            trim)
  # Store key counter quantiles
  results_list[["counter_max"]] <- round(max(weighted_pop$pseudo_pop$counter_weight), 2)
  cap99 <- round(quantile(weighted_pop$pseudo_pop$counter_weight, 0.99), 2)
  results_list[["counter99"]] <- paste(cap99, "(99th)")
  
  # Create alternate pseudopopulation where counter is capped
  weighted_pop_capped99 <- copy(weighted_pop)
  weighted_pop_capped99$pseudo_pop$counter_weight[which(weighted_pop_capped99$pseudo_pop$counter_weight >= cap99)] <- cap99
  
  # Store logistic regression output
  results_list[["logistic_regression_output"]] <- concatenate_results(get_gps_weighted_logistic_results(weighted_pop)$coefficients["w", ])
  results_list[["logistic_regression_output_capped99"]] <- concatenate_results(get_gps_weighted_logistic_results(weighted_pop_capped99)$coefficients["w", ])
  
  # Save covariate balance plots
  for (capped in c(1, .99)){ # quantiles of counter # c(1, .99, .95)
    if (capped == 1){
      pseudo_pop <- weighted_pop
      model_name_addition <- ""
    } else if (capped == .99){
      pseudo_pop <- weighted_pop_capped99
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
    results_list[[paste0("cov_bal.capped", capped)]] <- get_weighted_correlation_plot(pseudo_pop, cat_confounder_names, data$a, subset(data, select = cat_confounder_names), quant_confounders)
    # cov_bal <- make_correlation_plot(pseudo_pop, "weighting", cat_confounder_names, data$a, subset(data, select = cat_confounder_names))
    # ggsave(paste0("results/covariate_balance_plots/filtered_FFL/continuous_exposure/", model_name, model_name_addition, ".png"), cov_bal)
  }
  
  return(results_list) # numerical output, to be stored in results table
}

# Get numerical results
state.5.95_weight <- all_weighting_results_1model(100, data_with_state, c(0.05, 0.95),
                                          "state.5.95", "State_Name")
state.1.99_weight <- all_weighting_results_1model(100, data_with_state, c(0.01, 0.99),
                                          "state.1.99", "State_Name")

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.05, 0.95),
                                                           "state.urbanity.5.95", c("State_Name", "urban_rural"))
state.urbanity.1.99_weight <- all_weighting_results_1model(100, data_with_urbanity_state, c(0.01, 0.99),
                                                   "state.urbanity.1.99", c("State_Name", "urban_rural"))

##### NOT UPDATED (IGNORE FOR NOW): Adjusting by GPS

all_adjusting_results_1model <- function(seed, data, trim, cat_confounder_names){
  set.seed(seed)
  results_list <- list()
  
  # trim data
  data <- data[(data$a >= trim[1]) & (data$a <= trim[2]), ]
  
  # estimate GPS and add it to the data as a covariate
  gps <- get_gps(data$y, data$a, data[, c(cat_confounder_names, quantitative_confounders)])
  data$gps <- gps$dataset$gps
  
  # Store logistic regression output
  results_list[["logistic_regression_output"]] <- concatenate_results(get_gps_adjusted_logistic_results(data)$coefficients["a", ])
  return(results_list) # numerical output, to be stored in results table
}

# Get numerical results
state.5.95_adjust <- all_adjusting_results_1model(100, data_with_state, exposure5.95, "State_Name")
state.1.99_adjust <- all_adjusting_results_1model(100, data_with_state, exposure1.99, "State_Name")

# Sensitivity Analysis: including urban-rural
state.urbanity.5.95_adjust <- all_adjusting_results_1model(100, data_with_urbanity_state, exposure5.95, c("State_Name", "urban_rural"))
state.urbanity.1.99_adjust <- all_adjusting_results_1model(100, data_with_urbanity_state, exposure1.99, c("State_Name", "urban_rural"))


##### AD HOC (to do: re-code this better, use 5.95 instead of 1.99)
##### Collect covariate balances into one plot

state.1.99_weight_cov_bal.capped0.99 <- read.csv("results/covariate_balance_plots/filtered_FFL/continuous_exposure/csvs/state.1.99_weight.capped0.99_cov_bal.csv")
state.1.99_weight_cov_bal.capped1 <- read.csv("results/covariate_balance_plots/filtered_FFL/continuous_exposure/csvs/state.1.99_weight_cov_bal.csv")
state.urbanity.1.99_weight_cov_bal.capped0.99 <- read.csv("results/covariate_balance_plots/filtered_FFL/continuous_exposure/csvs/state.urbanity.1.99_weight.capped0.99_cov_bal.csv")
state.urbanity.1.99_weight_cov_bal.capped1 <- read.csv("results/covariate_balance_plots/filtered_FFL/continuous_exposure/csvs/state.urbanity.1.99_weight_cov_bal.csv")

temp1 <- state.1.99_match$cov_bal.capped0.99
temp1$Dataset[temp1$Dataset == "Unmatched"] <- "Unadjusted"
temp2 <- state.1.99_weight_cov_bal.capped0.99
temp2 <- temp2[temp2$Dataset == "Weighted", ]
temp2$X <- NULL
colnames(temp2)[3] <- "Absolute Correlation"
abs_cor <- rbind(temp1, temp2)

p <- ggplot(abs_cor, aes(x = `Absolute Correlation`, y = Covariate, color = Dataset, group = Dataset)) +
  geom_point() +
  geom_line(orientation = "y")
ggsave(paste0("results/covariate_balance_plots/filtered_FFL/continuous_exposure/all_methods_on_one_plot/state.1.99.capped99_all.cov.bal.png"), p)

##### AD HOC (to do: re-code this better) #####
##### Print results in LaTeX
all_results[Model %in% c("Naive Logistic", "Naive NegBin", "Adjust"), `:=`(Counter_Max = "-", Counter_Cap = "-")]
all_results[Cat_Covariates == "State" & Model == "Match" & Exposure_Range == formatted1.99, `:=`(Counter_Max = state.1.99$counter_max, Counter_Cap = state.1.99$counter99, Results = state.1.99$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State" & Model == "Match" & Exposure_Range == formatted5.95, `:=`(Counter_Max = state.5.95_match$counter_max, Counter_Cap = state.5.95_match$counter99, Results = state.5.95_match$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State" & Model == "Weight" & Exposure_Range == formatted5.95, `:=`(Counter_Max = state.5.95_weight$counter_max, Counter_Cap = state.5.95_weight$counter99, Results = state.5.95_weight$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State" & Model == "Weight" & Exposure_Range == formatted1.99, `:=`(Counter_Max = state.1.99_weight$counter_max, Counter_Cap = state.1.99_weight$counter99, Results = state.1.99_weight$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State" & Model == "Adjust" & Exposure_Range == formatted5.95, Results := "-0.0413 (0.0193) **"]
all_results[Cat_Covariates == "State" & Model == "Adjust" & Exposure_Range == formatted1.99, Results := state.1.99_adjust$logistic_regression_output]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Match" & Exposure_Range == formatted1.99, `:=`(Counter_Max = 55217, Counter_Cap = 480, Results = "-0.1088 (0.003) ***")]
# all_results[Cat_Covariates == "State + Urbanity" & Model == "Match" & Exposure_Range == formatted5.95, `:=`(Counter_Cap = 265.11, Results = "-0.0502 (0.0049) ***")]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Match" & Exposure_Range == formatted5.95, `:=`(Counter_Max = state.urbanity.5.95_match$counter_max, Counter_Cap = state.urbanity.5.95_match$counter99, Results = state.urbanity.5.95_match$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Weight" & Exposure_Range == formatted1.99, `:=`(Counter_Max = state.urbanity.1.99_weight$counter_max, Counter_Cap = state.urbanity.1.99_weight$counter99, Results = state.urbanity.1.99_weight$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Weight" & Exposure_Range == formatted5.95, `:=`(Counter_Max = state.urbanity.5.95_weight$counter_max, Counter_Cap = state.urbanity.5.95_weight$counter99, Results = state.urbanity.5.95_weight$logistic_regression_output_capped99)]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Adjust" & Exposure_Range == formatted5.95, Results := state.urbanity.5.95_adjust$logistic_regression_output]
all_results[Cat_Covariates == "State + Urbanity" & Model == "Adjust" & Exposure_Range == formatted1.99, Results := state.urbanity.1.99_adjust$logistic_regression_output]

# all_results$Counter_Max <- rep(c(state.1.99[["counter_max"]], state.5.95[["counter_max"]],
#                                  censusdiv.1.99[["counter_max"]], censusdiv.5.95[["counter_max"]],
#                                  state.urbanity.1.99[["counter_max"]], state.urbanity.5.95[["counter_max"]],
#                                  censusdiv.urbanity.1.99[["counter_max"]], censusdiv.urbanity.5.95[["counter_max"]]),
#                                each = 3)
# all_results$Counter_Cap <- c("NA", state.1.99[["counter99"]], state.1.99[["counter95"]],
#                              "NA", state.5.95[["counter99"]], state.5.95[["counter95"]],
#                              "NA", censusdiv.1.99[["counter99"]], censusdiv.1.99[["counter95"]],
#                              "NA", censusdiv.5.95[["counter99"]], censusdiv.5.95[["counter95"]],
#                              "NA", state.urbanity.1.99[["counter99"]], state.urbanity.1.99[["counter95"]],
#                              "NA", state.urbanity.5.95[["counter99"]], state.urbanity.5.95[["counter95"]],
#                              "NA", censusdiv.urbanity.1.99[["counter99"]], censusdiv.urbanity.1.99[["counter95"]],
#                              "NA", censusdiv.urbanity.5.95[["counter99"]], censusdiv.urbanity.5.95[["counter95"]])
# all_results$Logistic_Results <- c(state.1.99[["logistic_regression_output"]], state.1.99[["logistic_regression_output_capped99"]], state.1.99[["logistic_regression_output_capped95"]],
#                                   state.5.95[["logistic_regression_output"]], state.5.95[["logistic_regression_output_capped99"]], state.5.95[["logistic_regression_output_capped95"]],
#                                   censusdiv.1.99[["logistic_regression_output"]], censusdiv.1.99[["logistic_regression_output_capped99"]], censusdiv.1.99[["logistic_regression_output_capped95"]],
#                                   censusdiv.5.95[["logistic_regression_output"]], censusdiv.5.95[["logistic_regression_output_capped99"]], censusdiv.5.95[["logistic_regression_output_capped95"]],
#                                   state.urbanity.1.99[["logistic_regression_output"]], state.urbanity.1.99[["logistic_regression_output_capped99"]], state.urbanity.1.99[["logistic_regression_output_capped95"]],
#                                   state.urbanity.5.95[["logistic_regression_output"]], state.urbanity.5.95[["logistic_regression_output_capped99"]], state.urbanity.5.95[["logistic_regression_output_capped95"]],
#                                   censusdiv.urbanity.1.99[["logistic_regression_output"]], censusdiv.urbanity.1.99[["logistic_regression_output_capped99"]], censusdiv.urbanity.1.99[["logistic_regression_output_capped95"]],
#                                   censusdiv.urbanity.5.95[["logistic_regression_output"]], censusdiv.urbanity.5.95[["logistic_regression_output_capped99"]], censusdiv.urbanity.5.95[["logistic_regression_output_capped95"]])

xtable(all_results)

