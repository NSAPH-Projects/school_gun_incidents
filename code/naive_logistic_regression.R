library(xtable)

source("make_discretized_datasets.R")

# get data for continuous treatment
cleaned_data <- read_cleaned_data_as_df()
data_analysis <- get_analysis_df(cleaned_data, treatment = "mean_total_km", all_confounder_names)
data_analysis <- na.omit(data_analysis)

# get data for continuous treatment trimmed at 95th percentile
data_trim_exposure <- data_analysis[which(data_analysis$a < quantile(data_analysis$a, 0.95)), ]

# get data for binary treatment with multiple alternative cutoffs
data_binary_exposure1km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1)
data_binary_exposure1.609km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1.609)
data_binary_exposure2km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2)
data_binary_exposure3.218km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 3.218)

# Functions to get naive logistic regression results

get_logistic_results <- function(df){
  logistic_model <- glm(y ~ ., 
                        data = df[, c("y", "a", all_confounder_names)], 
                        family = "binomial")
  summary(logistic_model)$coefficients
}

get_key_logistic_results <- function(df, var_names = c("a", "total_crime_2021", "total_population_2020", "mental_health_index", "prop_hunted_with_shotgun_2021", "log_median_hh_income")){
  logistic_results <- get_logistic_results(df)
  logistic_results_subset <- logistic_results[var_names, ]
  logistic_results_codified <- apply(logistic_results_subset, 1, concatenate_results)
  return(logistic_results_codified)
}

get_n_n0_n1 <- function(exposure_vec){
  n <- length(exposure_vec)
  
  if (length(unique(exposure_vec)) == 2){ # if exposure is binary
    n0 <- sum(exposure_vec == 0)
    n1 <- n - n0
  } else{ # if exposure is continuous
    n0 <- "N/A"
    n1 <- "N/A"
  }
  
  n_vec <- c(n, n0, n1)
  names(n_vec) <- c("n", "n_0", "n_1")
  return(n_vec)
}

# Make table of results

all_analyses_results <- sapply(list(data_analysis, data_trim_exposure,
                                    data_binary_exposure1km, data_binary_exposure1.609km, data_binary_exposure2km, data_binary_exposure3.218km),
                               get_key_logistic_results)
all_analyses_n_n0_n1 <- sapply(list(data_analysis$a, data_trim_exposure$a,
                                    data_binary_exposure1km$a, data_binary_exposure1.609km$a, data_binary_exposure2km$a, data_binary_exposure3.218km$a),
                               get_n_n0_n1)
all_analyses_table <- rbind(all_analyses_results, all_analyses_n_n0_n1)
colnames(all_analyses_table) <- c("Continuous", "Continuous trimmed", paste("geq", c("1 km", "1 mi", "2 km", "2 mi")))


# Convert to LaTeX
xtable(all_analyses_table)

