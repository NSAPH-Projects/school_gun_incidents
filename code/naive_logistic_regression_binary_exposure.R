library(xtable)

source("make_discretized_datasets.R")

data_binary_exposure1km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1)
data_binary_exposure1.5km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1.5)
data_binary_exposure1.609km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1.609)
data_binary_exposure2km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2)
data_binary_exposure2.174km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2.174)
data_binary_exposure2.336km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2.336)
data_binary_exposure2.430km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2.430)
data_binary_exposure2.710km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 2.710)
data_binary_exposure3.218km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 3.218)

get_logistic_results <- function(df){
  logistic_model <- glm(y ~ ., 
                        data = df[, c("y", "a", all_confounder_names)], 
                        family = "binomial")
  summary(logistic_model)$coefficients[2, ]
}

all_analyses_results <- sapply(list(data_binary_exposure1km, data_binary_exposure1.5km, data_binary_exposure1.609km, data_binary_exposure2km,
                                    data_binary_exposure2.174km, data_binary_exposure2.336km, data_binary_exposure2.430km, data_binary_exposure2.710km,
                                    data_binary_exposure3.218km), get_logistic_results)
colnames(all_analyses_results) <- paste0("leq ", c(1, 1.5, 1.609, 2, 2.174, 2.336, 2.430, 2.710, 3.218))

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

all_analyses_results_rounded <- apply(all_analyses_results, c(1,2), function(vec) format(round(as.numeric(vec), 4), scientific = F))
all_analyses_results_rounded <- rbind(all_analyses_results_rounded, Significance = sapply(all_analyses_results_rounded["Pr(>|z|)", ], codify_significance))
all_analyses_results_rounded <- rbind(all_analyses_results_rounded, Results = paste0(all_analyses_results_rounded["Estimate", ], " (", all_analyses_results_rounded["Std. Error",], ") ", all_analyses_results_rounded["Significance",]))
all_analyses_results_table <- as.data.frame(all_analyses_results_rounded["Results", ])

# Create LaTeX table of results
xtable(all_analyses_results_table)

