## Correlation functions ----

get_abs_cor <- function(w, binary_cov){
  return(abs(cor(w, binary_cov)))
}

# Calculate mean of absolute point-biserial correlation between continuous exposure and each binary indicator for an unordered categorical covariate 
# params: w is the vector of continuous exposure, unordered_var is vector of unordered categorical covariate 
cor_unordered_var <- function(w, unordered_var){
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  abs_cor <- lapply(binary_indicators, get_abs_cor, w = w)
  return(mean(unlist(abs_cor)))
}

weighted_cor_unordered_var <- function(w, unordered_var, weights){
  library(wCorr)
  
  levels <- levels(unordered_var) # assumes unordered_var is already a factor, as it should be to be entered into generate_pseudo_pop()
  binary_indicators <- lapply(levels, function(i) 1*(unordered_var == i))
  weighted_cor <- lapply(binary_indicators, weightedCorr, y = w, method = "Pearson", weights = weights)
  abs_weighted_cor <- lapply(weighted_cor, abs)
  return(mean(unlist(abs_weighted_cor)))
}

make_correlation_plot <- function(abs_cor_table){
  ggplot(abs_cor_table, aes(x = `Absolute Correlation`, y = Covariate, color = Dataset, group = Dataset)) +
    geom_point() +
    geom_line(orientation = "y")
}