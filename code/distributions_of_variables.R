# setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/helper_functions.R")

##### Get data and functions

df <- fread("data/all_tracts_2020_subset_vars_revised.csv")

# get dataset for main analysis
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
data_with_state_trimmed <- data_with_state[data_with_state$a >= exposure5.95[1] & data_with_state$a <= exposure5.95[2], ]
data_with_urbanity_state_trimmed <- data_with_urbanity_state[data_with_urbanity_state$a >= exposure5.95[1] & data_with_urbanity_state$a <= exposure5.95[2], ]

# get summary statistics of (binary) outcome and (continuous) exposure
nrow(data_with_state) # number of observations, i.e., census tracts
sum(data_with_state$y)
# mean(data_with_state$y)
min(data_with_state$a)
max(data_with_state$a)

nrow(data_with_state_trimmed) # number of observations, i.e., census tracts
sum(data_with_state_trimmed$y)
# mean(data_with_state_trimmed$y)
min(data_with_state_trimmed$a)
max(data_with_state_trimmed$a)

# get summary statistics of quantitative covariates
for (var in quantitative_confounders){
  print(var)
  
  print(mean(data_with_state[[var]]))
  print(sd(data_with_state[[var]]))
  
  print(mean(data_with_state_trimmed[[var]]))
  print(sd(data_with_state_trimmed[[var]]))
}

# get summary statistics of categorical covariates
prop.table(table(data_with_state$State_Name))
prop.table(table(data_with_urbanity_state$urban_rural)) # for sensitivity analysis using urbanity as covariate

prop.table(table(data_with_state_trimmed$State_Name))
prop.table(table(data_with_urbanity_state_trimmed$urban_rural)) # for sensitivity analysis using urbanity as covariate

