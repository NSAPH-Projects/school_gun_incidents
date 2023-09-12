print("## Load packages ----")

library(data.table)
library(ggplot2)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))

qualitative_confounder_names <- c("census_division_number", "State_Name", "urbanity")
exposure_names <- c("mean_distance_all_persistent_dealers", "mean_dist_commercial_dealers")
outcome_name <- "SGI"
quantitative_confounder_names <- colnames(tracts_data)[!(colnames(tracts_data) %in% c(qualitative_confounder_names,
                                                                                      exposure_names,
                                                                                      outcome_name))]

quantitative_confounders_data <- subset(tracts_data, select = quantitative_confounder_names)


print("## Calculate correlations between quantitative confounders ----")

cor_matrix <- cor(as.matrix(quantitative_confounders_data))
cor_matrix <- round(cor_matrix, 2)
cor_matrix[upper.tri(cor_matrix, diag = T)] <- NA # set redundant values to NA; set the diagonal to NA too since it is equal to 1
cor_matrix <- melt(cor_matrix, na.rm = T)

cor_matrix_plot <- ggplot(cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", limit = c(-1,1)) +
  geom_text(aes(Var1, Var2, label = value), color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())
cor_matrix_plot


print("## Plot distribution of mean_distance_all_persistent_dealers ----")

summary(tracts_data$mean_distance_all_persistent_dealers)
mean(tracts_data$mean_distance_all_persistent_dealers, na.rm = T)
sd(tracts_data$mean_distance_all_persistent_dealers, na.rm = T)
quantile(tracts_data$mean_distance_all_persistent_dealers, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
hist(tracts_data$mean_distance_all_persistent_dealers,
     main = "Average Distance from Schools to Closest Persistent Firearms Dealer",
     xlab = "Miles")

# > summary(tracts_data$mean_distance_all_persistent_dealers)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#   0.0000   0.7855   1.3527   2.0987   2.3769 100.2164       47 
# > mean(tracts_data$mean_distance_all_persistent_dealers, na.rm = T)
# [1] 2.09868
# > sd(tracts_data$mean_distance_all_persistent_dealers, na.rm = T)
# [1] 3.030087
# > quantile(tracts_data$mean_distance_all_persistent_dealers, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
#         1%         5%        95%        99% 
#  0.1969042  0.3675525  5.9500051 12.6856855 


print("## Plot distribution of mean_dist_commercial_dealers ----")

summary(tracts_data$mean_dist_commercial_dealers)
mean(tracts_data$mean_dist_commercial_dealers, na.rm = T)
sd(tracts_data$mean_dist_commercial_dealers, na.rm = T)
quantile(tracts_data$mean_dist_commercial_dealers, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
hist(tracts_data$mean_dist_commercial_dealers,
     main = "Average Distance from Schools to Closest Persistent Commercial Firearms Dealer",
     xlab = "Miles")

# > summary(tracts_data$mean_dist_commercial_dealers)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.085   2.018   3.717   3.993 119.540      49 
# > mean(tracts_data$mean_dist_commercial_dealers, na.rm = T)
# [1] 3.716745
# > sd(tracts_data$mean_dist_commercial_dealers, na.rm = T)
# [1] 5.597451
# > quantile(tracts_data$mean_dist_commercial_dealers, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
#         1%         5%        95%        99% 
#  0.2518657  0.4712640 12.5540300 26.9261654 


print("## Plot distribution of quantitative confounders ----")

for (var in quantitative_confounder_names){
  cat(var, "\n")
  cat("Mean:", mean(tracts_data[[var]]), "\n")
  cat("SD:", sd(tracts_data[[var]]), "\n\n")
}

# populationtotals_TOTPOP20 
# Mean: 4212.885 
# SD: 1694.07 
# 
# populationtotals_DPOP_CY 
# Mean: 4282.519 
# SD: 3520.787 
# 
# groupquarters_GQINST20_P 
# Mean: 25.12312 
# SD: 39.85036 
# 
# householdincome_ACSSNAP_P 
# Mean: 12.33508 
# SD: 11.39345 
# 
# householdincome_ACSPUBAI_P 
# Mean: 2.725975 
# SD: 3.164656 
# 
# households_ACSHHBPOV_P 
# Mean: 13.1396 
# SD: 10.24804 
# 
# EmploymentUnemployment_UNEMP_CY_P 
# Mean: 4.596308 
# SD: 3.695939 
# 
# EmploymentUnemployment_UNAGE16CY_P 
# Mean: 1.352279 
# SD: 1.778503 
# 
# vehiclesavailable_ACSOVEH0_P 
# Mean: 3.84418 
# SD: 7.353378 
# 
# vehiclesavailable_ACSRVEH0_P 
# Mean: 13.79859 
# SD: 15.30456 
# 
# crime_CRMCYTOTC 
# Mean: 103.7272 
# SD: 68.08008 
# 
# educationalattainment_ACSBACHDEG_P 
# Mean: 19.53411 
# SD: 10.65795 
# 
# educationalattainment_ACSMASTDEG_P 
# Mean: 8.845186 
# SD: 6.905343 
# 
# sports_MP33018a_B_P 
# Mean: 2.60036 
# SD: 1.389531 
# 
# raceandhispanicorigin_WHITE20_P 
# Mean: 62.82788 
# SD: 26.52726 
# 
# raceandhispanicorigin_BLACK20_P 
# Mean: 12.77639 
# SD: 20.03504 
# 
# raceandhispanicorigin_ASIAN20_P 
# Mean: 5.068195 
# SD: 8.993349 
# 
# raceandhispanicorigin_RACE2UP20_P 
# Mean: 9.867802 
# SD: 7.028816 
# 
# hispanicorigin_HISPPOP20_P 
# Mean: 17.98622 
# SD: 21.6697 
# 
# population_adult 
# Mean: 77.82567 
# SD: 5.427749 
# 
# area_sq_miles 
# Mean: 43.1599 
# SD: 227.5018 
# 
# mean_depression 
# Mean: 21.38128 
# SD: 3.618051 
# 
# mean_distress 
# Mean: 15.90269 
# SD: 3.344148 
# 
# housing_per_100sqmi 
# Mean: 191052.5 
# SD: 479962.8 
# 
# schools_per_100sqmi 
# Mean: 212.6955 
# SD: 534.9909 
# 
# firearm_retailers_per_100sqmi 
# Mean: 30.47063 
# SD: 75.99728 
# 
# log_med_HH_income 
# Mean: 11.0791 
# SD: 0.7506252 
# 
# log_med_HH_income_15to24 
# Mean: 7.298514 
# SD: 6.27241 
