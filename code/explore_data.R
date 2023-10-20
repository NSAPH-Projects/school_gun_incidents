print("## Load packages ----")

library(data.table)
library(ggplot2)
library(xtable)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- fread(paste0(dir, "data/intermediate/final_data_sep2023.csv"))

qualitative_confounder_names <- c("census_division_number", "State_Name", "urbanicity")
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
     main = "Average Distance from Schools to\nClosest Persistent Firearms Dealer",
     xlab = "Miles")

# > summary(tracts_data$mean_distance_all_persistent_dealers)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.7855   1.3527   2.0987   2.3770 100.2164 
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
     main = "Average Distance from Schools to\nClosest Persistent Commercial Firearms Dealer",
     xlab = "Miles")

# > summary(tracts_data$mean_dist_commercial_dealers)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.085   2.018   3.717   3.993 119.540 
# > mean(tracts_data$mean_dist_commercial_dealers, na.rm = T)
# [1] 3.716745
# > sd(tracts_data$mean_dist_commercial_dealers, na.rm = T)
# [1] 5.597451
# > quantile(tracts_data$mean_dist_commercial_dealers, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
#         1%         5%        95%        99% 
#  0.2518657  0.4712640 12.5540300 26.9261654 


print("## Plot distribution of quantitative confounders ----")

table1 <- data.table(Variable = quantitative_confounder_names)
table1[, `:=`(Mean = mean(tracts_data[[Variable]]),
              SD = sd(tracts_data[[Variable]]))]
rownames(table1) <- rep("", nrow(associational_results))
xtable(table1)

for (var in quantitative_confounder_names){
  cat(var, "\n")
  cat("Mean:", mean(tracts_data[[var]]), "\n")
  cat("SD:", sd(tracts_data[[var]]), "\n\n")
}

# populationtotals_TOTPOP20 
# Mean: 4212.992 
# SD: 1694.153 
# 
# populationtotals_DPOP_CY 
# Mean: 4283.304 
# SD: 3521.841 
# 
# householdincome_ACSSNAP_P 
# Mean: 12.33794 
# SD: 11.39585 
# 
# householdincome_ACSPUBAI_P 
# Mean: 2.726669 
# SD: 3.165389 
# 
# households_ACSHHBPOV_P 
# Mean: 13.14116 
# SD: 10.24911 
# 
# EmploymentUnemployment_UNEMP_CY_P 
# Mean: 4.597362 
# SD: 3.696481 
# 
# EmploymentUnemployment_UNAGE16CY_P 
# Mean: 1.352452 
# SD: 1.778859 
# 
# vehiclesavailable_ACSOVEH0_P 
# Mean: 3.846128 
# SD: 7.355989 
# 
# crime_CRMCYTOTC 
# Mean: 103.7414 
# SD: 68.08332 
# 
# educationalattainment_ACSBACHDEG_P 
# Mean: 19.53447 
# SD: 10.6577 
# 
# educationalattainment_ACSMASTDEG_P 
# Mean: 8.844534 
# SD: 6.904798 
# 
# sports_MP33018a_B_P 
# Mean: 2.60044 
# SD: 1.389605 
# 
# raceandhispanicorigin_WHITE20_P 
# Mean: 62.82134 
# SD: 26.53019 
# 
# raceandhispanicorigin_BLACK20_P 
# Mean: 12.78005 
# SD: 20.04002 
# 
# raceandhispanicorigin_ASIAN20_P 
# Mean: 5.068915 
# SD: 8.994996 
# 
# raceandhispanicorigin_RACE2UP20_P 
# Mean: 9.868876 
# SD: 7.030839 
# 
# hispanicorigin_HISPPOP20_P 
# Mean: 17.98938 
# SD: 21.67549 
# 
# percent_adult 
# Mean: 77.82323 
# SD: 5.425681 
# 
# area_sq_miles 
# Mean: 43.16498 
# SD: 227.5799 
# 
# mean_depression 
# Mean: 21.38104 
# SD: 3.617652 