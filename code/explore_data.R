print("## Load packages ----")

library(data.table)
library(ggplot2)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- fread(paste0(dir, "data/intermediate/intermediate_data_aug2023.csv"))

qualitative_confounder_names <- c("census_division_number", "STATE_ABBR", "urban_rural")
exposure_names <- c("dist_closest_dealer", "dist_closest_commercial")
outcome_name <- "binary_shooting"
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

# Since cor(sports_MP33018a_B_P, sports_MP33017a_B_P) = 0.95, exclude sports_MP33017a_B_P


print("## Remove highly correlated (>= 0.95) confounders ----")

tracts_data_final <- copy(tracts_data)
quantitative_confounders_data_final <- copy(quantitative_confounders_data)
tracts_data_final[, `:=`(sports_MP33017a_B_P = NULL)]
quantitative_confounders_data_final[, `:=`(sports_MP33017a_B_P = NULL)]
quantitative_confounder_names_final <- quantitative_confounder_names[!(quantitative_confounder_names %in% c("sports_MP33017a_B_P"))]

# recalculate correlation matrix after removing highly correlated (>= 0.95) confounders
cor_matrix_final <- cor(as.matrix(quantitative_confounders_data_final))
cor_matrix_final <- round(cor_matrix_final, 2)
cor_matrix_final[upper.tri(cor_matrix_final, diag = T)] <- NA # set redundant values to NA; set the diagonal to NA too since it is equal to 1
cor_matrix_final <- melt(cor_matrix_final, na.rm = T)

cor_matrix_final_plot <- ggplot(cor_matrix_final, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", limit = c(-1,1)) +
  geom_text(aes(Var1, Var2, label = value), color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())
cor_matrix_final_plot


print("## Plot distribution of dist_closest_dealer ----")

summary(tracts_data_final$dist_closest_dealer)
mean(tracts_data_final$dist_closest_dealer, na.rm = T)
sd(tracts_data_final$dist_closest_dealer, na.rm = T)
hist(tracts_data_final$dist_closest_dealer,
     main = "Average Distance from Schools to Closest Firearms Dealer",
     xlab = "Miles")


print("## Plot distribution of dist_closest_commercial ----")

summary(tracts_data_final$dist_closest_commercial)
mean(tracts_data_final$dist_closest_commercial, na.rm = T)
sd(tracts_data_final$dist_closest_commercial, na.rm = T)
hist(tracts_data_final$dist_closest_commercial,
     main = "Average Distance from Schools to Closest Commercial Firearms Dealer",
     xlab = "Miles")


print("## Plot distribution of quantitative confounders ----")

for (var in quantitative_confounder_names_final){
  cat(var, "\n")
  cat("Mean:", mean(tracts_data_final[[var]]), "\n")
  cat("SD:", sd(tracts_data_final[[var]]), "\n\n")
}

# P0010001 
# Mean: 4210.206 
# SD: 1693.987 
# 
# populationtotals_DPOP_CY 
# Mean: 4280.126 
# SD: 3526.212 
# 
# hu_per_100_sqmi ### different from previous Table 1
# Mean: 191284.7 
# SD: 481055.6 
# 
# schools_per_100_sqmi ### different from previous Table 1
# Mean: 0.02124115 
# SD: 0.05373203 
# 
# area_sq_mile ### different from previous Table 1
# Mean: 43.15273 
# SD: 227.4726 
# 
# groupquarters_GQINST20_P ### different from previous Table 1
# Mean: 25.13192 
# SD: 39.85418 
# 
# prop_adult 
# Mean: 0.7783094 
# SD: 0.05433586 
# 
# householdincome_ACSSNAP_P 
# Mean: 12.35961 
# SD: 11.41292 
# 
# households_ACSPUBAI_P 
# Mean: 2.728706 
# SD: 3.167255 
# 
# households_ACSHHBPOV_P 
# Mean: 13.16409 
# SD: 10.27064 
# 
# EmploymentUnemployment_UNEMPRT_CY 
# Mean: 4.600719 
# SD: 3.699306 
# 
# EmploymentUnemployment_UNEMRT16CY 
# Mean: 9.742484 
# SD: 11.93386 
# 
# vehiclesavailable_ACSOVEH0_P 
# Mean: 3.852289 
# SD: 7.341542 
# 
# crime_CRMCYTOTC 
# Mean: 103.7511 
# SD: 68.02357 
# 
# MHLTH_CrudePrev 
# Mean: 15.13786 
# SD: 2.686491 
# 
# DEPRESSION_CrudePrev 
# Mean: 20.60004 
# SD: 3.550804 
# 
# educationalattainment_BACHDEG_CY_P 
# Mean: 20.99193 
# SD: 10.99163 
# 
# educationalattainment_GRADDEG_CY_P 
# Mean: 13.02408 
# SD: 10.69426 
# 
# firearm_retailers_per_100sqmi ### different from previous Table 1
# Mean: 48.97875 
# SD: 107.4379 
# 
# sports_MP33018a_B_P 
# Mean: 2.600491 
# SD: 1.390328 
# 
# prop_white 
# Mean: 0.628158 
# SD: 0.2654937 
# 
# prop_black 
# Mean: 0.1280754 
# SD: 0.2008618 
# 
# prop_asian 
# Mean: 0.05059295 
# SD: 0.08978894 
# 
# prop_multiracial 
# Mean: 0.09862712 
# SD: 0.07031812 
# 
# prop_hispanic 
# Mean: 0.1797497 
# SD: 0.2167559 
# 
# log_med_HH_income 
# Mean: 11.12088 
# SD: 0.7344127 
# 
# log_avg_HH_income_15to24 
# Mean: 10.88946 
# SD: 0.95539 
