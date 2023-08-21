print("## Load packages ----")

library(data.table)
library(ggplot2)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- fread(paste0(dir, "data/intermediate/intermediate_data_aug2023.csv"))

qualitative_confounder_names <- c("census_division_number", "urban_rural")
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
hist(tracts_data_final$dist_closest_dealer)


print("## Plot distribution of dist_closest_commercial ----")

summary(tracts_data_final$dist_closest_commercial)
hist(tracts_data_final$dist_closest_commercial)


print("## Plot distribution of quantitative confounders ----")

for (var in quantitative_confounder_names_final){
  print(var)
  print(mean(tracts_data_final[[var]]))
  print(sd(tracts_data_final[[var]]))
}

# [1] "P0010001"
# [1] 4210.206
# [1] 1693.987
# [1] "populationtotals_DPOP_CY"
# [1] 4280.126
# [1] 3526.212
# [1] "hu_per_100_sqmi" ### different from previous result
# [1] 191284.7
# [1] 481055.6
# [1] "schools_per_100_sqmi" ### different from previous result
# [1] 0.02124115
# [1] 0.05373203
# [1] "area_sq_mile" ### different from previous result
# [1] 43.15273
# [1] 227.4726
# [1] "groupquarters_GQINST20_P" ### different from previous result
# [1] 25.13192
# [1] 39.85418
# [1] "prop_adult"
# [1] 0.7783094
# [1] 0.05433586
# [1] "householdincome_ACSSNAP_P"
# [1] 12.35961
# [1] 11.41292
# [1] "households_ACSPUBAI_P"
# [1] 2.728706
# [1] 3.167255
# [1] "households_ACSHHBPOV_P"
# [1] 13.16409
# [1] 10.27064
# [1] "EmploymentUnemployment_UNEMPRT_CY"
# [1] 4.600719
# [1] 3.699306
# [1] "EmploymentUnemployment_UNEMRT16CY"
# [1] 9.742484
# [1] 11.93386
# [1] "vehiclesavailable_ACSOVEH0_P"
# [1] 3.852289
# [1] 7.341542
# [1] "crime_CRMCYTOTC"
# [1] 103.7511
# [1] 68.02357
# [1] "MHLTH_CrudePrev"
# [1] 15.13786
# [1] 2.686491
# [1] "DEPRESSION_CrudePrev"
# [1] 20.60004
# [1] 3.550804
# [1] "educationalattainment_BACHDEG_CY_P"
# [1] 20.99193
# [1] 10.99163
# [1] "educationalattainment_GRADDEG_CY_P"
# [1] 13.02408
# [1] 10.69426
# [1] "firearm_retailers_per_100sqmi" ### different from previous result
# [1] 48.97875
# [1] 107.4379
# [1] "sports_MP33018a_B_P"
# [1] 2.600491
# [1] 1.390328
# [1] "prop_white"
# [1] 0.628158
# [1] 0.2654937
# [1] "prop_black"
# [1] 0.1280754
# [1] 0.2008618
# [1] "prop_asian"
# [1] 0.05059295
# [1] 0.08978894
# [1] "prop_multiracial"
# [1] 0.09862712
# [1] 0.07031812
# [1] "prop_hispanic"
# [1] 0.1797497
# [1] 0.2167559
# [1] "log_med_HH_income"
# [1] 11.12088
# [1] 0.7344127
# [1] "log_avg_HH_income_15to24"
# [1] 10.88946
# [1] 0.95539
