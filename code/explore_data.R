print("## Load packages ----")
library(data.table)
library(ggplot2)


print("## Read data ----")
dir <- "../" # run code in the script location

tracts_data <- fread(paste0(dir, "data/intermediate/intermediate_cleaned_data_aug2023.csv"))

qualitative_confounder_names <- "STATE_ABBR"
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

# Note that cor(population_per_100sqmi, hu_per_100_sqmi) = 0.96, cor(sports_MP33018a_B_P, sports_MP33017a_B_P) = 0.95
# Therefore, exclude variables hu_per_100_sqmi, sports_MP33017a_B_P

print("## Remove highly correlated (>= 0.95) confounders ----")

tracts_data_cleaned <- copy(tracts_data)
quantitative_confounders_data_cleaned <- copy(quantitative_confounders_data)
tracts_data_cleaned[, `:=`(hu_per_100_sqmi = NULL,
                           sports_MP33017a_B_P = NULL)]
quantitative_confounders_data_cleaned[, `:=`(hu_per_100_sqmi = NULL,
                                             sports_MP33017a_B_P = NULL)]
quantitative_confounder_names <- quantitative_confounder_names[!(quantitative_confounder_names %in% c("hu_per_100_sqmi",
                                                                                                    "sports_MP33017a_B_P"))]

# # recalculate correlation matrix after removing highly correlated (>= 0.95) confounders
# cor_matrix_cleaned <- cor(as.matrix(quantitative_confounders_data_cleaned))
# cor_matrix_cleaned <- round(cor_matrix_cleaned, 2)
# cor_matrix_cleaned[upper.tri(cor_matrix_cleaned, diag = T)] <- NA # set redundant values to NA; set the diagonal to NA too since it is equal to 1
# cor_matrix_cleaned <- melt(cor_matrix_cleaned, na.rm = T)
# 
# cor_matrix_cleaned_plot <- ggplot(cor_matrix_cleaned, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "red", high = "blue", limit = c(-1,1)) +
#   geom_text(aes(Var1, Var2, label = value), color = "black") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank())


print("## Plot distribution of dist_closest_dealer ----")

summary(tracts_data_cleaned$dist_closest_dealer)
hist(tracts_data_cleaned$dist_closest_dealer)


print("## Plot distribution of dist_closest_commercial ----")

summary(tracts_data_cleaned$dist_closest_commercial)
hist(tracts_data_cleaned$dist_closest_commercial)


print("## Plot distribution of quantitative confounders ----")

for (var in quantitative_confounder_names){
  print(var)
  print(mean(tracts_data_cleaned[[var]]))
  print(sd(tracts_data_cleaned[[var]]))
}

