##### Set up #####

# library(gnm)

setwd("~/OneDrive - Harvard University/Research/Schools Vs Firearms")
# setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")


##### Load packages and get data #####

source("code/make_discretized_datasets.R")
load_packages()

cleaned_data <- read_cleaned_data_as_df()
data_analysis <- get_analysis_df(cleaned_data, treatment = "mean_total_km", all_confounder_names)
data_analysis <- na.omit(data_analysis)
data_trim_exposure <- data_analysis[which(data_analysis$a < quantile(data_analysis$a, 0.95)), ]


##### Run a spline regression, with and without the covariates #####

#### spline-lm-3knots ####
# Note: this is a linear model, not logistic

a_lims <- range(data_trim_exposure$a)
a_grid <- seq(a_lims[1], a_lims[2], length.out = 100)

spline1 <- lm(y ~ bs(a, k=3), data = data_trim_exposure)
spline_pred1 <- predict(spline1, newdata = list(a = a_grid), se = T)
plot(a_grid, spline_pred1$fit, type = "l", col = "red", lwd = 3)
lines(a_grid, spline_pred1$fit + 2*spline_pred1$se.fit, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, spline_pred1$fit - 2*spline_pred1$se.fit, lty = "dashed", lwd = 2, col = "green")
cat("Max occurs at", a_grid[which.max(spline_pred1$fit)], "km")


#### spline-lm-5knots ####
# Note: this is a linear model, not logistic

spline1_5knots <- lm(y ~ bs(a, k=5), data = data_trim_exposure)
spline_pred1_5knots <- predict(spline1_5knots, newdata = list(a = a_grid), se = T)
plot(a_grid, spline_pred1_5knots$fit, type = "l", col = "red", lwd = 3)
lines(a_grid, spline_pred1_5knots$fit + 2*spline_pred1_5knots$se.fit, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, spline_pred1_5knots$fit - 2*spline_pred1_5knots$se.fit, lty = "dashed", lwd = 2, col = "green")
cat("Max occurs at", a_grid[which.max(spline_pred1_5knots$fit)], "km")


#### spline-mgcv-bam-3-knots #### 

# model 1: exposure only

# plot spline
spline_exposure_only <- mgcv::bam(y~ s(a, bs='cr', k=3), data = data_trim_exposure, family=binomial(link="logit"))
plot(spline_exposure_only)

# calculate global max (change point) and generate binary exposure dataset using that as cutoff
plot <- plot(spline_exposure_only)
cutoff <- plot[[1]]$x[which.max(plot[[1]]$fit)]
# data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, cutoff)

# plot predicted log odds
pred_spline_exposure_only <- predict(spline_exposure_only, newdata = list(a = a_grid), se = T)
plot(a_grid, pred_spline_exposure_only$fit, type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (km)", ylab = "Log odds (log(p/(1-p))")
lines(a_grid, pred_spline_exposure_only$fit + 2*pred_spline_exposure_only$se.fit, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, pred_spline_exposure_only$fit - 2*pred_spline_exposure_only$se.fit, lty = "dashed", lwd = 2, col = "green")

# calculate bootstrap SE estimate for predicted probability
set.seed(100)
B <- 50 # number of bootstrap samples
n <- nrow(data_trim_exposure)
boot_indices <- matrix(sample(1:n, n * B, replace=T), nrow=B, ncol=n)
boot_a <- apply(boot_indices, c(1,2), function(i) data_trim_exposure$a[i])
boot_y <- apply(boot_indices, c(1,2), function(i) data_trim_exposure$y[i])

inverse_logit_spline_exposure_only_boot <- function(boot_row, n_knots){
  df <- data.frame(a = boot_a[boot_row, ], y = boot_y[boot_row, ])
  spline <- mgcv::bam(y~ s(a, bs='cr', k=n_knots), data = df, family=binomial(link="logit"))
  pred <- predict(spline, newdata = list(a = a_grid), se = F)
  return(inverse_logit(pred))
}

boot_thetas <- sapply(1:B, inverse_logit_spline_exposure_only_boot, n_knots = 3)
boot_se <- apply(boot_thetas, 1, sd) # for each value of a in a_grid, calculate sd(boot_thetas)

# plot predicted probability
plot(a_grid, inverse_logit(pred_spline_exposure_only$fit), type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (km)", ylab = "Probability of school shooting")
lines(a_grid, inverse_logit(pred_spline_exposure_only$fit) + 2*boot_se, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, inverse_logit(pred_spline_exposure_only$fit) - 2*boot_se, lty = "dashed", lwd = 2, col = "green")

# model 2: exposure and Census division number (1-9)

spline_exposure_and_division <- mgcv::bam(y~ s(a, bs='cr', k=3) + census_division_number, data = data_trim_exposure, family=binomial(link="logit"))
plot(spline_exposure_and_division)

# model 3: all covariates

# note: prop_without_vehicles_2019 excluded because it was giving error: longer object length is not a multiple of shorter object length
# Error in while (dev0 + sum(pcoef0 * Sb0) < dev1 + sum(pcoef * Sb) && kk <  : 
#   missing value where TRUE/FALSE needed
spline_exposure_with_covariates <- mgcv::bam(y ~ s(a, bs='cr', k=3) + census_division_number + total_population_2020 + housing_units_per_sq_meter + Tract_Area_sq_meters + log_median_hh_income + schools_per_sq_meter + log_median_hh_income_15to24 + total_crime_2021 + dealers_per_sq_meter + mental_health_index + daytime_pop_2021 + prop_white_only + prop_black_only + prop_asian_only + prop_multiracial + prop_hispanic_latino + prop_food_stamps_2019 + prop_public_assist_income_2019 + prop_below_poverty_2019 + prop_hunted_with_shotgun_2021 + prop_bachelor_deg_25plus_2021 + prop_grad_deg_25plus_2021 + prop_unemployed_2021 + prop_unemployed_16to24_2021 + prop_institutional_group + prop_noninstitutional_group + prop_18plus, data = data_trim_exposure, family=binomial(link="logit"))
plot(spline_exposure_with_covariates)

# note: when running model on data_analysis (ie, untrimmed exposure), exclude prop_hunted_with_shotgun_2021
# and prop_unemployed_16to24_2021 as well, for same error

# calculate global max (change point) and generate binary exposure dataset using that as cutoff
plot <- plot(spline_exposure_with_covariates)
cutoff <- plot[[1]]$x[which.max(plot[[1]]$fit)]
# data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, cutoff)


#### spline-mgcv-bam-5-knots #### 

# model 1: exposure only

spline_exposure_only_5knots <- mgcv::bam(y~ s(a, bs='cr', k=5), data = data_trim_exposure, family=binomial(link="logit"))
plot <- plot(spline_exposure_only_5knots)
cat("Max occurs at", plot[[1]]$x[which.max(plot[[1]]$fit)], "km")

# plot predicted log odds
pred_spline_exposure_only_5knots <- predict(spline_exposure_only_5knots, newdata = list(a = a_grid), se = T)
plot(a_grid, pred_spline_exposure_only_5knots$fit, type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (km)", ylab = "Log odds (log(p/(1-p))")
lines(a_grid, pred_spline_exposure_only_5knots$fit + 2*pred_spline_exposure_only_5knots$se.fit, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, pred_spline_exposure_only_5knots$fit - 2*pred_spline_exposure_only_5knots$se.fit, lty = "dashed", lwd = 2, col = "green")

# plot predicted probability
boot_thetas_5knots <- sapply(1:B, inverse_logit_spline_exposure_only_boot, n_knots = 5)
boot_se_5knots <- apply(boot_thetas_5knots, 1, sd) # check margin
plot(a_grid, inverse_logit(pred_spline_exposure_only_5knots$fit), type = "l", col = "red", lwd = 3, xlab = "Mean minimum distance to gun dealer (km)", ylab = "Probability of school shooting")
lines(a_grid, inverse_logit(pred_spline_exposure_only_5knots$fit) + 2*boot_se, lty = "dashed", lwd = 2, col = "green")
lines(a_grid, inverse_logit(pred_spline_exposure_only_5knots$fit) - 2*boot_se, lty = "dashed", lwd = 2, col = "green")

# model 2: exposure and Census division number (1-9)

spline_exposure_and_division_5knots <- mgcv::bam(y~ s(a, bs='cr', k=5) + census_division_number, data = data_trim_exposure, family=binomial(link="logit"))
plot(spline_exposure_and_division_5knots)

# model 3: all covariates

# note: prop_without_vehicles_2019 excluded because it was giving error: longer object length is not a multiple of shorter object length
# Error in while (dev0 + sum(pcoef0 * Sb0) < dev1 + sum(pcoef * Sb) && kk <  : 
#   missing value where TRUE/FALSE needed
spline_exposure_with_covariates_5knots <- mgcv::bam(y ~ s(a, bs='cr', k=5) + census_division_number + total_population_2020 + housing_units_per_sq_meter + Tract_Area_sq_meters + log_median_hh_income + schools_per_sq_meter + log_median_hh_income_15to24 + total_crime_2021 + dealers_per_sq_meter + mental_health_index + daytime_pop_2021 + prop_white_only + prop_black_only + prop_asian_only + prop_multiracial + prop_hispanic_latino + prop_food_stamps_2019 + prop_public_assist_income_2019 + prop_below_poverty_2019 + prop_hunted_with_shotgun_2021 + prop_bachelor_deg_25plus_2021 + prop_grad_deg_25plus_2021 + prop_unemployed_2021 + prop_unemployed_16to24_2021 + prop_institutional_group + prop_noninstitutional_group + prop_18plus, data = data_trim_exposure, family=binomial(link="logit"))
plot(spline_exposure_with_covariates_5knots)

# calculate global max (change point) and generate binary exposure dataset using that as cutoff
plot <- plot(spline_exposure_with_covariates_5knots)
cutoff <- plot[[1]]$x[which.max(plot[[1]]$fit)]
# data_binary_exposure <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, cutoff)


####  spline-gnm #### 
# To Do: Fix error (Error in s(a, bs = "cr", k = 3) : unused arguments (bs = "cr", k = 3))
# gnm_spline_exposure_only <- gnm(y~ s(a, bs='cr', k=3), data = data_trim_exposure, family=binomial(link="logit"))
# plot(gnm_spline_exposure_only)


##### THE FOLLOWING IS NOT A SPLINE; JUST EXPLORATORY #####

# Plot density of exposure (trimmed at 95th percentile), subsetting by outcome.
# Note: Green is outcome == 1, Blue is outcome == 0

ffx <- density(data_trim_exposure$a[data_trim_exposure$y == 0])$x
ff0 <- density(data_trim_exposure$a[data_trim_exposure$y == 0])$y
ff1 <- density(data_trim_exposure$a[data_trim_exposure$y == 1])$y
plot(ffx, ff1, xlab = "Mean Minimum Distance to Gun Dealer (km)", ylab = "Density", main = "", col = "green", type = "l", lwd = 2)
lines(ffx, ff0, col = "blue", lwd = 2)
# rug(data_trim_exposure$a[data_trim_exposure$y == 0], ticksize = 0.1, lwd = 2, col = "blue") # too crowded
rug(data_trim_exposure$a[data_trim_exposure$y == 1], ticksize = 0.05, lwd = 2, col = "green")
legend("topright", col = c("green", "blue"), legend = c("At least 1 school shooting", "No school shootings"), lty = 1)


