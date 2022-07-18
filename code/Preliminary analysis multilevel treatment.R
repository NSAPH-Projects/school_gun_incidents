rm(list=ls())
set.seed(2022)

setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
# setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# Get datasets (continuous and discrete versions)
data_analysis <- get_undiscretized_dataset()

#census_regions <- rep(NA, nrow(data_analysis))
#census_regions[which(data_analysis$census_division_number == 1 | data_analysis$census_division_number == 2)] <- 1
#census_regions[which(data_analysis$census_division_number == 3 | data_analysis$census_division_number == 4)] <- 2
#census_regions[which(data_analysis$census_division_number == 5 | data_analysis$census_division_number == 6 | data_analysis$census_division_number == 7)] <- 3
#census_regions[which(data_analysis$census_division_number == 8 | data_analysis$census_division_number == 9)] <- 4
#table(census_regions)
#data_analysis$census_region <- census_regions

data_discretized_treatment <- discretize_treatment(data_analysis)
data_discretized <- decilize_quantitative_confounders(data_discretized_treatment)

# Explore continuous exposure distribution
summary(data_analysis$a)
quantile(data_analysis$a)
quantile(data_analysis$a[which(data_analysis$a < quantile(data_analysis$a, 0.95))])

# Get discretized confounder matrix
data_discretized_treatment$x <- data_discretized_treatment[, all_confounder_names]
data_discretized_treatment$x <- t(apply(data_discretized_treatment$x, 1, unlist)) 

# Initialize data frame tracking weights for every observation
all_analyses_weights <- data.frame(discretized_treatment = rep(NA, nrow(data_analysis)))

################################################################################
## Matching to Find the Average Treatment Effect on the Control (ATC)

treatments <- levels(as.factor(data_discretized_treatment$a)) #Levels of treatment variable
control <- "1" #Name of control level
data_discretized_treatment$match.weights <- 1 #Initialize matching weights

################################################################################
## Nearest neighbor matching with replacement - continuous confounders
cov_bal_plots <- list()

set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized_treatment[data_discretized_treatment$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", exact = c("census_division_number"), replace = TRUE) # , ratio = 10
  data_discretized_treatment[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
  
  cov_bal_plots[[i]] <- love.plot(m,
                          drop.distance = TRUE, 
                          var.order = "unadjusted",
                          abs = TRUE,
                          line = TRUE, 
                          thresholds = c(m = .1))  +
    ggtitle(paste("Covariate Balance - Group", i, "vs Group 1"))
}
summary(data_discretized_treatment$match.weights)
all_analyses_weights$discretized_treatment <- data_discretized_treatment$match.weights # save weights in all_analyses data frame

cov_bal_plots[["2"]]
cov_bal_plots[["3"]]
cov_bal_plots[["4"]]

#Check balance using cobalt
bal.matched <- bal.tab(a ~ x, data = data_discretized_treatment, 
                       weights = "match.weights", method = "matching", 
                       focal = control, which.treat = .all)

bal.unmatched <- bal.tab(a ~ x, data = data_discretized_treatment, 
                         weights = rep(1, nrow(data_discretized_treatment)), method = "matching", 
                         focal = control, which.treat = .all)

cor_val_matched <- abs(bal.matched$Balance$Corr.Adj)
cor_val_unmatched <- abs(bal.unmatched$Balance$Corr.Adj)


# gather correlations into 1 data frame
abs_cor = data.frame(cov = all_confounder_names,
                     unmatched = cor_val_unmatched,
                     matched = cor_val_matched) %>%
  gather(c(unmatched, matched), key = 'dataset', value = 'absolute correlation')

# make plot
p <- ggplot(abs_cor, aes(x = cov, y = `absolute correlation`, color = dataset, group = dataset)) +
  geom_hline(yintercept= 0.1) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90))

p

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_discretized_treatment[data_discretized_treatment$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)

################################################################################
## Nearest neighbor matching with replacement - DISCRETE confounders
cov_bal_plots <- list()
data_discretized$x =  data_discretized[, decilized_confounder_names]
data_discretized$x <- t(apply(data_discretized$x, 1, unlist)) 

set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, decilized_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", exact = c("census_division_number", "dealers_per_sq_meter_decile",
                                                                    "log_median_hh_income_decile"),  replace = TRUE) 
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
  
  cov_bal_plots[[i]] <- love.plot(m,
                                  drop.distance = TRUE, 
                                  var.order = "unadjusted",
                                  abs = TRUE,
                                  line = TRUE, 
                                  thresholds = c(m = .1))  +
    ggtitle(paste("Covariate Balance - Group", i, "vs Group 1"))
}
summary(data_discretized$match.weights)
all_analyses_weights$discrete_confounders <- data_discretized$match.weights # save weights in all_analyses data frame

cov_bal_plots[["2"]]
cov_bal_plots[["3"]]
cov_bal_plots[["4"]]

#Check balance using cobalt
bal.matched <- bal.tab(a ~ x, data = data_discretized, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

bal.unmatched <- bal.tab(a ~ x, data = data_discretized, 
        weights = rep(1, nrow(data_discretized)), method = "matching", 
        focal = control, which.treat = .all)
 
 cor_val_matched <- abs(bal.matched$Balance$Corr.Adj)
 cor_val_unmatched <- abs(bal.unmatched$Balance$Corr.Adj)

 
 # gather correlations into 1 data frame
 abs_cor = data.frame(cov = decilized_confounder_names,
                      unmatched = cor_val_unmatched,
                      matched = cor_val_matched) %>%
   gather(c(unmatched, matched), key = 'dataset', value = 'absolute correlation')
 
 # make plot
 p <- ggplot(abs_cor, aes(x = cov, y = `absolute correlation`, color = dataset, group = dataset)) +
   geom_hline(yintercept= 0.1) +
   geom_point() +
   geom_line() +
   theme(axis.text.x = element_text(angle = 90))
 
 p

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_discretized[data_discretized$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)


################################################################################
###### Pharmacy

data_pharmacy <- get_analysis_df(cleaned_data, "mean_pharmacy_km", all_confounder_names)
data_pharmacy <- na.omit(data_pharmacy)
quantile(data_pharmacy$a)

# Trim exposure at 95th percentile, then discretize into quartiles
data_pharmacy$a <- ntile(data_pharmacy$a, 4)
treatments <- levels(as.factor(data_pharmacy$a)) #Levels of treatment variable
control <- "1" #Name of control level
data_pharmacy$match.weights <- 1 #Initialize matching weights

################################################################################
## Nearest neighbor matching with replacement
set.seed(42)
cov_bal_plots <- list()
for (i in treatments[treatments != control]) {
  d <- data_pharmacy[data_pharmacy$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", 
               distance = "logit", replace = TRUE) # method = "full"
  data_pharmacy[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
  
  cov_bal_plots[[i]] <- love.plot(m,
                                  drop.distance = TRUE, 
                                  var.order = "unadjusted",
                                  abs = TRUE,
                                  line = TRUE, 
                                  thresholds = c(m = .1))  +
    ggtitle(paste("Covariate Balance - Group", i, "vs Group 1"))
}
summary(data_pharmacy$match.weights)

#Check balance using cobalt
bal.tab(a ~ x, data = data_pharmacy, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_pharmacy[data_pharmacy$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)

################################################################################
###### Grocery

data_grocery <- get_analysis_df(cleaned_data, "mean_grocery_km", all_confounder_names)
data_grocery <- na.omit(data_grocery)
quantile(data_grocery$a)

# Trim exposure at 95th percentile, then discretize into quartiles
data_grocery$a <- ntile(data_grocery$a, 4)
treatments <- levels(as.factor(data_grocery$a)) #Levels of treatment variable
control <- "1" #Name of control level
data_grocery$match.weights <- 1 #Initialize matching weights

################################################################################
## Nearest neighbor matching with replacement
set.seed(42)
cov_bal_plots <- list()
for (i in treatments[treatments != control]) {
  d <- data_grocery[data_grocery$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", 
               distance = "logit", replace = TRUE) # method = "full"
  data_grocery[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
  
  cov_bal_plots[[i]] <- love.plot(m,
                                  drop.distance = TRUE, 
                                  var.order = "unadjusted",
                                  abs = TRUE,
                                  line = TRUE, 
                                  thresholds = c(m = .1))  +
    ggtitle(paste("Covariate Balance - Group", i, "vs Group 1"))
}
summary(data_grocery$match.weights)

#Check balance using cobalt
bal.tab(a ~ x, data = data_grocery, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_grocery[data_grocery$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)



################################################################################
## Weighting
w.out <- weightit(as.factor(a) ~ x, data = data_discretized, focal = "1", estimand = "ATT")

#Check balance
summary(w.out$weights)
bal.tab(w.out, which.treat = .all)

#Estimate treatment effects (using jtools to get robust SEs)
#(Can also use survey package)
summ(glm(y ~ relevel(as.factor(a), "1"), data = data_discretized[which(w.out$weights <= 1),], weights = w.out$weights[which(w.out$weights <= 1)], family = binomial(link = "logit")), robust = "HC1", digits = 5) # 


################################################################################
## Multilevel matching package

library(multilevelMatching)
simulated_data <- multilevelMatching::simulated_data
knitr::kable(head(simulated_data), digits = 2)


outcome <- simulated_data$outcome
#treatment <- simulated_data$treatment
treatment <- rep(c(1:4), nrow(simulated_data)/4)

covar_matrix <- as.matrix(
  simulated_data[ ,names(simulated_data) %in% paste0("covar", 1:6)]
)
identifying_names <- paste0(
  rep(letters[1:25],each = 12), rep(letters[1:25], 12)
)
names(treatment) <- identifying_names

# Matching on Covariates
set.seed(123)
fit <- multiMatch(
  Y = data_discretized$y,
  W = data_discretized$a,
  X = data_discretized$x,
  match_on = "covariates"
)
fit



# Matching on GPS
match_on <- "multinom"
set.seed(123)
fit2 <- multiMatch(
  Y = data_discretized$y,
  W = data_discretized$a,
  X = data_discretized$x,
  match_on = match_on,
  trimming = FALSE
)
fit2
