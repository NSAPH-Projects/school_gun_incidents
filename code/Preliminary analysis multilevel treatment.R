rm(list=ls())
setwd("/Users/falco//OneDrive - Harvard University/Research/Schools Vs Firearms")
source("code/helper_functions.R")
library(cobalt)
library(WeightIt)
library(jtools)
library(MatchIt)
library(cobalt)


set.seed(2022)
load_packages(clear_env = T)

#cleaned_data <- read.csv("data/all_tracts_2020_subset_vars.csv", header = TRUE, stringsAsFactors = FALSE)
#colnames(cleaned_data)
#dim(cleaned_data)

mental_health_data <-  read.csv("data/all_tracts_2020_subset_vars_inc_mental.csv", header = TRUE, stringsAsFactors = FALSE)

all_confounder_names <- c(all_confounder_names, "mental_health_index")
data_analysis <- get_analysis_df(mental_health_data, "mean_total_km", all_confounder_names)

data_analysis <- na.omit(data_analysis) # omit census tracts with no mental health index

# Discretize exposure into quartiles
summary(data_analysis$a)
quantile(data_analysis$a)
quantile(data_analysis$a[which(data_analysis$a < quantile(data_analysis$a, 0.95))])
data_discretized <- copy(data_analysis)
data_discretized$a <- ntile(data_discretized$a, 4)

data_discretized$census_division <- as.numeric(as.factor(data_discretized$census_division))
data_discretized$x =  data_discretized[, all_confounder_names]
data_discretized$x <- t(apply(data_discretized$x, 1, unlist)) 

################################################################################
## Matching 

treatments <- levels(as.factor(data_discretized$a)) #Levels of treatment variable
control <- "1" #Name of control level
data_discretized$match.weights <- 1 #Initialize matching weights

################################################################################
## Nearest neighbor matching with replacement
set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", exact = c("census_division"), 
               distance = "logit", replace = TRUE) # , ratio = 10
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}
summary(data_discretized$match.weights)

love.plot(m,
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1))

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
            data = data_discretized[data_discretized$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)

################################################################################
decilized_vars_list = c("total_population_2020", "housing_units_per_sq_meter",   
                        "log_median_hh_income", "schools_per_sq_meter",        
                        "log_median_hh_income_15to24", "total_crime_2021",
                        "dealers_per_sq_meter", "daytime_pop_2021",
                        "prop_white_only", "prop_black_only", "prop_asian_only",
                        "prop_multiracial", "prop_hispanic_latino", 
                        "prop_food_stamps_2019", "prop_public_assist_income_2019",
                        "prop_below_poverty_2019", "prop_without_vehicles_2019",    
                        "prop_hunted_with_shotgun_2021", "prop_bachelor_deg_25plus_2021",
                        "prop_grad_deg_25plus_2021", "prop_unemployed_2021",          
                        "prop_unemployed_16to24_2021", "prop_institutional_group",      
                        "prop_noninstitutional_group", "prop_18plus",
                        "mental_health_index") 

data_discretized <- data_discretized %>% 
  mutate_at(list(decile = ~ntile(., 10)), .vars = vars( decilized_vars_list ) )

decilized_confounder_names <- paste(all_confounder_names,"_decile", sep = "")
decilized_confounder_names <- gsub('census_division_decile', 'census_division', decilized_confounder_names)

data_discretized$x =  data_discretized[, decilized_confounder_names]
data_discretized$x <- t(apply(data_discretized$x, 1, unlist)) 

set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, decilized_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", exact = c("census_division", "dealers_per_sq_meter_decile"), caliper = 0.1, replace = TRUE) # method = "full"
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}
summary(data_discretized$match.weights)

love.plot(m,
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1))

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
## Caliper matching 0.01, replacement
set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, link = "logit", caliper = .01,replace = TRUE) # method = "full"
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}
summary(data_discretized$match.weights)

#Check balance using cobalt
bal.tab(a ~ x, data = data_discretized, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_discretized[data_discretized$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)

################################################################################
## Caliper matching 0.01, 2 replacement
set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, link = "logit", caliper = .01,replace = TRUE, ratio = 2) # method = "full"
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}
summary(data_discretized$match.weights)

#Check balance using cobalt
bal.tab(a ~ x, data = data_discretized, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

#Estimate treatment effects
summary(glm(y ~ relevel(as.factor(a), control),
            data = data_discretized[data_discretized$match.weights > 0,], 
            weights = match.weights),
        family = binomial(link = "logit"), robust = "HC1", digits = 5)


################################################################################
## Caliper matching 0.1, 10 replacements
set.seed(42)
for (i in treatments[treatments != control]) {
  d <- data_discretized[data_discretized$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, link = "logit", caliper = .01,
               ratio = 10, replace = TRUE) # method = "full"
  data_discretized[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
}
summary(data_discretized$match.weights)

#Check balance using cobalt
bal.tab(a ~ x, data = data_discretized, 
        weights = "match.weights", method = "matching", 
        focal = control, which.treat = .all)

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
for (i in treatments[treatments != control]) {
  d <- data_pharmacy[data_pharmacy$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", 
               distance = "logit", replace = TRUE) # method = "full"
  data_pharmacy[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
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
for (i in treatments[treatments != control]) {
  d <- data_grocery[data_grocery$a %in% c(i, control),] #Subset just the control and 1 treatment
  d$treat_i <- as.numeric(d$a != i) #Create new binary treatment variable
  d$x =  d[, all_confounder_names]
  d$x <- t(apply(d$x, 1, unlist)) 
  # m <- matchit(treat_i ~ .  -y -a -match.weights, data = d)
  m <- matchit(treat_i ~ x, data = d, method = "nearest", 
               distance = "logit", replace = TRUE) # method = "full"
  data_grocery[names(m$weights), "match.weights"] <- m$weights[names(m$weights)] #Assign matching weights
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
