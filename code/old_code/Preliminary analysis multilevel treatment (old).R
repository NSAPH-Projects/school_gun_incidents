################################################################################
## Nearest neighbor matching with replacement - continuous confounders

# Get discretized confounder matrix
data_discretized_treatment$x <- data_discretized_treatment[, all_confounder_names]
data_discretized_treatment$x <- t(apply(data_discretized_treatment$x, 1, unlist)) 

# Initialize matching weights and covariate balance plots
data_discretized_treatment$match.weights <- 1
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