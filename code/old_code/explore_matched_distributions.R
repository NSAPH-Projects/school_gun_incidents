# setwd("/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/")
source("code/make_discretized_datasets.R")

# get data, using >1 mi as cutoff for being treated
data_binary_exposure1.609km <- get_binary_exposure_continuous_confounders_dataset("mean_total_km", F, 1.609)

# without the following line, rownames are from before na.omit() in get_binary_exposure_continuous_confounders_dataset()
rownames(data_binary_exposure1.609km) <- 1:nrow(data_binary_exposure1.609km)
data_discretized1.609km <- decilize_quantitative_confounders(data_binary_exposure1.609km)

# match on PS
set.seed(42)

formula_matching <- as.formula(paste("a ~", paste(decilized_confounder_names, collapse = "+", sep = "")))
matched_pop <- matchit(formula_matching,
                       data = data_discretized1.609km,
                       estimand = "ATC",
                       method = "nearest",
                       replace = TRUE,
                       exact = c("census_division_number"))

# explore matches
print("In matched_pop$match.matrix, LHS (rownames) is the matched control unit and RHS is the matched treated unit")
head(matched_pop$match.matrix)
tail(matched_pop$match.matrix)
cat("Number of control units = Number of included/matched control units = Number of matches =", length(matched_pop$match.matrix))
cat("Number of matched treated units:", length(unique(matched_pop$match.matrix)))
cat("Number of units that are not matched (all are treated units):", sum(matched_pop$weights == 0))
cat("Max times any [included/matched] treated unit is matched:", max(table(matched_pop$match.matrix)))
cat("Min times any [included/matched] treated unit is matched:", min(table(matched_pop$match.matrix)))
print("Distribution of number of times any [included/matched] treated unit is matched")
quantile(as.numeric(table(matched_pop$match.matrix)), c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))

# explore distribution of outcome in matched control and treated units
cat("Out of", length(matched_pop$match.matrix), "matched control units,",
    sum(data_discretized1.609km$y[matched_pop$treat == 0]), "have the outcome")
cat("Out of", length(unique(matched_pop$match.matrix)), "matched treated units,",
    sum(data_discretized1.609km$y[matched_pop$treat == 1 & matched_pop$weights > 0]), "have the outcome")
