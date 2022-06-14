library(data.table)

dir <- "Harvard University/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"
all_tracts_2020_subset_vars <- fread(paste0(dir, "data/all_tracts_2020_subset_vars.csv"))
shooting_tracts_2020_subset_vars <- fread(paste0(dir, "data/shooting_tracts_2020_subset_vars.csv"))

# function to map 1 variable to {1,2,3,4,5} by quantile
discretize_1_var <- function(var){
  quantiles <- quantile(var, probs = c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
  
  var_discretized <- ifelse(var <= quantiles[1], 1,
                            ifelse(var <= quantiles[2], 2,
                                   ifelse(var <= quantiles[3], 3,
                                          ifelse(var <= quantiles[4], 4, 5))))
  return(var_discretized)
}

# function to map variables to {1,2,3,4,5} by quantile
# variables may be excluded from the discretization process; if so their names should be given as a vector
# this function saves resulting data.table to output_filename if given
discretize_vars <- function(dt, exclude_vars = NA, output_filename){
  if (!is.na(exclude_vars)){
    dt2 <- subset(dt, select = names(dt)[!names(dt) %in% exclude_vars]) # if exclude_vars is specified, don't discretize it/them
  } else{
    dt2 <- dt
  }
  
  for (var in names(dt2)){
    dt2[[var]] <- discretize_1_var(dt2[[var]]) # discretize each variable remaining
  }
  
  if (!is.na(exclude_vars)){
    for (var in exclude_vars){
      dt2[[var]] <- dt[[var]] # add unmodified variable(s) back to data.table
    }
  }
  
  if (!is.na(output_filename)){
    fwrite(dt2, output_filename)
  }
  return(dt2)
}

all_tracts_2020_discretized <- discretize_vars(all_tracts_2020_subset_vars, "binary_shooting_incident", paste0(dir, "data/all_tracts_2020_discretized.csv"))
shooting_tracts_2020_discretized <- discretize_vars(shooting_tracts_2020_subset_vars, "shooter_school_affiliation", paste0(dir, "data/shooting_tracts_2020_discretized.csv"))
