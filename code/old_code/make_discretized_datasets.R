source("code/helper_functions.R")

all_confounder_names <- c("census_division_number", quantitative_confounders)
decilized_confounder_names <- c("census_division_number", paste0(quantitative_confounders, "_decile"))
decilized_confounder_names_incl_urban_rural <- c("urban_rural", decilized_confounder_names)
decilized_confounder_names_state_level <- paste0(quantitative_confounders, "_decile")

# to do: edit this function to allow more options/parameters like get_binary_exposure_continuous_confounders_dataset()
get_undiscretized_dataset <- function(exposure_name, confounder_names = NA){
  cleaned_data <-  fread("data/all_tracts_2020_subset_vars.csv")
  if ("State_Name" %in% colnames(cleaned_data)){
    data_analysis <- get_analysis_df(cleaned_data, exposure_name, c(all_confounder_names, "State_Name"))
  } else{
    data_analysis <- get_analysis_df(cleaned_data, exposure_name, all_confounder_names)
  }
  data_analysis <- na.omit(data_analysis) 
  return(data_analysis)
}

get_binary_exposure_continuous_confounders_dataset <- function(exposure_name, write_file = F, cutoff = NA,
                                                               confounder_names = NA, alternate_data = NA, na_omit = T){
  if (is.na(alternate_data)){
    df <- fread("data/all_tracts_2020_subset_vars.csv")
  } else{
    df <- fread(alternate_data)
  }
  
  if (is.na(cutoff)){
    cutoff <- median(df[[exposure_name]])
  }
  
  df[[exposure_name]] <- 1 * (df[[exposure_name]] > cutoff)
  
  if (write_file){
    write.csv(df, "data/all_tracts_2020_subset_vars_binary_exposure.csv")
  }
  
  if (is.na(confounder_names)){
    data_analysis <- get_analysis_df(df, exposure_name, all_confounder_names)
  } else{
    data_analysis <- get_analysis_df(df, exposure_name, confounder_names)
  }
  
  if (na_omit){
    data_analysis <- na.omit(data_analysis)
  }
  
  return(data_analysis)
}

discretize_treatment <- function(df){
   df <- copy(df)
   df$a <- ntile(df$a, 4)
   return(df)
}

decilize_quantitative_confounders <- function(df, write_file = F, decilized_confounders = NA){
  data_discretized <- copy(df)
  if (is.na(decilized_confounders)){
    decilized_confounders <- quantitative_confounders
  }
  for (var in decilized_confounders){
    data_discretized[[paste0(var, "_decile")]] <- ntile(data_discretized[[var]], 10)
    data_discretized[[var]] <- NULL
  }
  
  if (write_file){
    fwrite(data_discretized, "data/all_tracts_2020_subset_vars_discretized.csv")
  }
  return(data_discretized)
}

