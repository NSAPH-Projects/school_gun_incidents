print("## Load packages ----")

library(readxl)
library(data.table)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- read_excel(paste0(dir, "data/input/private/gun_violence_v2.xlsx"))
tracts_data <- as.data.table(tracts_data)
# var_descriptions <- tracts_data[1, ] # save variable descriptions
tracts_data <- tracts_data[2:nrow(tracts_data), ] # remove row of variable descriptions from dataset

census_divisions_data <- fread(paste0(dir, "data/input/open/census_regions_divisions.csv"))

urbanity_data <- fread(paste0(dir, "data/input/open/NCHSURCodes2013.csv"))


print("## Merge Datasets ----")

census_divisions_data <- subset(census_divisions_data, select = c("state_fips", "census_division_number"))
census_divisions_data[, state_fips := ifelse(nchar(state_fips) == 1, paste0("0", state_fips), state_fips)] # add leading 0 if necessary, convert state_fips to character

urbanity_data <- urbanity_data[, .(county_fips = as.character(`FIPS code`), urban_rural = `2013 code`)]
urbanity_data[, county_fips := ifelse(nchar(county_fips) == 4, paste0("0", county_fips), county_fips)] # add leading 0 if necessary, convert state_fips to character

tracts_data[, state_fips := substr(GEOID, 1, 2)]
tracts_data[, county_fips := substr(GEOID, 1, 5)]
tracts_data <- merge(tracts_data, census_divisions_data, by = "state_fips", all.x = T, all.y = F)
tracts_data <- merge(tracts_data, urbanity_data, by = "county_fips", all.x = T, all.y = F)


print("## Exclude some rows ----")

tracts_data <- tracts_data[!startsWith(GEOID, "72")] # remove Puerto Rico because school shooting dataset (https://www.chds.us/ssdb) doesn't cover PR
tracts_data <- tracts_data[P0010001 != 0] # remove 18 Census tracts with population 0 since several variables will be NA


print("## Subset and transform variables ----")

tracts_data[, GEOID := NULL]
tracts_data[, state_fips := NULL]
tracts_data[, county_fips := NULL]
tracts_data[, groupquarters_GQNINST20_P := NULL] # collinear with groupquarters_GQINST20_P
tracts_data[, num_dealers := NULL] # already including firearm_retailers_per_100sqmi
tracts_data[, num_schools := NULL] # already including schools_per_100_sqmi
tracts_data[, duration_weighted_density := NULL] # exclude this variable for simplicity
tracts_data[, num_shootings := NULL] # not analyzing this outcome
tracts_data[, incident_date := NULL] # not analyzing this outcome
tracts_data[, dist_open_dealer := NULL] # not analyzing this exposure
tracts_data[, dist_open_commercial := NULL] # not analyzing this exposure

qualitative_confounder_names <- c("census_division_number", "STATE_ABBR", "urban_rural")

for (var in colnames(tracts_data)){
  if (var %in% qualitative_confounder_names){
    tracts_data[[var]] <- as.factor(tracts_data[[var]])
  } else{
    tracts_data[[var]] <- as.numeric(tracts_data[[var]])
  }
}

tracts_data[, log_med_HH_income := log(householdincome_MEDHINC_CY + 0.01)]
tracts_data[, log_avg_HH_income_15to24 := log(incomebyage_AVGIA15_CY + 0.01)]
tracts_data[, householdincome_MEDHINC_CY := NULL]
tracts_data[, incomebyage_AVGIA15_CY := NULL]


print("## Remove highly correlated (>= 0.95) confounders ----")

tracts_data_final <- copy(tracts_data)
tracts_data_final[, `:=`(sports_MP33017a_B_P = NULL)]


print("## Save intermediate and final datasets ----")

fwrite(tracts_data, paste0(dir, "data/intermediate/intermediate_data_aug2023.csv"))
fwrite(tracts_data_final, paste0(dir, "data/intermediate/final_data_aug2023.csv"))
