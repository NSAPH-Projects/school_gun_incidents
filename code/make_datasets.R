print("## Load packages ----")

library(readxl)
library(data.table)


print("## Read data ----")

dir <- "../" # run code in the script location

tracts_data <- read_excel(paste0(dir, "data/input/private/gun_violence_v3_persistent.xlsx"))
tracts_data <- as.data.table(tracts_data)

census_divisions_data <- fread(paste0(dir, "data/input/open/census_regions_divisions.csv"))

urbanity_data <- fread(paste0(dir, "data/input/open/NCHSURCodes2013.csv"))

codebook <- read_excel(paste0(dir, "data/input/private/Data Dictionary V3.xlsx"))


print("## Merge Datasets ----")

census_divisions_data <- subset(census_divisions_data, select = c("state_fips", "census_division_number"))
census_divisions_data[, state_fips := ifelse(nchar(state_fips) == 1, paste0("0", state_fips), state_fips)] # add leading 0 if necessary, convert state_fips to character

urbanity_data <- urbanity_data[, .(county_fips = as.character(`FIPS code`), urbanity = `2013 code`)]
urbanity_data[, county_fips := ifelse(nchar(county_fips) == 4, paste0("0", county_fips), county_fips)] # add leading 0 if necessary, convert state_fips to character

tracts_data[, state_fips := substr(GEOID, 1, 2)]
tracts_data[, county_fips := substr(GEOID, 1, 5)]
tracts_data <- merge(tracts_data, census_divisions_data, by = "state_fips", all.x = T, all.y = F)
tracts_data <- merge(tracts_data, urbanity_data, by = "county_fips", all.x = T, all.y = F)


print("## Exclude some rows ----")

tracts_data <- tracts_data[num_schools > 0] # include only census tracts containing at least one school; now there are 56,883 observations
# tracts_data <- tracts_data[!startsWith(GEOID, "72")] # remove Puerto Rico because school shooting dataset (https://www.chds.us/ssdb) doesn't cover PR
# tracts_data <- tracts_data[populationtotals_TOTPOP20 != 0] # remove 18 Census tracts with population 0 since several variables will be NA


print("## Subset variables ----")

all_vars <- setDT(data.frame(var_name = codebook$FieldName, include = as.logical(codebook[[9]])))
include_vars <- all_vars[include == TRUE, var_name]

tracts_data <- subset(tracts_data, select = include_vars)


print("## Transform variables ----")

tracts_data[, SGI := num_gun_incidents >= 1]
tracts_data[, housing_per_100sqmi := housingunittotals_TOTHU20 / area_sq_miles * 100]
tracts_data[, schools_per_100sqmi := num_schools / area_sq_miles * 100]
tracts_data[, firearm_retailers_per_100sqmi := num_ffl / area_sq_miles * 100]
tracts_data[, log_med_HH_income := log(householdincome_ACSMEDHINC + 0.01)]
tracts_data[, log_med_HH_income_15to24 := log(incomebyage_ACSMEDIA15 + 0.01)]
tracts_data[, num_gun_incidents := NULL]
tracts_data[, housingunittotals_TOTHU20 := NULL]
tracts_data[, num_schools := NULL]
tracts_data[, num_ffl := NULL]
tracts_data[, householdincome_ACSMEDHINC := NULL]
tracts_data[, incomebyage_ACSMEDIA15 := NULL]

qualitative_confounder_names <- c("census_division_number", "State_Name", "urbanity")

for (var in colnames(tracts_data)){
  if (var %in% qualitative_confounder_names){
    tracts_data[[var]] <- as.factor(tracts_data[[var]])
  } else{
    tracts_data[[var]] <- as.numeric(tracts_data[[var]])
  }
}


print("## Save final datasets ----")

fwrite(tracts_data, paste0(dir, "data/intermediate/final_data_sep2023.csv"))
