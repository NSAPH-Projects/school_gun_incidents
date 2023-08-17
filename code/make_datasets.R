print("## Load packages ----")
library(readxl)
library(data.table)


print("## Read data ----")
dir <- "../" # run code in the script location

tracts_data <- read_excel(paste0(dir, "data/input/private/gun_violence_v2.xlsx"))
tracts_data <- as.data.table(tracts_data)
# var_descriptions <- tracts_data[1, ] # save variable descriptions
tracts_data <- tracts_data[2:nrow(tracts_data), ] # remove row of variable descriptions from dataset


print("## Exclude some rows ----")

tracts_data <- tracts_data[!startsWith(GEOID, "72")] # remove Puerto Rico because school shooting dataset (https://www.chds.us/ssdb) doesn't cover PR
tracts_data <- tracts_data[P0010001 != 0] # remove 18 Census tracts with population 0 since several variables will be NA


print("## Subset and transform variables ----")
tracts_data[, GEOID := NULL]
tracts_data[, groupquarters_GQNINST20_P := NULL] # collinear with groupquarters_GQINST20_P
tracts_data[, num_shootings := NULL] # not analyzing this outcome
tracts_data[, incident_date := NULL] # not analyzing this outcome
tracts_data[, dist_open_dealer := NULL] # not analyzing this exposure
tracts_data[, dist_open_commercial := NULL] # not analyzing this exposure
tracts_data[, duration_weighted_density := NULL] # exclude this variable for simplicity

qualitative_confounder_names <- "STATE_ABBR"

for (var in colnames(tracts_data)){
  if (!(var %in% qualitative_confounder_names)){ # alternatively, != "STATE_ABBR"
    tracts_data[[var]] <- as.numeric(tracts_data[[var]])
  }
}

tracts_data[, population_per_100sqmi := P0010001 / area_sq_mile * 100]
tracts_data[, daytime_population_per_100sqmi := populationtotals_DPOP_CY / area_sq_mile * 100]


print("## Remove highly correlated (>= 0.95) confounders ----")

tracts_data_cleaned <- copy(tracts_data)
tracts_data_cleaned[, `:=`(hu_per_100_sqmi = NULL,
                           sports_MP33017a_B_P = NULL)]

print("## Save intermediate and final datasets ----")
fwrite(tracts_data, paste0(dir, "data/intermediate/intermediate_cleaned_data_aug2023.csv"))
fwrite(tracts_data_cleaned, paste0(dir, "data/intermediate/cleaned_data_aug2023.csv"))
