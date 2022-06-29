library(readxl)
library(data.table)

##### Import raw data #####

dir <- "~/Harvard University/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

tracts_2020_all_data <- read_excel(paste0(dir, "data/tracts_2020_all_data.xlsx"))
tracts_2020_all_data <- as.data.table(tracts_2020_all_data)

codebook <- read_excel(paste0(dir, "data_dictionaries/codebook_all.xlsx"))


##### Remove rows #####

tracts_2020_subset <- tracts_2020_all_data[!startsWith(GEOID, "72")] # remove Puerto Rico because...
tracts_2020_subset <- tracts_2020_subset[!is.na(mean_total_miles)] # remove 13 NA's in treatment variable
tracts_2020_subset <- tracts_2020_subset[P0010001 != 0] # remove 19 Census tracts with total population 0


##### Subset columns (aka variables) #####

all_vars <- setDT(data.frame(var_name = codebook$FieldName, include = as.logical(codebook[[7]])))
include_vars <- all_vars[include == TRUE, var_name]
tracts_2020_subset <- subset(tracts_2020_subset, select = include_vars)


##### Rename variables ##### 

setnames(tracts_2020_subset, old = c("Shape_Area",
                                     "PCT_P0030001", "PCT_P0020005", "PCT_P0020006", "PCT_P0020007", "PCT_P0020008", "PCT_P0020009", "PCT_P0020011", "PCT_P0020002", 
                                     "P0050002", "P0050007",
                                     "householdincome_medhinc_cy", "incomebyage_media15_cy",
                                     "P0010001", "daytimepopulation_dpop_cy", "H0010001", "crime_crmcytotc"),
         new = c("Tract_Area_sq_meters",
                 "pct_18plus", "white_only_pct", "black_only_pct", "american_indian_alaskan_native_only_pct", "asian_only_pct", "native_hawaiian_pacific_islander_only_pct", "multiracial_pct", "hispanic_latino_pct",
                 "institutional_group_pop", "noninstitutional_group_pop",
                 "median_household_inc_2021", "median_household_inc_15to24_2021",
                 "total_population_2020", "daytime_pop_2021", "total_housing_units", "total_crime_2021"))


##### Extract, transform, scale variables ##### 

tracts_2020_transformed <- copy(tracts_2020_subset)
tracts_2020_transformed[, state_fips := substr(GEOID, 1, 2)]
tracts_2020_transformed[, county_fips := substr(GEOID, 1, 5)]
tracts_2020_transformed[, mean_total_km := mean_total_miles * 1.609344]
tracts_2020_transformed[, log_median_hh_income := log(median_household_inc_2021 + 0.01)]
tracts_2020_transformed[, log_median_hh_income_15to24 := log(median_household_inc_15to24_2021 + 0.01)]
tracts_2020_transformed[, `:=`(dealers_per_sq_meter = count_gun_dealers/Tract_Area_sq_meters,
                          schools_per_sq_meter = count_schools/Tract_Area_sq_meters)]
tracts_2020_transformed[, `:=`(prop_18plus = pct_18plus/100,
                          prop_white_only = white_only_pct/100,
                               prop_black_only = black_only_pct/100,
                               prop_american_indian_alaskan_native_only = american_indian_alaskan_native_only_pct/100,
                               prop_asian_only = asian_only_pct/100,
                               prop_native_hawaiian_pacific_islander_only = native_hawaiian_pacific_islander_only_pct/100,
                               prop_multiracial = multiracial_pct/100,
                               prop_hispanic_latino = hispanic_latino_pct/100)]
tracts_2020_transformed[, `:=`(prop_food_stamps_2019 = foodstampssnap_acssnap_p/100,
                               prop_public_assist_income_2019 = households_acspubai_p/100,
                               prop_below_poverty_2019 = households_acshhbpov_p/100,
                               prop_without_vehicles_2019 = vehiclesavailable_acsoveh0_p/100,
                               prop_hunted_with_shotgun_2021 = sports_mp33018a_b_p/100,
                               prop_bachelor_deg_25plus_2021 = educationalattainment_bachdeg_cy_p/100,
                               prop_grad_deg_25plus_2021 = educationalattainment_graddeg_cy_p/100,
                               prop_unemployed_2021 = employmentunemployment_unemprt_cy/100,
                               prop_unemployed_16to24_2021 = employmentunemployment_unage16cy_p/100)]
tracts_2020_transformed[, `:=`(housing_units_per_sq_meter = total_housing_units/Tract_Area_sq_meters,
                               prop_institutional_group = institutional_group_pop/total_population_2020, 
                               prop_noninstitutional_group = noninstitutional_group_pop/total_population_2020)]
tracts_2020_transformed <- tracts_2020_transformed[, `:=`(GEOID = NULL, mean_total_miles = NULL, median_household_inc_2021 = NULL, median_household_inc_15to24_2021 = NULL,
                                                          pct_18plus = NULL, white_only_pct = NULL, black_only_pct = NULL, american_indian_alaskan_native_only_pct = NULL,
                                                          asian_only_pct = NULL, native_hawaiian_pacific_islander_only_pct = NULL, multiracial_pct = NULL,
                                                          hispanic_latino_pct = NULL, foodstampssnap_acssnap_p = NULL, households_acspubai_p = NULL,
                                                          households_acshhbpov_p = NULL, vehiclesavailable_acsoveh0_p = NULL, sports_mp33018a_b_p = NULL,
                                                          educationalattainment_bachdeg_cy_p = NULL, educationalattainment_graddeg_cy_p = NULL,
                                                          employmentunemployment_unemprt_cy = NULL, employmentunemployment_unage16cy_p = NULL,
                                                          total_housing_units = NULL, institutional_group_pop = NULL, noninstitutional_group_pop = NULL)]

##### Save dataset for all Census tracts containing a school #####

fwrite(tracts_2020_transformed, paste0(dir, "Data/all_tracts_2020_subset_vars.csv"))
