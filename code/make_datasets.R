# library(readxl)
library(data.table)

##### Import raw data #####

dir <- "Harvard University/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

tracts_2020_all_data <- read_excel(paste0(dir, "data/tracts_2020_all_data.xlsx"))
tracts_2020_all_data <- as.data.table(tracts_2020_all_data)


#### Process supplementary datasets ####

county_data <- read_excel(paste0(dir, "data/NCHSURCodes2013.xlsx"))
county_data <- as.data.table(county_data)
county_data <- county_data[, .(county_fips = as.character(`FIPS code`), urban_rural = `1990-based code`)] # urban_rural variable is NCHS 2013's code (1 = "Large central metro", 6 = "Noncore")
county_data[nchar(county_fips) == 4, county_fips := paste0("0", county_fips)]


##### Sort variables into treatment, outcome, and covariates #####

all_vars <- names(tracts_2020_all_data)

treatment_vars <- c("mean_total_miles", "max_total_miles", "min_total_miles",
                    "count_gun_dealers")
outcome_vars <- c("binary_shooting_incident", "count_school_shootings", "num_shooters",
                  "shooter_school_affiliation", "FIRST_weapontype", "Preplanned", "Targets", "School_Level",
                  "Accomplice", "Shots_Fired", "First_Shot", "Situation", "Time_Period", "During_School", "Hostages", "Barricade", "Active_Shooter_FBI", "Officer_Involved", "shooter_injury", "shooter_outcome",
                  "Media_Attention", "Narrative", "Number_News",
                  "Summary", "Sources",
                  "Date", "incident_date", "Incident_ID", "Location", "Location_Type", "School", "City", "State_1",
                  "shooter_age", "shooter_race", "shooter_gender", "Gang_Related", "Bullied", "Domestic_Violence")
unclear_vars <- c("NEAR_DIST", "Quarter", "Reliability")

covars <- all_vars[!all_vars %in% treatment_vars]
covars <- covars[!covars %in% outcome_vars]
covars <- covars[!covars %in% unclear_vars]


##### Add variables ##### 

tracts_2020_subset_vars <- tracts_2020_all_data
tracts_2020_subset_vars[, pop_below18 := P0010001 - P0030001]
tracts_2020_subset_vars[, state_fips := substr(GEOID, 1, 2)]
tracts_2020_subset_vars[, county_fips := substr(GEOID, 1, 5)]
tracts_2020_subset_vars <- merge(tracts_2020_subset_vars, county_data, by="county_fips", all.x=T, all.y=F)

all_vars <- names(tracts_2020_subset_vars)


##### Exclude variables #####

exclude_misc_covars <- c("OBJECTID", "GEOID", "sports_mp33018a_b_i", "POP100", # POP100 is duplicate of P0010001
                         "crime_crmcyperc", "crime_crmcyproc") # cor(total_crime, property_crime) = 0.99 and cor(total_crime, personal_crime) = 0.79, so keep only total
exclude_ses_covars <- c("incomebyage_avgia15_cy", "employmentunemployment_unemrt16cy", "employmentunemployment_unemp_cy_p",
                        "wealth_avgdi_cy", "householdincome_pci_cy") # cor(median_household_income, average_disposable_income) = 0.98 and cor(median_household_income, income_per_capita) = 0.88
exclude_group_quarters_covars <- c("P0050002", "P0050006", "P0050007") # keep: P0050001, P0050003, P0050004, P0050005, P0050008, P0050009, P0050010
exclude_housing_covars <- c("HU100", "H0010002", "H0010003") # HU100 is duplicate of H0010001; H0010003 = H0010001 - H0010002; cor(H0010001, H0010002) = 0.95
race18plus_covars <- c("P0030001", "P0030009", "P0040002", "P0040003", "P0040004", "P0040007", "P0040008", "P0040006", "P0040009",
                               "P0040010", "P0040005", "P0040011", "P0040012", "P0040022", "P0040023", "P0040024", "P0040025",
                               "P0040026", "P0040018", "P0040019", "P0040020", "P0040021", "P0040027", "P0040014", "P0040015",
                               "P0040013", "P0040016", "P0040017", "P0030002", "P0030005", "P0030006", "P0030004", "P0030007",
                               "P0030008", "P0030003", "P0030026", "P0030010", "P0030020", "P0030021", "P0030022", "P0030023",
                               "P0030024", "P0030016", "P0030017", "P0030018", "P0030019", "P0030025", "P0030012", "P0030013",
                               "P0030011", "P0030014", "P0030015") # remove P0030001 (total 18+ population) too since 0.98 correlation with total pop
exclude_num_race_covars <- c("P0010002", "P0010010", "P0010026") # keep: P0010010 (population of 2+ races)
biracial_covars <- c("P0010020", "P0010021", "P0010022", "P0010023", "P0010024", "P0010016", "P0010017", "P0010018",
                             "P0010019", "P0010025", "P0010012", "P0010013", "P0010011", "P0010014", "P0010015")
exclude_uniracial_covars <- c("P0010008") # keep: P0010001 (total population), P0010005 (American Indian/Alaskan native alone), P0010006 (Asian alone), P0010004 (Black alone), P0010007 (Native Hawaiian/Pacific Islander alone), P0010003 (White alone)
nonHispLat_covars <- c("P0020003", "P0020007", "P0020008", "P0020006", "P0020009", "P0020004", "P0020011", "P0020012"
                       , "P0020022", "P0020023", "P0020024", "P0020025", "P0020026", "P0020018", "P0020019", "P0020020",
                       "P0020021", "P0020027", "P0020014", "P0020015", "P0020013", "P0020016", "P0020017", "P0020010", "P0020005") # keep: P0020002 (Hispanic/Latino population)
pct_vars <- c("PCT_P0020007", "PCT_P0020008", "PCT_P0020006", "PCT_P0020002", "PCT_P0020009", "PCT_H0010002", "PCT_H0010003", "PCT_P0030001", "PCT_P0020011", "PCT_P0020010", "PCT_P0020005")

# combine all variables to be excluded
all_exclude_vars <- c(unclear_vars, exclude_misc_covars, exclude_ses_covars, exclude_group_quarters_covars, exclude_housing_covars,
                      race18plus_covars, exclude_num_race_covars, biracial_covars, exclude_uniracial_covars, nonHispLat_covars, pct_vars)
all_include_vars <- all_vars[!all_vars %in% all_exclude_vars]

# subset data
tracts_2020_subset_vars <- subset(tracts_2020_subset_vars, select=all_include_vars)


##### Rename variables ##### 

setnames(tracts_2020_subset_vars, old = c("P0010003", "P0010004", "P0010005", "P0010006", "P0010007", "P0010009", "P0020002",
                                          "P0050001", "P0050003", "P0050004", "P0050005", "P0050008", "P0050009", "P0050010",
                                          "householdincome_medhinc_cy", "incomebyage_media15_cy",
                                          "P0010001", "daytimepopulation_dpop_cy", "H0010001", "crime_crmcytotc"),
         new = c("white_only_pop", "black_only_pop", "american_indian_alaskan_native_only_pop", "asian_only_pop", "native_hawaiian_pacific_islander_only_pop", "biracial_pop", "hispanic_latino_pop",
                 "total_group_quarters", "adult_correctional_pop", "juvenile_detention_pop", "nursing_pop", "university_pop", "military_pop", "other_noninstitutional_pop",
                 "median_household_inc_2021", "median_household_inc_15to24_2021",
                 "total_population", "daytime_pop_2021", "total_housing_units", "total_crime_2021"))


##### Scale variables to population ##### 

tracts_2020_vars_scaled <- tracts_2020_subset_vars
tracts_2020_vars_scaled[, `:=`(prop_white_only = white_only_pop/total_population,
                               prop_black_only = black_only_pop/total_population,
                               prop_american_indian_alaskan_native_only = american_indian_alaskan_native_only_pop/total_population,
                               prop_asian_only = asian_only_pop/total_population,
                               prop_native_hawaiian_pacific_islander_only = native_hawaiian_pacific_islander_only_pop/total_population,
                               prop_biracial = biracial_pop/total_population,
                               prop_hispanic_latino = hispanic_latino_pop/total_population)]
tracts_2020_vars_scaled[, `:=`(prop_food_stamps_2019 = foodstampssnap_acssnap_p/100,
                               prop_public_assist_income_2019 = households_acspubai_p/100,
                               prop_below_poverty_2019 = households_acshhbpov_p/100,
                               prop_without_vehicles_2019 = vehiclesavailable_acsoveh0_p/100,
                               prop_hunted_with_shotgun_2021 = sports_mp33018a_b_p/100,
                               prop_bachelor_deg_25plus_2021 = educationalattainment_bachdeg_cy_p/100,
                               prop_grad_deg_25plus_2021 = educationalattainment_graddeg_cy_p/100,
                               prop_unemployed_2021 = employmentunemployment_unemprt_cy/100,
                               prop_unemployed_16to24_2021 = employmentunemployment_unage16cy_p/100)]
tracts_2020_vars_scaled[, `:=`(prop_group_quartered = total_group_quarters/total_population, 
                               prop_adult_correctional = adult_correctional_pop/total_population, 
                               prop_juvenile_detention = juvenile_detention_pop/total_population, 
                               prop_nursing = nursing_pop/total_population, 
                               prop_university = university_pop/total_population,
                               prop_military = military_pop/total_population,
                               prop_other_noninstitutional = other_noninstitutional_pop/total_population)]
tracts_2020_vars_scaled <- tracts_2020_vars_scaled[, `:=`(white_only_pop = NULL, black_only_pop = NULL, american_indian_alaskan_native_only_pop = NULL,
                                                            asian_only_pop = NULL, native_hawaiian_pacific_islander_only_pop = NULL, biracial_pop = NULL,
                                                            hispanic_latino_pop = NULL, foodstampssnap_acssnap_p = NULL, households_acspubai_p = NULL,
                                                            households_acshhbpov_p = NULL, vehiclesavailable_acsoveh0_p = NULL, sports_mp33018a_b_p = NULL,
                                                            educationalattainment_bachdeg_cy_p = NULL, educationalattainment_graddeg_cy_p = NULL,
                                                          employmentunemployment_unemprt_cy = NULL, employmentunemployment_unage16cy_p = NULL,
                                                          total_group_quarters = NULL, adult_correctional_pop = NULL, juvenile_detention_pop = NULL, nursing_pop = NULL,
                                                          university_pop = NULL, military_pop = NULL, other_noninstitutional_pop = NULL)]

##### Make preliminary dataset (1 treatment, 1 outcome) for all Census tracts containing a school #####

exclude_treatment_vars1 <- treatment_vars[treatment_vars != "mean_total_miles"] # include only mean_total_miles as treatment variable
exclude_outcome_vars1 <- outcome_vars[outcome_vars != "binary_shooting_incident"] # include only binary_shooting_incident as outcome variable
all_tracts_2020_subset_vars <- subset(tracts_2020_vars_scaled, select = names(tracts_2020_vars_scaled)[!names(tracts_2020_vars_scaled) %in% c(exclude_treatment_vars1, exclude_outcome_vars1)])
fwrite(all_tracts_2020_subset_vars, paste0(dir, "Data/all_tracts_2020_subset_vars.csv"))

##### Make preliminary dataset (1 treatment, 1 outcome) for all Census tracts that had at least 1 school shooting #####

shooting_tracts_2020_subset_vars <- tracts_2020_vars_scaled[binary_shooting_incident == 1, ] # filter to 829 tracts that had at least 1 shooting

exclude_treatment_vars2 <- treatment_vars[treatment_vars != "mean_total_miles"] # include only mean_total_miles as treatment variable
exclude_outcome_vars2 <- outcome_vars[outcome_vars != "shooter_school_affiliation"] # include only shooter_school_affiliation as outcome variable
shooting_tracts_2020_subset_vars <- subset(shooting_tracts_2020_subset_vars, select = names(tracts_2020_vars_scaled)[!names(tracts_2020_vars_scaled) %in% c(exclude_treatment_vars2, exclude_outcome_vars2)])
shooting_tracts_2020_subset_vars[, prop_military := NULL] # remove prop_military covariate because it equals 0 for all tracts in this dataset
fwrite(shooting_tracts_2020_subset_vars, paste0(dir, "Data/shooting_tracts_2020_subset_vars.csv"))


### For later analysis: use gun dealer density as treatment (density = dealers/population or dealers/area)
# exclude_treatment_vars2 <- treatment_vars[outcome_vars != "count_gun_dealers"] # include only count_gun_dealers as treatment variable
# shooting_tracts_2020_subset_vars[, dealers_per_capita := count_gun_dealers/P0010001]
# shooting_tracts_2020_subset_vars[, dealers_per_area := count_gun_dealers/Shape_Area]

