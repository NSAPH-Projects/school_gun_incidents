# Import data
library(readxl)
library(data.table)

dir <- "/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

tracts_2020_all_data <- read_excel(paste0(dir, "data/tracts_2020_all_data.xlsx"))
tracts_2020_all_data <- as.data.table(tracts_2020_all_data)
all_tracts_2020_new_distances <- read_excel(paste0(dir, "data/all_tracts_2020_new_distances.xls"))
all_tracts_2020_new_distances <- as.data.table(all_tracts_2020_new_distances)

all_tracts_2020_subset_vars <- fread(paste0(dir, "data/all_tracts_2020_subset_vars.csv"))
# shooting_tracts_2020_subset_vars <- fread(paste0(dir, "data/shooting_tracts_2020_subset_vars.csv"))

# Check number of NA's in each variable
# Answer: as written in data dictionary
check_nulls <- function(i, df){
  return(sum(is.na(df[[i]])))
}
nulls <- sapply(1:ncol(tracts_2020_all_data), check_nulls, tracts_2020_all_data)
names(nulls) <- names(tracts_2020_all_data)

# Check that each Census tract in this dataset has at least 1 schools
# Answer: yes
schools <- tracts_2020_all_data[, "count_schools"]
summary(schools)

# Examine binary_shooting_incident, count_school_shootings, and num_shooters
# Observe: binary_shooting_incident = (count_school_shootings >= 1)
table(tracts_2020_all_data$binary_shooting_incident)
table(tracts_2020_all_data$count_school_shootings)
table(tracts_2020_all_data$num_shooters)

# Explore variable: date of school shooting
# Observe that Date and incident_date are equivalent variables
Date <- as.Date(tracts_2020_all_data$Date[!is.na(tracts_2020_all_data$Date)])
incident_date <- as.Date(tracts_2020_all_data$incident_date[!is.na(tracts_2020_all_data$incident_date)])
sum(Date != incident_date)
hist(year(Date))
hist(month(Date))

# Correct "Location" and "Location_Type" for 1 observation
tracts_2020_all_data[binary_shooting_incident == 1 & is.na(Location), Narrative]
tracts_2020_all_data[binary_shooting_incident == 1 & is.na(Location), `:=`(Location = "In or behind school fieldhouse", Location_Type = "Outside on School Property")]

# Check accidental shootings
tracts_2020_all_data[binary_shooting_incident == 1 & str_detect(Situation, "Accident"), .(count_school_shootings, Date, Preplanned, Narrative, Summary, Situation, Targets, Shots_Fired)]

# Check shooter ages
unique(tracts_2020_all_data[binary_shooting_incident == 1, shooter_age])
temp <- tracts_2020_all_data[binary_shooting_incident == 1 & !shooter_age %in% c("Adult", "Child", "Minor", "Teen", "null") & !is.na(shooter_age),
                             .(count_school_shootings, Date, shooter_age = as.integer(shooter_age), Narrative, Summary, Situation, Targets, Shots_Fired)]
temp[shooter_age < 10]
temp[shooter_age > 70]

# Check location of events labeled as school shootings
tracts_2020_all_data[binary_shooting_incident == 1 & Location_Type == "Off School Property",
                             .(count_school_shootings, Date, Location, Location_Type, Narrative, Summary, Situation)]


# Examine tracts with >1 count_school_shootings (outcome variables are for most recent school shooting)
tracts_2020_all_data[binary_shooting_incident == 1 & count_school_shootings > 1, .(count_school_shootings, Date, Narrative, Summary, Situation, Targets, Shots_Fired)]

# Check that PCT_{...} vars match {population of interest}/{total population}*100 (Answer: close but not always exact)
pct_vars <- c("PCT_P0020007", "PCT_P0020008", "PCT_P0020006", "PCT_P0020002", "PCT_P0020009", "PCT_H0010002", "PCT_H0010003", "PCT_P0030001", "PCT_P0020011", "PCT_P0020010", "PCT_P0020005")
tracts_2020_all_data[, .(PCT_P0020008, P0010006, P0010001)]

# Examine 10 group_quarters variables (all start with "P005")
group_quarters_vars <- c("P0050001", "P0050002", "P0050003", "P0050004", "P0050005", "P0050006", "P0050007", "P0050008", "P0050009", "P0050010")
quarters <- subset(tracts_2020_all_data, select = group_quarters_vars)
quarters$sum1 <- quarters$P0050007 + quarters$P0050002
sum(quarters$sum1 != quarters$P0050001) # Observe: 002 + 007 = 001 = total
# Also observe: 002 seems to contain 006 and 007 seems to contain 010
quarters$sum2 <- quarters$P0050003 + quarters$P0050004 + quarters$P0050005 + quarters$P0050008 + quarters$P0050009 + quarters$P0050010
sum(quarters$sum2 != quarters$P0050001) # Observe: 003, 004, 005, 008, 009, and 010 should be disjoint and account for most of 001 (total)

# Check collinearity between variables
quant_vars <- unlist(lapply(all_tracts_2020_subset_vars, is.numeric))
all_tracts_2020_quant_vars <- subset(all_tracts_2020_subset_vars, select = quant_vars)
View(cor(all_tracts_2020_quant_vars, use = "pairwise.complete.obs"))

# View(cor(subset(shooting_tracts_2020_subset_vars, select =
#                   names(shooting_tracts_2020_subset_vars)[names(shooting_tracts_2020_subset_vars) != "shooter_school_affiliation"]),
#          use = "pairwise.complete.obs"))

# Check that mean_total_miles is same in tracts_2020_all_data.xlsx and all_tracts_2020_new_distances.xlsx
temp <- merge(tracts_2020_all_data[!is.na(mean_total_miles), .(GEOID, mean_total_miles)], all_tracts_2020_new_distances[!is.na(mean_total_miles), .(GEOID, mean_total_miles)], by = "GEOID")
nrow(temp[round(mean_total_miles.x, digits = 10) != round(mean_total_miles.y, digits = 10)]) # 0
nrow(temp[round(mean_total_miles.x, digits = 11) != round(mean_total_miles.y, digits = 11)]) # 1
nrow(temp[round(mean_total_miles.x, digits = 12) != round(mean_total_miles.y, digits = 12)]) # 17
nrow(temp[round(mean_total_miles.x, digits = 13) != round(mean_total_miles.y, digits = 13)]) # 178
nrow(temp[mean_total_miles.x != mean_total_miles.y]) # 25887
