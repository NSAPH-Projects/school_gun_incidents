# Import data
library(readxl)
tracts_2020_all_data <- read_excel("Harvard University/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/data/tracts_2020_all_data.xlsx")
all_tracts_2020_subset_vars <- fread(paste0(dir, "data/all_tracts_2020_subset_vars.csv"))
shooting_tracts_2020_subset_vars <- fread(paste0(dir, "data/shooting_tracts_2020_subset_vars.csv"))

# Check number of NA's in each variable (Answer: as written in data dictionary)
check_nulls <- function(i, df){
  return(sum(is.na(df[[i]])))
}
nulls <- sapply(1:ncol(tracts_2020_all_data), check_nulls, tracts_2020_all_data)
names(nulls) <- names(tracts_2020_all_data)

# Check that each Census tract in this dataset has at least 1 schools (Answer: yes)
schools <- tracts_2020_all_data[, "count_schools"]
summary(schools)

# Examine binary_shooting_incident, count_school_shootings, and num_shooters (Observe: binary_shooting_incident = (count_school_shootings >= 1))
table(tracts_2020_all_data$binary_shooting_incident)
table(tracts_2020_all_data$count_school_shootings)
table(tracts_2020_all_data$num_shooters)

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
View(cor(all_tracts_2020_subset_vars, use = "pairwise.complete.obs"))
View(cor(subset(shooting_tracts_2020_subset_vars, select =
                  names(shooting_tracts_2020_subset_vars)[names(shooting_tracts_2020_subset_vars) != "shooter_school_affiliation"]),
         use = "pairwise.complete.obs"))

