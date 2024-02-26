print("## Load packages ----")

library(readxl)
library(lubridate)
library(data.table)
library(ggplot2)


print("## Read raw data ----")

dir <- here::here() # location of repo

raw_outcome_data <- read_excel(here::here(dir, "data/input/private/Public v3.1 K-12 School Shooting Database (8 28 2023).xlsx"),
                               sheet = "Incident")
raw_outcome_data <- as.data.table(raw_outcome_data)
raw_outcome_data[, Date := as.Date(Date)]
raw_outcome_data <- raw_outcome_data[Date >= "2014-01-01" & Date <= "2023-08-31"] # the study period is January 2014 to August 2023, inclusive
cat("Total number of incidents between January 1, 2014 and August 31, 2023:", nrow(raw_outcome_data))


print("## Plot school gun incidents (counts, not binary SGI variable in main analysis) ----")

raw_outcome_by_month <- raw_outcome_data[, Date := floor_date(Date, unit = "month")] # count monthly incidents
raw_outcome_by_month <- raw_outcome_data[, .(SGI_counts = .N), by = Date] # each row in the dataset is a school gun incident
# raw_outcome_by_month <- raw_outcome_data[, .(SGI_counts = .N), by = .(Year, Month)] # each row in the dataset is a school gun incident, so count the month-year of each incident

raw_outcome_plot <- ggplot(raw_outcome_by_month, aes(Date, SGI_counts)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_date(date_labels = "%Y", date_breaks = "year", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, max(raw_outcome_by_month$SGI_counts), by = 5), minor_breaks = NULL) +
  ylab("Number of school gun incidents") + 
  theme_bw()

ggsave(paste0(dir, "results/exploratory/raw_outcome_by_month.png"), raw_outcome_plot)


print("## Explore raw outcome ----")

table(raw_outcome_data$School_Level, useNA = "always")

# 44724        6-12  Elementary        High Junior High        K-12         K-8 
#     8           4         326         728           5          31          36 
# Middle       Other     Unknown        <NA> 
#    143          19           4          20 


table(raw_outcome_data$During_School, useNA = "always")

#    No  Yes <NA> 
#   657  658    9 


table(raw_outcome_data$Involves_Students_Staff, useNA = "always")

#  No    Y  Yes <NA> 
# 316    1  999    8 
