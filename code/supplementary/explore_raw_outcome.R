print("## Load packages ----")

library(readxl)
library(lubridate)
library(data.table)
library(ggplot2)


print("## Read raw data ----")

dir <- "../" # run code in the script location; may need to use dir <- "../../"

raw_outcome_data <- read_excel(paste0(dir, "data/input/private/Public v3.1 K-12 School Shooting Database (8 28 2023).xlsx"),
                               sheet = "Incident")
raw_outcome_data <- as.data.table(raw_outcome_data)
raw_outcome_data[, Date := as.Date(Date)]
raw_outcome_data <- raw_outcome_data[Date >= "2014-01-01" & Date <= "2023-08-31"] # the study period is January 2014 to August 2023, inclusive


print("## Get school gun incidents (counts, not binary SGI variable in main analysis) ----")

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
