## Load packages ----

library(readxl)
library(tigris)
library(ggplot2)
library(viridis)


## Read in raw data, get main exposure (distance to commercial dealer) with GEOID (census tract) ----

dir <- here::here() # location of repository

exposure <- "mean_dist_commercial_dealers" # main exposure

tracts_data <- read_excel(here::here(dir, "data/input/private/gun_violence_v3.2_persistent.xlsx"))
tracts_data <- tracts_data[tracts_data$num_schools > 0, ]
tracts_data <- tracts_data[!is.na(tracts_data[[exposure]]), ]

exposure_with_geoid <- tracts_data[, c("GEOID", exposure)]
colnames(exposure_with_geoid) <- c("GEOID", "Exposure")


## Get census tract geometry/shape files, then merge with exposure ----

year <- 2022
contiguous_states_plus_DC <- c(state.abb[!(state.abb %in% c("AK", "HI"))], "DC")

get_tract_geometries_for_one_state <- function(state, year){
  tracts_info <- tigris::tracts(state = state, year = year)
  geometries <- subset(tracts_info, select = c("GEOID", "geometry"))
  return(geometries)
}

tract_geometries <- lapply(contiguous_states_plus_DC,
                           get_tract_geometries_for_one_state,
                           year = year)

tract_geometries <- do.call("rbind", tract_geometries, quote = T) # combines all states' census tracts into one sf/data.frame object

exposure_with_geometry <- merge(tract_geometries, exposure_with_geoid, by = "GEOID")


## Make map ----

p <- ggplot(exposure_with_geometry) +
  geom_sf(aes(geometry = geometry, fill = Exposure)) +
  scale_fill_viridis() +
  theme_bw()

results_dir <- here::here(dir, "results/exploratory")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = T)
ggsave(filename = here::here(results_dir, "mean_dist_commercial_dealers.png"),
       plot = p)

