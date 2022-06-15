library(usmap)
library(ggplot2)

dir <- "Harvard University/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

# set up
all_tracts_2020_subset_vars <- fread(paste0(dir, "data/all_tracts_2020_subset_vars.csv"))
shooting_tracts_2020_subset_vars <- fread(paste0(dir, "data/shooting_tracts_2020_subset_vars.csv"))

all_tracts_2020_subset_vars[nchar(state_fips) == 1, state_fips := paste0("0", state_fips)]
all_tracts_2020_subset_vars[nchar(county_fips) == 4, county_fips := paste0("0", county_fips)]
shooting_tracts_2020_subset_vars[nchar(state_fips) == 1, state_fips := paste0("0", state_fips)]
shooting_tracts_2020_subset_vars[nchar(county_fips) == 4, county_fips := paste0("0", county_fips)]


# get mean(mean_total_miles) for each state/county
df_states <- all_tracts_2020_subset_vars[, .(mean_total_miles = mean(mean_total_miles, na.rm = T)), by = state_fips]
df_counties <- all_tracts_2020_subset_vars[, .(mean_total_miles = mean(mean_total_miles, na.rm = T)), by = county_fips]
setnames(df_states, old = "state_fips", new = "fips")
setnames(df_counties, old = "county_fips", new = "fips")

# plot mean(mean_total_miles) for each state/county
states_map <- plot_usmap(regions = "states", data = df_states, values = "mean_total_miles")
counties_map <- plot_usmap(regions = "counties", data = df_counties, values = "mean_total_miles")
ggsave(paste0(dir, "figures/", "states_map_mean_miles.png"), states_map)
ggsave(paste0(dir, "figures/", "counties_map_mean_miles.png"), counties_map)

# plot mean(mean_total_miles) for each state/county, coding as {1,2,3,4,5} according to all states'/counties' mean(mean_total_miles)
# load function discretize_1_var from discretize_variables.R; I'll make a script of functions soon
df_states[, mean_miles_discretized := discretize_1_var(mean_total_miles)]
df_counties[, mean_miles_discretized := discretize_1_var(mean_total_miles)]
states_map_discretized <- plot_usmap(regions = "states", data = df_states, values = "mean_miles_discretized")
counties_map_discretized <- plot_usmap(regions = "counties", data = df_counties, values = "mean_miles_discretized")
ggsave(paste0(dir, "figures/", "states_map_mean_miles_discretized.png"), states_map_discretized)
ggsave(paste0(dir, "figures/", "counties_map_mean_miles_discretized.png"), counties_map_discretized)



#### IGNORE EVERYTHING AFTER THIS; NOT UPDATED ####

# function to get FIPS code (either 2 or 5 digits, "states" or "counties" level) from GEOID (multiple possible lengths/detail)
get_fips_from_geoid <- function(geoid, fips_level){
  fips <- vector()
  n_digits <- ifelse(fips_level == "counties", 5, ifelse(fips_level == "states", 2, NA))
  for (i in 1:length(geoid)){
    fips[i] <- substr(geoid[i], 1, n_digits)
  }
  return(fips)
}

make_temp_df <- function(var, var_name, fips){
  temp_df <- as.data.table(data.frame(var, fips))
  names(temp_df)[1] <- var_name
  return(temp_df)
}

plot_map <- function(temp_df, fips_level, values){
  return(plot_usmap(regions = fips_level, data = temp_df, values = values))
}

save_map <- function(map, filename){
  ggsave(paste0(dir, "figures/", filename), map)
}