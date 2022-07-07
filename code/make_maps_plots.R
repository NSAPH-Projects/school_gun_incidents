library(data.table)
library(usmap)
library(ggplot2)
library(ggpubr)

dir <- "/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

# set up
all_tracts_2020_subset_vars <- fread(paste0(dir, "data/all_tracts_2020_subset_vars.csv"))
# all_tracts_2020_subset_vars[nchar(state_fips) == 1, state_fips := paste0("0", state_fips)]
# all_tracts_2020_subset_vars[nchar(county_fips) == 4, county_fips := paste0("0", county_fips)]

# get mean(mean_total_km) for each state/county
df_states <- all_tracts_2020_subset_vars[, .(mean_total_km = mean(mean_total_km, na.rm = T),
                                             mean_dealers_per_sq_meter = mean(dealers_per_sq_meter, na.rm = T)), by = state_fips]
df_counties <- all_tracts_2020_subset_vars[, .(mean_total_km = mean(mean_total_km, na.rm = T),
                                               mean_dealers_per_sq_meter = mean(dealers_per_sq_meter, na.rm = T)), by = county_fips]
setnames(df_states, old = "state_fips", new = "fips")
setnames(df_counties, old = "county_fips", new = "fips")

# plot mean(mean_total_km) for each state/county
states_map_miles <- plot_usmap(regions = "states", data = df_states, values = "mean_total_km")
counties_map_miles <- plot_usmap(regions = "counties", data = df_counties, values = "mean_total_km")
ggsave(paste0(dir, "figures/", "states_map_mean_km.png"), states_map_miles)
ggsave(paste0(dir, "figures/", "counties_map_mean_km.png"), counties_map_miles)

# plot mean(mean_dealers_per_sq_meter) for each state/county
states_map_dealers <- plot_usmap(regions = "states", data = df_states, values = "mean_dealers_per_sq_meter")
counties_map_dealers <- plot_usmap(regions = "counties", data = df_counties, values = "mean_dealers_per_sq_meter")
ggsave(paste0(dir, "figures/", "states_map_mean_dealers.png"), states_map_dealers)
ggsave(paste0(dir, "figures/", "counties_map_mean_dealers.png"), counties_map_dealers)

# trim mean(mean_total_km) at (ie exclude past) 95th and 99th percentiles
df_states95 <- df_states[mean_total_km < quantile(mean_total_km, 0.95)]
df_states99 <- df_states[mean_total_km < quantile(mean_total_km, 0.99)]
df_counties95 <- df_counties[mean_total_km < quantile(mean_total_km, 0.95)]
df_counties99 <- df_counties[mean_total_km < quantile(mean_total_km, 0.99)]

# plot 95th- and 99th-percentile trimmed mean(mean_total_km) for each state/county
states_map_trimmed95 <- plot_usmap(regions = "states", data = df_states95, values = "mean_total_km")
counties_map_trimmed95 <- plot_usmap(regions = "counties", data = df_counties95, values = "mean_total_km")
states_map_trimmed99 <- plot_usmap(regions = "states", data = df_states99, values = "mean_total_km")
counties_map_trimmed99 <- plot_usmap(regions = "counties", data = df_counties99, values = "mean_total_km")
ggsave(paste0(dir, "figures/", "states_map_mean_km_trimmed95.png"), states_map_trimmed95)
ggsave(paste0(dir, "figures/", "counties_map_mean_km_trimmed95.png"), counties_map_trimmed95)
ggsave(paste0(dir, "figures/", "states_map_mean_km_trimmed99.png"), states_map_trimmed99)
ggsave(paste0(dir, "figures/", "counties_map_mean_km_trimmed99.png"), counties_map_trimmed99)

# plot mean_dealers_per_sq_meter > and <= median(mean_dealers_per_sq_meter)
df_states_above_median <- df_states[mean_dealers_per_sq_meter > median(mean_dealers_per_sq_meter)]
df_states_below_median <- df_states[mean_dealers_per_sq_meter <= median(mean_dealers_per_sq_meter)]
df_counties_above_median <- df_counties[mean_dealers_per_sq_meter > median(mean_dealers_per_sq_meter)]
df_counties_below_median <- df_counties[mean_dealers_per_sq_meter <= median(mean_dealers_per_sq_meter)]

# plot mean_dealers_per_sq_meter > and <= median(mean_dealers_per_sq_meter)
states_map_above_median <- plot_usmap(regions = "states", data = df_states_above_median, values = "mean_dealers_per_sq_meter")
counties_map_above_median <- plot_usmap(regions = "counties", data = df_counties_above_median, values = "mean_dealers_per_sq_meter")
states_map_below_median <- plot_usmap(regions = "states", data = df_states_below_median, values = "mean_dealers_per_sq_meter")
counties_map_below_median <- plot_usmap(regions = "counties", data = df_counties_below_median, values = "mean_dealers_per_sq_meter")
ggsave(paste0(dir, "figures/", "states_map_mean_dealers_above_median.png"), states_map_above_median)
ggsave(paste0(dir, "figures/", "counties_map_mean_dealers_above_median.png"), counties_map_above_median)
ggsave(paste0(dir, "figures/", "states_map_mean_dealers_below_median.png"), states_map_below_median)
ggsave(paste0(dir, "figures/", "counties_map_mean_dealers_below_median.png"), counties_map_below_median)

# Examine mean_total_miles, mean_grocery_km, mean_pharmacy_km
total_km_vs_grocery_km <- ggscatter(all_tracts_2020_subset_vars, x = "mean_total_km", y = "mean_grocery_km",
                                    add = "reg.line", add.params = list(color = "blue")) +
  stat_cor(method = "pearson")
total_km_vs_pharmacy_km <- ggscatter(all_tracts_2020_subset_vars, x = "mean_total_km", y = "mean_pharmacy_km",
                                     add = "reg.line", add.params = list(color = "blue")) +
  stat_cor(method = "pearson")
grocery_km_vs_pharmacy_km <- ggscatter(all_tracts_2020_subset_vars, x = "mean_grocery_km", y = "mean_pharmacy_km",
                                       add = "reg.line", add.params = list(color = "blue")) +
  stat_cor(method = "pearson")

ggsave(paste0(dir, "figures/total_km_vs_grocery_km.png"), total_km_vs_grocery_km)
ggsave(paste0(dir, "figures/total_km_vs_pharmacy_km.png"), total_km_vs_pharmacy_km)
ggsave(paste0(dir, "figures/grocery_km_vs_pharmacy_km.png"), grocery_km_vs_pharmacy_km)


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