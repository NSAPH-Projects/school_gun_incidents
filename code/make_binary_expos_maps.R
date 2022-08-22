library(usmap)
library(ggplot2)

dir <- "/Users/s012852/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Bargagli Stoffi, Falco Joannes - Schools Vs Firearms/"

# get continuous exposure (in km) and county FIPS data
cleaned_data <- read_cleaned_data_as_df()
data_analysis_with_county <- get_analysis_df(cleaned_data, treatment = "mean_total_km", c("county_fips", all_confounder_names))
data_analysis_with_county <- na.omit(data_analysis_with_county)
exposure_and_county <- subset(data_analysis_with_county, select = c("a", "county_fips"))
exposure_and_county$county_fips <- ifelse(nchar(exposure_and_county$county_fips) == 4, paste0("0", exposure_and_county$county_fips), exposure_and_county$county_fips)
colnames(exposure_and_county) <- c("mean_total_km", "fips")

# dichotomize treatment, with multiple alternative cutoffs
binary_exposure1km <- copy(exposure_and_county)
binary_exposure1.609km <- copy(exposure_and_county)
binary_exposure2km <- copy(exposure_and_county)
binary_exposure3.218km <- copy(exposure_and_county)
binary_exposure1km$mean_total_km <- binary_exposure1km$mean_total_km > 1
binary_exposure1.609km$mean_total_km <- binary_exposure1.609km$mean_total_km > 1.609
binary_exposure2km$mean_total_km <- binary_exposure2km$mean_total_km > 2
binary_exposure3.218km$mean_total_km <- binary_exposure3.218km$mean_total_km > 3.218

# make maps
map1km <- plot_usmap(regions = "counties", data = binary_exposure1km, values = "mean_total_km")
map1mi <- plot_usmap(regions = "counties", data = binary_exposure1.609km, values = "mean_total_km")
map2km <- plot_usmap(regions = "counties", data = binary_exposure2km, values = "mean_total_km")
map2mi <- plot_usmap(regions = "counties", data = binary_exposure3.218km, values = "mean_total_km")
ggsave(paste0(dir, "exploratory_figures/binary_exposure_maps/", "counties_map_cutoff1km.png"), map1km)
ggsave(paste0(dir, "exploratory_figures/binary_exposure_maps/", "counties_map_cutoff1mi.png"), map1mi)
ggsave(paste0(dir, "exploratory_figures/binary_exposure_maps/", "counties_map_cutoff2km.png"), map2km)
ggsave(paste0(dir, "exploratory_figures/binary_exposure_maps/", "counties_map_cutoff2mi.png"), map2mi)
