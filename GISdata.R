# Generate a GIS dataset for the UK 2015-2025.
# This will create the "gis-data.gpkg" file used to filter the study region 


library(dplyr)
library(rnaturalearth)
library(sf)
library(ggplot2)

# file to save spatial data
gpkg_file <- "data/gis-data.gpkg"
dir.create(dirname(gpkg_file), showWarnings = FALSE, recursive = TRUE)

# political boundaries
# land border with lakes removed
ne_land <- ne_download(scale = 50, category = "cultural",
                       type = "admin_0_countries_lakes",
                       returnclass = "sf") |>
  filter(CONTINENT %in% c("Europe")) |>
  st_set_precision(1e6) |>
  st_union()
# country boundaries
ne_countries <- ne_download(scale = 50, category = "cultural",
                            type = "admin_0_countries_lakes",
                            returnclass = "sf") |>
  select(country = ADMIN, country_code = ISO_A2)
# state boundaries for the UK
ne_states <- ne_download(scale = 50, category = "cultural",
                         type = "admin_0_map_subunits",
                         returnclass = "sf") |> 
  filter(ADMIN == "United Kingdom") |> 
  select(name = NAME, iso_a2 = ISO_A2)
# country lines
# downloaded globally then filtered to UK st_intersect()
ne_country_lines <- ne_download(scale = 50, category = "cultural",
                                type = "admin_0_boundary_lines_land",
                                returnclass = "sf") |> 
  st_geometry()
lines_on_land <- st_intersects(ne_country_lines, ne_land, sparse = FALSE) |>
  as.logical()
ne_country_lines <- ne_country_lines[lines_on_land]
# states
ne_state_lines <- ne_download(scale = 50, category = "cultural",
                              type = "admin_1_states_provinces_lines",
                              returnclass = "sf") |>
  filter(ADM0_A3 %in% c("GBR")) |>
  mutate(iso_a2 = recode(ADM0_A3, GB = "GB")) |> 
  select(country = ADM0_NAME, country_code = iso_a2)

# save all layers to a geopackage
unlink(gpkg_file)
write_sf(ne_land, gpkg_file, "ne_land")
write_sf(ne_countries, gpkg_file, "ne_countries")
write_sf(ne_states, gpkg_file, "ne_states")
write_sf(ne_country_lines, gpkg_file, "ne_country_lines")
write_sf(ne_state_lines, gpkg_file, "ne_state_lines")



# Exploratory analysis and visualisation
# load gis data
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") |> 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") |> 
  st_geometry()
ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") |> 
  st_geometry()
study_region <- read_sf("data/gis-data.gpkg", "ne_states") |> 
  st_geometry()

# prepare ebird data for mapping
checklists_sf <- checklists |> 
  # convert to spatial points
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(species_observed)

# map
par(mar = c(0.25, 0.25, 4, 0.25))
# set up plot area
plot(st_geometry(checklists_sf), 
     main = "Willow Tit eBird Observations\nFeb-Apr 2015-2023",
     col = NA, border = NA)
# contextual gis data
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)
plot(study_region, col = "#e6e6e6", border = NA, add = TRUE)
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
# ebird observations
# not observed
plot(filter(checklists_sf, !species_observed),
     pch = 19, cex = 0.02, col = alpha("#555555", 0.1),
     add = TRUE)
# observed

plot(filter(checklists_sf, species_observed),
     pch = 19, cex = 0.3, col = alpha("#4daf4a", 1),
     add = TRUE)
# legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklist", "Willow Tit sighting"),
       pch = 19)
box()

