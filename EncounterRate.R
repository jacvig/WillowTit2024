# Estimate the encounter rate of Willow Tit on eBird checklists for February- April in the UK.

  ## Code adapted from Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, 
  ##  O.J. Robinson, E.T. Miller, T. Auer, S. Kelling, D. Fink, A. Johnston. 2020. 
  ##  Best Practices for Using eBird Data. Version 2.0. 
  ##  https://ebird.github.io/ebird-best-practices/ 
  ##  Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.5281/zenodo.3620739


library(dplyr)
library(ebirdst)
library(fields)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(mccf1)
library(ranger)
library(readr)
library(scam)
library(sf)
library(terra)
library(tidyr)

# set random number seed for reproducibility
set.seed(1)

# environmental variables: landcover and elevation
env_vars <- read_csv("data/environmental-variables_checklists.csv")

# zero-filled ebird data combined with environmental data
checklists_env <- read_csv("data/checklists-zf_wiltit_jul_gb.csv") |> 
  inner_join(env_vars, by = "checklist_id")

# prediction grid
pred_grid <- read_csv("data/environmental-variables_prediction-grid.csv")
# raster template for the grid
r <- rast("data/prediction-grid.tif")
# get the coordinate reference system of the prediction grid
crs <- st_crs(r)

# load gis data for making maps
study_region <- read_sf("data/gis-data.gpkg", "ne_states") |> 
  # filter(state_code == "GBR") |> 
  st_transform(crs = crs) |> 
  st_geometry()
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") |> 
  st_transform(crs = crs) |> 
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") |> 
  st_transform(crs = crs) |> 
  st_geometry()
ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") |> 
  st_transform(crs = crs) |> 
  st_geometry()



# Spatiotemporal subsampling
  ## subsampling to reduce spatial and temporal biases. 
  ## define an equal area, 3 km by 3 km square grid across the study region. Sample one detection and one non-detection checklist from each grid cell for each week of each year

# sample one checklist per 3km x 3km x 1 week grid for each year
# sample detection/non-detection independently 
checklists_ss <- grid_sample_stratified(checklists_env,
                                        obs_column = "species_observed",
                                        sample_by = "type")
  ## subsampling has reduced the number of rows by 174,150 (65%).


# convert checklists to spatial features
all_pts <- checklists_env |>  
  filter(type == "train") |> 
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |>
  st_transform(crs = crs) |> 
  select(species_observed)
ss_pts <- checklists_ss |>  
  filter(type == "train") |> 
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) |>
  st_transform(crs = crs) |> 
  select(species_observed)
both_pts <- list(before_ss = all_pts, after_ss = ss_pts)

# map
p <- par(mfrow = c(1, 2))
for (i in seq_along(both_pts)) {
  par(mar = c(0.25, 0.25, 0.25, 0.25))
  # set up plot area
  plot(st_geometry(both_pts[[i]]), col = NA)
  # contextual gis data
  plot(ne_land, col = "#dddddd", border = "#888888", lwd = 0.5, add = TRUE)
  plot(study_region, col = "#cccccc", border = NA, add = TRUE)
  plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
  plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
  # ebird observations
  # not observed
  plot(st_geometry(both_pts[[i]]),
       pch = 19, cex = 0.1, col = alpha("#555555", 0.25),
       add = TRUE)
  # observed
  plot(filter(both_pts[[i]], species_observed) |> st_geometry(),
       pch = 19, cex = 0.3, col = alpha("#4daf4a", 0.5),
       add = TRUE)
  # legend
  legend("bottomright", bty = "n",
         col = c("#555555", "#4daf4a"),
         legend = c("Non-detection", "Detection"),
         pch = 19)
  box()
  par(new = TRUE, mar = c(0, 0, 3, 0))
  if (names(both_pts)[i] == "before_ss") {
    title("Willow Tit eBird Observations\nBefore subsampling")
  } else {
    title("After subsampling")
  }
}
par(p)
