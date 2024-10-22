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
env_vars <- read_csv("data/environmental-variables_checklists_jun_us-ga.csv")

# zero-filled ebird data combined with environmental data
checklists_env <- read_csv("data/checklists-zf_woothr_jun_us-ga.csv") |> 
  inner_join(env_vars, by = "checklist_id")

# prediction grid
pred_grid <- read_csv("data/environmental-variables_prediction-grid_us-ga.csv")
# raster template for the grid
r <- rast("data/prediction-grid_us-ga.tif")
# get the coordinate reference system of the prediction grid
crs <- st_crs(r)

# load gis data for making maps
study_region <- read_sf("data/gis-data.gpkg", "ne_states") |> 
  filter(state_code == "US-GA") |> 
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