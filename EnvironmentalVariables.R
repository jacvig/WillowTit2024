# Find environmental variables 

# This code is intended to find land cover and elevation variables for a defined area
# and create a prediction grid for use in species distribution.
# NB. Requires MODIS data. See Processing_MODIS_files.R 
  
  ## Code adapted from Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, 
  ##  O.J. Robinson, E.T. Miller, T. Auer, S. Kelling, D. Fink, A. Johnston. 2020. 
  ##  Best Practices for Using eBird Data. Version 2.0. 
  ##  https://ebird.github.io/ebird-best-practices/ 
  ##  Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.5281/zenodo.3620739


# Landcover

library(dplyr)
library(exactextractr)
library(landscapemetrics)
library(readr)
library(sf)
library(stringr)
library(terra)
library(tidyr)
library(units)
library(viridis)



# load and inspect the landcover data
landcover <- rast("data/modis_mc12q1_2014-2023_crop.tif")
print(landcover)

# Rename the layers of the raster if not already done so
names(landcover) <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
names(landcover)

# map the data for a specific year
plot(as.factor(landcover[["Year_2023"]]), 
     main = "MODIS Landcover 2023",
     axes = FALSE)

# To see landscape classes
lc_classes <- read_csv("data/mcd12q1_umd_classes.csv")
knitr::kable(lc_classes)


# find full set of ebird checklist locations
checklists <- read_csv("data/checklists-zf_wiltit_jul_gb.csv") |> 
  # landcover data not availble for 2023, so use the closest year
  mutate(year_lc = as.character(pmin(year, 2022)))

# generate circular neighborhoods for all checklists
checklists_sf <- checklists |> 
  # identify unique location/year combinations
  distinct(locality_id, year_lc, latitude, longitude) |> 
  # generate a 3 km neighborhoods. 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
buffers <- st_buffer(checklists_sf, dist = set_units(1.5, "km"))
# Depending on size, this last step may take some time.

# for each location, crop and mask the landcover layer corresponding to the checklist year to the circular neighborhood around that checklist. 
# Then calculate percentage landcover (pland) and edge density (ed) metrics within each neighborhood
# This may take 30 minutes or longer.
lsm <- NULL
for (i in seq_len(nrow(buffers))) {
  buffer_i <- st_transform(buffers[i, ], crs = crs(landcover))
  year <- as.character(buffer_i$year_lc)
  
  # crop and mask landcover raster so all values outside buffer are missing
  lsm[[i]] <- crop(landcover[[year]], buffer_i) |> 
    mask(buffer_i) |> 
    # calculate landscape metrics
    calculate_lsm(level = "class", metric = c("pland", "ed")) |> 
    # add variables to uniquely identify each point
    mutate(locality_id = buffer_i$locality_id, 
           year_lc = buffer_i$year_lc) |> 
    select(locality_id, year_lc, class, metric, value)
}
lsm <- bind_rows(lsm)

lsm_wide <- lsm |> 
  # fill missing classes with zeros
  complete(nesting(locality_id, year_lc),
           class = lc_classes$class,
           metric = c("ed", "pland"),
           fill = list(value = 0)) |> 
  # bring in more descriptive names
  inner_join(select(lc_classes, class, label), by = "class") |> 
  # transform from long to wide format
  pivot_wider(values_from = value,
              names_from = c(class, label, metric),
              names_glue = "{metric}_c{str_pad(class, 2, pad = '0')}_{label}",
              names_sort = TRUE) |> 
  arrange(locality_id, year_lc)



# Elevation

# Download elevation data at 1 km resolution from https://www.earthenv.org/topography
# Calculate the mean and standard deviation of the elevation within 3 km diameter circular neighborhoods centered on each checklist location.


# elevation raster
elevation <- rast("data/elevation_1KMmd_GMTEDmd.tif")

# mean and standard deviation within each circular neighborhood
elev_buffer <- exact_extract(elevation, buffers, fun = c("mean", "stdev"),
                             progress = FALSE) |> 
  # add variables to uniquely identify each point
  mutate(locality_id = buffers$locality_id, year_lc = buffers$year_lc) |> 
  select(locality_id, year_lc, 
         elevation_mean = mean,
         elevation_sd = stdev)

# combine elevation and landcover
env_variables <- inner_join(elev_buffer, lsm_wide,
                            by = c("locality_id", "year_lc"))

# attach and expand to checklists
env_variables <- checklists |> 
  select(checklist_id, locality_id, year_lc) |> 
  inner_join(env_variables, by = c("locality_id", "year_lc")) |> 
  select(-locality_id, -year_lc)

# save to csv, dropping any rows with missing variables
write_csv(drop_na(env_variables), 
          "data/environmental-variables_checklists.csv", 
          na = "")



# Prediction grid

# using the MODIS landcover data from the most recent year for which they’re available in addition to elevation data
# create a template raster with cells equal in dimension to the diameter of the circular neighborhoods
# use an equal area coordinate reference system for the prediction grid; we’ll use a Lambert’s azimuthal equal area projection centered on the study region.

# Find centroid of the data
checklists_sf |> 
  st_union() |> 
  st_centroid() |> 
  st_coordinates() |> 
  round(1)
  #         X    Y
  # [1,] -1.6 52.9

# lambert's azimuthal equal area projection for UK
laea_crs <- st_crs("+proj=laea +lat_0=52.9 +lon_0=-1.6")

# Find layers for filtering
gpkg <- st_layers("data/gis-data.gpkg")
gpkg

# study region: United Kingdom
study_region <- read_sf("data/gis-data.gpkg", layer = "ne_states") |> 
   st_transform(crs = laea_crs)

# create a raster template covering the region with 3 km resolution
r <- rast(study_region, res = c(3000, 3000))

# fill the raster with 1s inside the study region
r <- rasterize(study_region, r, field = 1) |> 
  setNames("study_region")


# save for later use
r <- writeRaster(r, "data/prediction-grid.tif",
                 overwrite = TRUE,
                 gdal = "COMPRESS=DEFLATE")


# generate neighborhoods for the prediction grid cell centers. NB This may take some time to run
buffers_pg <- as.data.frame(r, cells = TRUE, xy = TRUE) |> 
  select(cell_id = cell, x, y) |> 
  st_as_sf(coords = c("x", "y"), crs = laea_crs, remove = FALSE) |> 
  st_transform(crs = 4326) |> 
  st_buffer(set_units(3, "km"))


# estimate landscape metrics for each cell in the prediction grid. NB This may take some time to run
lsm_pg <- NULL
for (i in seq_len(nrow(buffers_pg))) {
  buffer_i <- st_transform(buffers_pg[i, ], crs = crs(landcover))
  
  # crop and mask landcover raster so all values outside buffer are missing
  lsm_pg[[i]] <- crop(landcover[["2022"]], buffer_i) |> 
    mask(buffer_i) |> 
    # calcualte landscape metrics
    calculate_lsm(level = "class", metric = c("pland", "ed")) |> 
    # add variable to uniquely identify each point
    mutate(cell_id = buffer_i$cell_id) |> 
    select(cell_id, class, metric, value)
}
lsm_pg <- bind_rows(lsm_pg)

# transform to wide format
lsm_wide_pg <- lsm_pg |> 
  # fill missing classes with zeros
  complete(cell_id,
           class = lc_classes$class,
           metric = c("ed", "pland"),
           fill = list(value = 0)) |> 
  # bring in more descriptive names
  inner_join(select(lc_classes, class, label), by = "class") |> 
  # transform from long to wide format
  pivot_wider(values_from = value,
              names_from = c(class, label, metric),
              names_glue = "{metric}_c{str_pad(class, 2, pad = '0')}_{label}",
              names_sort = TRUE,
              values_fill = 0) |> 
  arrange(cell_id)


# mean and standard deviation of elevation
elev_buffer_pg <- exact_extract(elevation, buffers_pg, 
                                fun = c("mean", "stdev"),
                                progress = FALSE) |> 
  # add variables to uniquely identify each point
  mutate(cell_id = buffers_pg$cell_id) |> 
  select(cell_id, elevation_mean = mean, elevation_sd = stdev)


# combine landcover and elevation
env_variables_pg <- inner_join(elev_buffer_pg, lsm_wide_pg, by = "cell_id")

# attach the xy coordinates of the cell centers
env_variables_pg <- buffers_pg |> 
  st_drop_geometry() |> 
  select(cell_id, x, y) |> 
  inner_join(env_variables_pg, by = "cell_id")

# save as csv, dropping any rows with missing variables
write_csv(drop_na(env_variables_pg),
          "data/environmental-variables_prediction-grid.csv", 
          na = "")



  # Write R objects to csv to save. Where an object takes a long time to run, save it individually and then read it back in when needed.
  # e.g.write.csv(x=buffers, file ="Robjects/buffers.csv" )

  # To read back in
  # buffers<- read.csv(file = "Robjects/buffers.csv")

# Example map. Convert environmental variables to a spatial format. NB. make sure laea_crs and r objects are available.
forest_cover <- env_variables_pg |> 
  # convert to spatial features
  st_as_sf(coords = c("x", "y"), crs = laea_crs) |> 
  # rasterize points
  rasterize(r, field = "pland_c04_deciduous_broadleaf")

# make a map
par(mar = c(0.25, 0.25, 2, 0.25))
plot(forest_cover, 
     axes = FALSE, box = FALSE, col = viridis(10), 
     main = "Deciduous Broadleaf Forest (% cover)")
