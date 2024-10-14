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
