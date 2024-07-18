# Modeling Willow Tit population data for the UK 2015-2024.


  ##  This script is adapted from Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, 
  ##  O.J. Robinson, E.T. Miller, T. Auer, S. Kelling, D. Fink, A. Johnston. 2020. 
  ##  Best Practices for Using eBird Data. Version 2.0. 
  ##  https://ebird.github.io/ebird-best-practices/ 
  ##  Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.5281/zenodo.3620739
  ##  Also available from https://cornelllabofornithology.github.io/ebird-best-practices/


# Prerequisites
# Working with the large eBird dataset requires the Unix command-line utility AWK
# Windows users need to install Cygwin (https://www.cygwin.com/) to access AWK.

# R Packages
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebird-best-practices")


library(auk)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library(sf)


# Import EBD (checklist) data
f_sed <- "ebd_GB_wiltit1_smp_relJun-2024_sampling.txt"
checklists <- read_sampling(f_sed)
glimpse(checklists)

# Import observation data
f_ebd <- "ebd_GB_wiltit1_smp_relJun-2024.txt"
observations <- read_ebd(f_ebd)
glimpse(observations)


# Filter checklist data
checklists <- checklists |> 
  filter(all_species_reported,
         between(year(observation_date), 2015, 2024),
         between(month(observation_date), 2,4)) # Feb-April the pre-breeding season is when WTs are territorial. Also this is the survey period for RSPB/RBPB surveys

# Filter observation data
observations <- observations |> 
  filter(all_species_reported,
         between(year(observation_date), 2015,2024),
         between(month(observation_date), 2,4)) 

# convert checklist locations to points geometries
checklists_sf <- checklists |> 
  select(checklist_id, latitude, longitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# zero-fill 
zf <- auk_zerofill(observations, checklists, collapse = TRUE)


# transform variables: 
# convert time to a decimal value between 0 and 24
# force the distance traveled to 0 for stationary checklists
# create a new variable for speed
# convert X to NA
# transform observation_count variable to an integer
# function to convert time observation to hours since midnight

time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
zf <- zf |> 
  mutate(
    # convert count to integer and X to NA
    # ignore the warning "NAs introduced by coercion"
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for stationary counts
    effort_distance_km = if_else(protocol_type == "Stationary", 
                                 0, effort_distance_km),
    # convert duration to hours
    effort_hours = duration_minutes / 60,
    # speed km/h
    effort_speed_kmph = effort_distance_km / effort_hours,
    # convert time to decimal hours since midnight
    hours_of_day = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )


# additional filtering to restrict checklists by duration and distance
zf_filtered <- zf |> 
  filter(protocol_type %in% c("Stationary", "Traveling"),
         effort_hours <= 6,
         effort_distance_km <= 10,
         effort_speed_kmph <= 100,
         number_observers <= 10)



