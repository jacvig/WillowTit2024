# Modeling Willow Tit population data for the UK 2014-2023.


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
f_sed <- "data-raw/ebd_GB_wiltit1_smp_relJul-2024_sampling.txt"
checklists <- read_sampling(f_sed)
glimpse(checklists)

# Import observation data
f_ebd <- "data-raw/ebd_GB_wiltit1_smp_relJul-2024.txt"
observations <- read_ebd(f_ebd)
glimpse(observations)


# Filter checklist data
checklists <- checklists |> 
  filter(all_species_reported,
         between(year(observation_date), 2014, 2023),
         between(month(observation_date), 2,4)) # Feb-April the pre-breeding season is when WTs are territorial. Also this is the survey period for RSPB/RBPB surveys

# Filter observation data
observations <- observations |> 
  filter(all_species_reported,
         between(year(observation_date), 2014,2023),
         between(month(observation_date), 2,4)) 

# convert checklist locations to points geometries
checklists_sf <- checklists |> 
  select(checklist_id, latitude, longitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



# boundary of study region, buffered by 1 km
study_region_buffered <- read_sf("data/gis-data.gpkg", layer = "ne_states") |>
  filter(iso_a2 == "-99") |>
  st_transform(crs = st_crs(checklists_sf)) |>
  st_buffer(dist = 1000)

# spatially subset the checklists to those in the study region
in_region <- checklists_sf[study_region_buffered, ]

# join to checklists and observations to remove checklists outside region
checklists <- semi_join(checklists, in_region, by = "checklist_id")
observations <- semi_join(observations, in_region, by = "checklist_id")







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

 
# plot one of the effort variables to see distribution
ggplot(zf_filtered)+
  aes(x = effort_distance_km)+
  geom_histogram(binwidth = .5,
                 aes(y = after_stat(count/ sum(count))))+
  scale_y_continuous(limits = c(0, NA), labels = scales::label_percent())+
  labs(x = "km",
       y = "% of eBird checklists",
       title =  "Distribution of eBird checklist distance")



# randomly split the data into training 80% and testing 20% sets
zf_filtered$type <- if_else(runif(nrow(zf_filtered)) <= 0.8, "train", "test")
# confirm the proportion in each set is correct
table(zf_filtered$type) / nrow(zf_filtered)

# remove redundant variables
checklists <- zf_filtered |> 
  select(checklist_id, observer_id, type,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         hours_of_day, 
         effort_hours, effort_distance_km, effort_speed_kmph,
         number_observers)
write_csv(checklists, "data/checklists-zf_wiltit_jul_gb.csv", na = "")




# Time of day
# summarize data by hourly bins
breaks <- seq(0, 24)
labels <- breaks[-length(breaks)] + diff(breaks) / 2
checklists_time <- checklists |> 
  mutate(hour_bins = cut(hours_of_day, 
                         breaks = breaks, 
                         labels = labels,
                         include.lowest = TRUE),
         hour_bins = as.numeric(as.character(hour_bins))) |> 
  group_by(hour_bins) |> 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_tod_hist <- ggplot(checklists_time) +
  aes(x = hour_bins, y = n_checklists) +
  geom_segment(aes(xend = hour_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Hours since midnight",
       y = "# checklists",
       title = "Distribution of observation start times")

# frequency of detection
g_tod_freq <- ggplot(checklists_time |> filter(n_checklists > 100)) +
  aes(x = hour_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hours since midnight",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_tod_hist, g_tod_freq)


# Checklist duration
# summarize data by hour long bins
breaks <- seq(0, 6)
labels <- breaks[-length(breaks)] + diff(breaks) / 2
checklists_duration <- checklists |> 
  mutate(duration_bins = cut(effort_hours, 
                             breaks = breaks, 
                             labels = labels,
                             include.lowest = TRUE),
         duration_bins = as.numeric(as.character(duration_bins))) |> 
  group_by(duration_bins) |> 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_duration_hist <- ggplot(checklists_duration) +
  aes(x = duration_bins, y = n_checklists) +
  geom_segment(aes(xend = duration_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Checklist duration [hours]",
       y = "# checklists",
       title = "Distribution of checklist durations")

# frequency of detection
g_duration_freq <- ggplot(checklists_duration |> filter(n_checklists > 100)) +
  aes(x = duration_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Checklist duration [hours]",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_duration_hist, g_duration_freq)


# Distance travelled
# summarize data by 1 km bins
breaks <- seq(0, 10)
labels <- breaks[-length(breaks)] + diff(breaks) / 2
checklists_dist <- checklists |> 
  mutate(dist_bins = cut(effort_distance_km, 
                         breaks = breaks, 
                         labels = labels,
                         include.lowest = TRUE),
         dist_bins = as.numeric(as.character(dist_bins))) |> 
  group_by(dist_bins) |> 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_dist_hist <- ggplot(checklists_dist) +
  aes(x = dist_bins, y = n_checklists) +
  geom_segment(aes(xend = dist_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Distance travelled [km]",
       y = "# checklists",
       title = "Distribution of distance travelled")

# frequency of detection
g_dist_freq <- ggplot(checklists_dist |> filter(n_checklists > 100)) +
  aes(x = dist_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Distance travelled [km]",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_dist_hist, g_dist_freq)



# Number of observers
# summarize data
breaks <- seq(0, 10)
labels <- seq(1, 10)
checklists_obs <- checklists |> 
  mutate(obs_bins = cut(number_observers, 
                        breaks = breaks, 
                        label = labels,
                        include.lowest = TRUE),
         obs_bins = as.numeric(as.character(obs_bins))) |> 
  group_by(obs_bins) |> 
  summarise(n_checklists = n(),
            n_detected = sum(species_observed),
            det_freq = mean(species_observed))

# histogram
g_obs_hist <- ggplot(checklists_obs) +
  aes(x = obs_bins, y = n_checklists) +
  geom_segment(aes(xend = obs_bins, y = 0, yend = n_checklists),
               color = "grey50") +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "# observers",
       y = "# checklists",
       title = "Distribution of the number of observers")

# frequency of detection
g_obs_freq <- ggplot(checklists_obs |> filter(n_checklists > 100)) +
  aes(x = obs_bins, y = det_freq) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "# observers",
       y = "% checklists with detections",
       title = "Detection frequency")

# combine
grid.arrange(g_obs_hist, g_obs_freq)
