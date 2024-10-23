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
  ## Check how much subsampling has reduced the number of rows.
  # original data
    nrow(checklists_env)
    #> [1] 269775
    count(checklists_env, species_observed) |> 
      mutate(percent = n / sum(n))
    #> # A tibble: 2 × 3
    #>   species_observed     n percent
    #>   <lgl>            <int>   <dbl>
    #>  1 FALSE            268319 0.995  
    #>  2 TRUE               1456 0.00540

    # after sampling
    nrow(checklists_ss)
    #> [1] 95625
    count(checklists_ss, species_observed) |> 
      mutate(percent = n / sum(n))
    #> # A tibble: 2 × 3
    #>   species_observed     n percent
    #>   <lgl>            <int>   <dbl>
    #>  1 FALSE            94690 0.990  
    #>  2 TRUE               935 0.00978      # subsampling decreased the overall number of checklists by a factor of 2.8 but increased the prevalence of detections from .5% to .9% 





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



# Random forest
  ## Will use a balanced forest model 

# filter to training set
checklists_train <- checklists_ss |> 
  filter(type == "train") |> 
  # select only the columns to be used in the model
  select(species_observed,
         year, day_of_year, hours_of_day,
         effort_hours, effort_distance_km, effort_speed_kmph,
         number_observers, 
         starts_with("pland_"),
         starts_with("ed_"),
         starts_with("elevation_"))


# calculate detection frequency
detection_freq <- mean(checklists_train$species_observed)

# ranger requires a factor response to do classification
er_model <- ranger(formula =  as.factor(species_observed) ~ ., 
                   data = checklists_train,
                   importance = "impurity",
                   probability = TRUE,
                   replace = TRUE, 
                   sample.fraction = c(detection_freq, detection_freq))


# Calibration

# predicted encounter rate based on out of bag samples
er_pred <- er_model$predictions[, 2]
# observed detection, converted back from factor
det_obs <- as.integer(checklists_train$species_observed)
# construct a data frame to train the scam model
obs_pred <- data.frame(obs = det_obs, pred = er_pred)

# train calibration model
calibration_model <- scam(obs ~ s(pred, k = 6, bs = "mpi"), 
                          gamma = 2,
                          data = obs_pred)


# group the predicted encounter rate into bins of width 0.02
# then calculate the mean observed encounter rates in each bin
er_breaks <- seq(0, 1, by = 0.02)
mean_er <- obs_pred |>
  mutate(er_bin = cut(pred, breaks = er_breaks, include.lowest = TRUE)) |>
  group_by(er_bin) |>
  summarise(n_checklists = n(),
            pred = mean(pred), 
            obs = mean(obs),
            .groups = "drop")

# make predictions from the calibration model
calibration_curve <- data.frame(pred = er_breaks)
cal_pred <- predict(calibration_model, calibration_curve, type = "response")
calibration_curve$calibrated <- cal_pred

# compared binned mean encounter rates to calibration model
ggplot(calibration_curve) +
  aes(x = pred, y = calibrated) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_line(color = "blue") +
  geom_point(data = mean_er, 
             aes(x = pred, y = obs),
             size = 2, alpha = 0.6,
             show.legend = FALSE) +
  labs(x = "Estimated encounter rate",
       y = "Observed encounter rate",
       title = "Calibration model") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1))

  ## The estimated encounter rates are larger than the observed encounter rates: All points fall below the x-y line, i.e. it is not well calibrated
  ## though relative ranking is good: points with higher estimated rates also have higher observed rates.
