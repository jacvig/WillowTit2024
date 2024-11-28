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



# Threshholding

# mcc and fscore calculation for various thresholds
mcc_f1 <- mccf1(
  # observed detection/non-detection
  response = obs_pred$obs,
  # predicted encounter rate from random forest
  predictor = obs_pred$pred)

# identify best threshold
mcc_f1_summary <- summary(mcc_f1)
#>  mccf1_metric best_threshold
#>  0.328892      0.7729254
threshold <- mcc_f1_summary$best_threshold[1]
  #> [1] 0.7729254



# Assessment 

# get the test set held out from training
checklists_test <- filter(checklists_ss, type == "test") |> 
  mutate(species_observed = as.integer(species_observed))

# predict to test data using random forest model
pred_er <- predict(er_model, data = checklists_test, type = "response")
# extract probability of detection
pred_er <- pred_er$predictions[, 2]
# convert predictions to binary (presence/absence) using the threshold
pred_binary <- as.integer(pred_er > threshold)
# calibrate
pred_calibrated <- predict(calibration_model, 
                           newdata = data.frame(pred = pred_er), 
                           type = "response") |> 
  as.numeric()
# constrain probabilities to 0-1
pred_calibrated[pred_calibrated < 0] <- 0
pred_calibrated[pred_calibrated > 1] <- 1
# combine observations and estimates
obs_pred_test <- data.frame(id = seq_along(pred_calibrated),
                            # actual detection/non-detection
                            obs = as.integer(checklists_test$species_observed),
                            # binary detection/on-detection prediction
                            pred_binary = pred_binary,
                            # calibrated encounter rate
                            pred_calibrated = pred_calibrated)

# mean squared error (mse)
mse <- mean((obs_pred_test$obs - obs_pred_test$pred_calibrated)^2, na.rm = TRUE)

# precision-recall auc
em <- precrec::evalmod(scores = obs_pred_test$pred_binary, 
                       labels = obs_pred_test$obs)
pr_auc <- precrec::auc(em) |> 
  filter(curvetypes == "PRC") |> 
  pull(aucs)

# calculate metrics for binary prediction: sensitivity, specificity
pa_metrics <- obs_pred_test |> 
  select(id, obs, pred_binary) |> 
  PresenceAbsence::presence.absence.accuracy(na.rm = TRUE, st.dev = FALSE)

# mcc and f1
mcc_f1 <- calculate_mcc_f1(obs_pred_test$obs, obs_pred_test$pred_binary)

# combine ppms together
ppms <- data.frame(
  mse = mse,
  sensitivity = pa_metrics$sensitivity,
  specificity = pa_metrics$specificity,
  pr_auc = pr_auc,
  mcc = mcc_f1$mcc,
  f1 = mcc_f1$f1
)
knitr::kable(pivot_longer(ppms, everything()), digits = 3)
  #>  |name        | value|
  #>  |:-----------|-----:|
  #>  |mse         | 0.007| # mean squared error
  #>  |sensitivity | 0.498|
  #>  |specificity | 0.989|
  #>  |pr_auc      | 0.152| # precision-recall auc
  #>  |mcc         | 0.367| # mean square contingency coefficient
  #>  |f1          | 0.360| # F score
  #>  
  #>  # The interpretation, I think, is that the closer each of these PPMs is to 1, the more likely.
      # NB Remember eBird data is imbalanced with more non-detections so this has an impact on the predictions.




# Habitat associations


# Predictor importance

# extract predictor importance from the random forest model object
pred_imp <- er_model$variable.importance
pred_imp <- data.frame(predictor = names(pred_imp), 
                       importance = pred_imp) |> 
  arrange(desc(importance))
# plot importance of top 20 predictors
ggplot(head(pred_imp, 20)) + 
  aes(x = reorder(predictor, importance), y = importance) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 2, colour = "#555555") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, 
       y = "Predictor Importance (Gini Index)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#cccccc", linewidth = 0.5))



# Partial dependence

# function to calculate partial dependence for a single predictor
calculate_pd <- function(predictor, er_model, calibration_model,
                         data, x_res = 25, n = 1000) {
  # create prediction grid using quantiles
  x_grid <- quantile(data[[predictor]],
                     probs = seq(from = 0, to = 1, length = x_res),
                     na.rm = TRUE)
  # remove duplicates
  x_grid <- x_grid[!duplicated(signif(x_grid, 8))]
  x_grid <- unname(unique(x_grid))
  grid <- data.frame(predictor = predictor, x = x_grid)
  names(grid) <- c("predictor", predictor)
  
  # subsample training data
  n <- min(n, nrow(data))
  data <- data[sample(seq.int(nrow(data)), size = n, replace = FALSE), ]
  
  # drop focal predictor from data
  data <- data[names(data) != predictor]
  grid <- merge(grid, data, all = TRUE)
  
  # predict encounter rate
  p <- predict(er_model, data = grid)
  
  # summarize
  pd <- grid[, c("predictor", predictor)]
  names(pd) <- c("predictor", "x")
  pd$encounter_rate <- p$predictions[, 2]
  pd <- dplyr::group_by(pd, predictor, x)
  pd <- dplyr::summarise(pd,
                         encounter_rate = mean(encounter_rate, na.rm = TRUE),
                         .groups = "drop")
  
  # calibrate
  pd$encounter_rate <- predict(calibration_model, 
                               newdata = data.frame(pred = pd$encounter_rate), 
                               type = "response")
  pd$encounter_rate <- as.numeric(pd$encounter_rate)
  # constrain to 0-1
  pd$encounter_rate[pd$encounter_rate < 0] <- 0
  pd$encounter_rate[pd$encounter_rate > 1] <- 1
  
  return(pd)
}

# calculate partial dependence for each of the top 6 predictors
pd <- NULL
for (predictor in head(pred_imp$predictor)) {
  pd <- calculate_pd(predictor, 
                     er_model = er_model, 
                     calibration_model = calibration_model,
                     data = checklists_train) |> 
    bind_rows(pd)
}
head(pd)
    #> # A tibble: 6 × 3
    #>   predictor                         x encounter_rate
    #>   <chr>                         <dbl>          <dbl>
    #>  1 ed_c13_urban  0           0.00270
    #>  2 ed_c13_urban  1.08        0.00278
    #>  3 ed_c13_urban  2.16        0.00274
    #>  4 ed_c13_urban  3.16        0.00281
    #>  5 ed_c13_urban  3.78        0.00279
    #>  6 ed_c13_urban  4.32        0.00286

# plot partial dependence
ggplot(pd) +
  aes(x = x, y = encounter_rate) +
  geom_line() +
  geom_point() +
  facet_wrap(~ factor(predictor, levels = rev(unique(predictor))), 
             ncol = 2, scales = "free") +
  labs(x = NULL, y = "Encounter Rate") +
  theme_minimal() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "grey60"),
        axis.ticks  = element_line(color = "grey60"))

      
      
      # calculate partial dependence for a single varaible
      pd_woody_savanna <- calculate_pd("pland_c08_woody_savanna", 
                                      er_model = er_model, 
                                      calibration_model = calibration_model,
                                       data = checklists_train)

      # plot partial dependence
      ggplot(pd_woody_savanna) +
        aes(x = x, y = encounter_rate) +
        geom_line() +
        geom_point() +
        labs(x = NULL, y = "Encounter Rate") +
        theme_minimal() +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.line = element_line(color = "grey60"),
              axis.ticks  = element_line(color = "grey60"))


  

     
  
# Prediction

# Standardised effort variables
# find peak time of day from partial dependence
pd_time <- calculate_pd("hours_of_day",
                        er_model = er_model, 
                        calibration_model = calibration_model,
                        data = checklists_train) |> 
  select(hours_of_day = x, encounter_rate)

# histogram
g_hist <- ggplot(checklists_train) +
  aes(x = hours_of_day) +
  geom_histogram(binwidth = 1, center = 0.5, color = "grey30",
                 fill = "grey50") +
  scale_x_continuous(breaks = seq(0, 24, by = 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Hours since midnight",
       y = "# checklists",
       title = "Distribution of observation start times")

# partial dependence plot
g_pd <- ggplot(pd_time) +
  aes(x = hours_of_day, y = encounter_rate) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 24, by = 3)) +
  labs(x = "Hours since midnight",
       y = "Encounter rate",
       title = "Observation start time partial dependence")

# combine
grid.arrange(g_hist, g_pd)


# trim ends of partial dependence
pd_time_trimmed <- pd_time[c(-1, -nrow(pd_time)), ]

# identify time maximizing encounter rate
pd_time_trimmed <- arrange(pd_time_trimmed, desc(encounter_rate))
t_peak <- pd_time_trimmed$hours_of_day[1]
print(t_peak)
#> [1] 6.866667


# add effort covariates to prediction grid
pred_grid_eff <- pred_grid |> 
  mutate(observation_date = ymd("2023-03-16"), #for the middle of our focal window for the latest year for which we have eBird data.i.e. 16 March 2022
         year = year(observation_date),
         day_of_year = yday(observation_date),
         hours_of_day = t_peak,
         effort_distance_km = 2,
         effort_hours = 1,
         effort_speed_kmph = 2,
         number_observers = 1)



# Model estimates

# estimate encounter rate
pred_er <- predict(er_model, data = pred_grid_eff, type = "response")
pred_er <- pred_er$predictions[, 2]
# define range-boundary
pred_binary <- as.integer(pred_er > threshold)
# apply calibration
pred_er_cal <- predict(calibration_model, 
                       data.frame(pred = pred_er), 
                       type = "response") |> 
  as.numeric()
# constrain to 0-1
pred_er_cal[pred_er_cal < 0] <- 0
pred_er_cal[pred_er_cal > 1] <- 1
# combine predictions with coordinates from prediction grid
predictions <- data.frame(cell_id = pred_grid_eff$cell_id,
                          x = pred_grid_eff$x,
                          y = pred_grid_eff$y,
                          in_range = pred_binary, 
                          encounter_rate = pred_er_cal)


r_pred <- predictions |> 
  # convert to spatial features
  st_as_sf(coords = c("x", "y"), crs = crs) |> 
  select(in_range, encounter_rate) |> 
  # rasterize
  rasterize(r, field = c("in_range", "encounter_rate"))
print(r_pred)
#> class       : SpatRaster 
#> dimensions  : 399, 217, 2  (nrow, ncol, nlyr)
#> resolution  : 3000, 3000  (x, y)
#> extent      : -423875.9, 227124.1, -313763.3, 883236.7  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=laea +lat_0=52.9 +lon_0=-1.6 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#> source(s)   : memory
#> names       : in_range, encounter_rate 
#> min values  :        0,      0.0000000 
#> max values  :        1,      0.1511175 





# Mapping 

par(mar = c(0.25, 0.25, 1.25, 0.25))
# set up plot area
plot(study_region, 
     main = "Willow Tit Range (March 2023)",
     col = NA, border = NA)
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)

# convert binary prediction to categorical
r_range <- as.factor(r_pred[["in_range"]])
plot(r_range, col = c("#e6e6e6", "forestgreen"),
     maxpixels = ncell(r_range),
     legend = FALSE, axes = FALSE, bty = "n",
     add = TRUE)

# borders
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
plot(study_region, border = "#000000", col = NA, lwd = 1, add = TRUE)
box()


# encounter rate
par(mar = c(4, 0.25, 0.25, 0.25))
# set up plot area
plot(study_region, col = NA, border = NA)
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)

# define quantile breaks
brks <- global(r_pred[["encounter_rate"]], fun = quantile, 
               probs = seq(0, 1, 0.1), na.rm = TRUE) |> 
  as.numeric() |> 
  unique()
# label the bottom, middle, and top value
lbls <- round(c(0, median(brks), max(brks)), 2)
# ebird status and trends color palette
pal <- ebirdst_palettes(length(brks) - 1)
plot(r_pred[["encounter_rate"]], 
     col = pal, breaks = brks, 
     maxpixels = ncell(r_pred),
     legend = FALSE, axes = FALSE, bty = "n",
     add = TRUE)

# borders
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
plot(study_region, border = "#000000", col = NA, lwd = 1, add = TRUE)
box()

# legend
par(new = TRUE, mar = c(0, 0, 0, 0))
title <- "Willow Tit Encounter Rate (March 2023)"
image.plot(zlim = c(0, 1), legend.only = TRUE, 
           col = pal, breaks = seq(0, 1, length.out = length(brks)),
           smallplot = c(0.25, 0.75, 0.03, 0.06),
           horizontal = TRUE,
           axis.args = list(at = c(0, 0.5, 1), labels = lbls,
                            fg = "black", col.axis = "black",
                            cex.axis = 0.75, lwd.ticks = 0.5,
                            padj = -1.5),
           legend.args = list(text = title,
                              side = 3, col = "black",
                              cex = 1, line = 0))
