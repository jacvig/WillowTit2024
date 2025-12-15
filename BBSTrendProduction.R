# Modeling population trends using Breeding Bird Survey dataset. 
# See BBSWillowTit.R for data processing code.
# BBS methodology for modeling populations is outlined in https://onlinelibrary.wiley.com/doi/10.1111/geb.13943 

# Steps to modelling population trends:
  # 1.Remove squares surveyed in only a single year. 
  # These will not contribute information about variation across years.

  # 2.Any square in which a species has never been recorded is removed.
  # These will not contribute information on species trends.

  # 3.Summing counts over distance bands, transect sections and modes of detection, 
  # and taking the maximum across visits to get square-specific, year-specific maximum counts per species.

  # 4.For all remaining squares, zeroes are imputed where a species was not detected 
  # in a square in a year but that square was surveyed. 

# Please note: Perplexity.ai was used to support writing and troubleshooting the code.

# Download packages
library(tidyverse)
library(lubridate)
library(dplyr)


# 1.Remove all single-surveyed squares

multi_sites<- merged_df |> 
  group_by(square) |> 
  filter(n_distinct(year) > 1) |> 
  ungroup()
  # 7,053,673 rows, (16,904 observations were only surveyed in a single year)

# 2.Remove any square in which WT has never been recorded
 filtered_multi<- multi_sites |> 
  group_by(square) |> 
    filter(any(species_code == "WT", na.rm = TRUE)) |> 
   ungroup()
# 956,801 (6,096,872 observations are from sites that never recorded WT)
 
      # The code:  multi_sites |> 
              #group_by(square) |> 
              #filter(species_code != "WT") |> 
              #ungroup()
     # is not the same as the above.

 
# Number of squares surveyed
 length(unique(filtered_multi$square))
 #608

# Split year, month, day 
 filtered_multi<- filtered_multi |> 
   mutate(date = as.Date(date),   #The column "date" needs to be in the data for this to work.
        Year = year(date),
        Month = month(date),
        Day = day(date))
 
 
# 3. Create counts for distance bands, transect sections and modes of detection
# Counts for sections
filtered_multi <- filtered_multi |> 
  mutate(section_count = rowSums(across(section_1:section_10), na.rm = TRUE))


# plot counts for WTs 
filtered_multi |> 
  filter(species_code == "WT") |> 
  ggplot(mapping = aes(x=year, y= section_count, fill = visit))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Willow Tit high counts",
       y = "high count")+
  theme_minimal()


# Find maximum counts across visits to get square-specific, year-specific maximum counts per species: 

# Max counts per year per species
year_counts<- filtered_multi |> 
  select(year, square, species_code, section_count) |> 
  group_by(year, species_code) |> 
  summarise(year_count = max(section_count, na.rm = TRUE),.groups = "drop")


# Max counts WT across years
max_counts<- filtered_multi |> 
 filter(species_code == "WT") |> 
  select(year, square, visit, species_code, section_count) |> 
  group_by(year, square, visit) |> 
  summarise(high_count = max(section_count, na.rm = TRUE))

Max_WT <-max_counts |> 
  group_by(year) |> 
  summarise(total = max(high_count, na.rm = TRUE))

Max_WT |> 
  ggplot(mapping = aes(x = year, y = total))+ 
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()

# add regression lines
Max_WT |> 
  ggplot(mapping = aes(x=year, y=total))+
  geom_point(position = position_dodge(width = 0.1)) + 
  geom_smooth(method = "lm", se = FALSE)+  # Add the regression line
  labs(title = "max counts of Willow Tits", x = "year", y = "count")+
  scale_color_hue()+
  theme(plot.title = element_text(hjust = 0.5))

max_counts |> 
  ggplot(mapping = aes(x= year, y= high_count, fill = visit))+
  geom_point(position = position_dodge(width = 0.1))+
  geom_smooth(method = "lm", se =FALSE)+
  theme_minimal()

################################
# TESTS to see if linear regression assumptions are correct
# https://www.youtube.com/watch?v=1lwvNLDSu0s

# assumption 1: relationship between independent and dependent variables is linear 
# residual vs fitted test. If there appears to be a pattern, there are other things going on.
# Randomness suggests the model *is* linear
model <- lm(total ~ year, data = Max_WT)
fitted_values <- fitted(model)
residuals <- resid(model)
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
# Add horizontal line at 0
abline(h = 0, col = "red")

# assumption 2: residuals are normally distributed
# use histogram
hist(residuals, freq = FALSE, main = "Histogram of Residuals", xlab = "Residuals")
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), add = TRUE, col = "red", lwd = 2)

# if not sure, make a Q-Q plot
model <- lm(Y ~ X, data = your_data)
residuals <- resid(model)
qqnorm(residuals)
qqline(residuals, col = "red")

# assumption 3: residuals are homoscedastic i.e. variance is evenly distributed across the values
# look at first model again

# What I don't understand is that the residuals appear to be linear and they follow 
# a normal distribution. So why did BTO use a Poisson instead of a GLIM? 
#################################


# Max count per square per species
square_counts<- filtered_multi |> 
  select(square, species_code, section_count) |> 
  group_by(square, species_code) |> 
  summarise(square_counts = max(section_count, na.rm = TRUE))

# for WT
square_counts |> 
  filter(species_code == "WT") |> 
  ggplot(mapping = aes(x = square, y = square_counts))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()



band_counts<- filtered_multi |> 
  select(distance_band, species_code, section_count) |> 
  group_by(distance_band, species_code) |> 
  summarise(band_counts = max(section_count, na.rm = TRUE),.groups = "drop")



# 4. Zero fill
# find squares in a year that did not record WT
squares_missing_WT <- filtered_multi |> 
  group_by(year, square) |> 
  summarize(has_WT = any(species_code == "WT")) |> 
  filter(!has_WT) |> 
  select(year, square)

# Add rows for these squares with a WT count of 0
new_rows <- squares_missing_WT |> 
  mutate(species_code = "WT", section_count = 0)

# Add these new rows into the original dataframe
WillowTit_zf <- filtered_multi |> 
  bind_rows(new_rows) |> 
  arrange(year, square, species_code)


# Subset the dataframe so only WT rows are kept
WillowTit_data <- subset(WillowTit_zf, species_code == "WT")


# Check if predictors are factors
is.factor(WillowTit_data$square)
is.factor(WillowTit_data$year)

# if FALSE, convert
WillowTit_data$square <- as.factor(WillowTit_data$square)
WillowTit_data$year <- as.factor(WillowTit_data$year)



# GENERALISED LINEAR MODEL

# Fit the generalized linear model (GLM) with a Poisson distribution and log link
mod <- glm(section_count ~ square + year, family = poisson(link = "log"), data = WillowTit_data)

summary(mod)

## Coefficients:
##               Estimate   Std. Error z value Pr(>|z|)    
## (Intercept)   0.428479   0.716081   0.598  0.54960    # Baseline log expected count for the reference levels of all categorical predictors.
## squareNT8268 -2.657013   1.226302  -2.167  0.03026 *  

# For each square, the Estimate, Standard Error, z value, and P value are given




# NEGATIVE BINOMIAL MODEL 

# Fit a negative binomial model as well
# First, load the MASS package
library(MASS)

# Fit the negative binomial GLM
mod_nb <- glm.nb(section_count ~ square + year, data = WillowTit_data)

# View the summary just like a regular GLM
summary(mod_nb)



# VISUALISE 

# Estimate expected counts and visualise the Poisson model
# Create a new data frame with all site-year combinations for prediction
WTnewdata <- expand.grid(square = levels(WillowTit_data$square), year = levels(WillowTit_data$year))

# Predict expected counts (trend) for each site-year
WTnewdata$predicted_count <- predict(mod, WTnewdata, type = "response")

# Summarise trends by year
library(dplyr)
trend <- WTnewdata |> 
  group_by(year) |> 
  summarize(mean_count = mean(predicted_count))

#plot population trends over years (Poisson model)
library(ggplot2)
ggplot(trend, aes(x = year, y = mean_count)) +
  geom_line() +
  geom_point() +
  labs(title = "Population Trend Over Time",
       x = "Year", y = "Predicted Mean Count")



#Estimate expected counts and visualise the negative binomial
WTnewdata <- expand.grid(square = levels(WillowTit_data$square), year = levels(WillowTit_data$year))

WTnewdata$predicted_count <- predict(mod_nb, WTnewdata, type = "response")

trend <- WTnewdata |> 
  group_by(year) |> 
  summarize(mean_count = mean(predicted_count))

ggplot(trend, aes(x = year, y = mean_count)) +
  geom_line() +
  geom_point() +
  labs(title = "Population Trend Over Time",
       x = "Year", y = "Predicted Mean Count")



####NEXT STEPS####
#Interpret the trend:
  
##  The model coefficients reflect multiplicative changes in expected count on the log scale.

##  Plotting predicted counts helps visualize if the population is increasing, decreasing, or stable over years.

##  You may also want to calculate confidence intervals around predictions for uncertainty visualisation.

#Further analyses:
  
##  Check residuals and model fit diagnostics.

##  Assess overdispersion or zero inflation if relevant.

##  Fit alternative models or include covariates if needed.