# This script is for exploring the Breeding Bird Survey dataset released https://zenodo.org/records/14161736
# This data contains data collected from the Breeding Bird Survey (BBS) between 1994 - 2023. The data are not all data held by the BTO
# The accompanying BBS paper can be found https://onlinelibrary.wiley.com/doi/10.1111/geb.13943 

# See BBSTrendProduction.R for modeling population trends

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(dplyr)

# Read in the count dataset. Each row represents a species observation for a visit with total number of individuals counted.
data<- read.csv("BBS_bird_dataset.csv") 

# read the survey visit dataset.Includes full date and weather data. Each square was surveyed E and L
visit<- read.csv("BBS_visit_dataset.csv")

# read the coordinate dataset. Information about each square.
coordinates<- read.csv("grid_square_coordinates_lookup.csv")


# Now we want to extract all Willow Tit observations for the UK across the years to map them
# We need to first find the exact date for each visit.

# Merge the count data and visit datasets
merged_df <- full_join(data, visit, by = c("year", "square", "visit"))

# Merge the coordinate data with the new dataframe so that each square with observations is matched with coordinates
merged_df <-full_join(merged_df, coordinates, by = "square")



# Filter and process
# Filter by species so only Willow Tit data are retained. This will be presence-only count data. 
# If presence/absence is required, it might be necessary to retain a row (unique by square and visit?) and fill section_1 to section_10 with 0.

# See species codes in the file species_lookup.csv
willowtit<- merged_df |> 
  filter(species_code == "WT")

# Between 1994-2023 there were 1479 WT observations across the UK

# See all BBS_region regions: England, Scotland, Wales
unique(willowtit$BBS_region)

# see all data in Manchester
willowtit |> 
  filter(BBS_region == "MANCX")


# DATES
#Add a new column that provides the cumulative numerical value for that day of the year
# Because 1996, 2000, 2004, 2008, 2012, 2016, 2020 were leap years, I will split the data into two data frames, run the modified code on each data frame then knit them together again.

#For non-leap years
#Create a data frame for these years
Unleap<-willowtit |> 
  filter(year %in% c(1994, 1995, 1997, 1998, 1999, 2001, 2002, 2003, 2005, 2006, 2007, 2009, 2010, 2011, 2013, 2014, 2015, 2017, 2018, 2019, 2021, 2022, 2023))

# Create two objects
days = c(31,28,31,30,31,30,31,31,30,31,30,31)  # Number of days in each month. 
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)


# Add the new column with values
Unleap<-Unleap |>  
  mutate(date = as.Date(date),   #The column Date needs to be in the data for this to work.
         Year = year(date),
         Month = month(date),
         Daym = day(date),
         Dayc = day(date) + cdays[Month])




#Do the same for the leap years. Separate them as own data frame
Leap <-willowtit |>  
  filter(year %in% c(1996, 2000, 2004, 2008, 2012, 2016, 2020))

dayleap = c(31,29,31,30,31,30,31,31,30,31,30,31)  # February 2020 needs 29 days 
cdayleap = c(0,32,60,91,121,152,182,213,244,274,305,335) #Each month thereafter will be +1 days

#Add columns and values into the data frame
Leap<- Leap |>  
  mutate(date = as.Date(date),
         Year = year(date),
         Month = month(date),
         Daym = day(date),
         Dayc = day(date) + cdayleap[Month]) #It doesn't seem to matter that the object dayleap was created.



#Combine the two into a new data frame
willowtit <-rbind(Unleap, Leap)



# section_1-10 gives number of individuals counted in each of the ten, 100m transect sections.
# For now, I only want to know total count so combine the values of these columns into a new column.

test <- willowtit |> 
  mutate(count = rowSums(across(section_1:section_10), na.rm = TRUE))

willowtit<- test |> 
  relocate(count, .after = section_10)


# Select only columns wanted
checklists <- willowtit |> 
  select(Year, Month, Daym, Dayc,
         square, visit, count,
         species_code, ETRS89Lat, ETRS89Long,
         BBS_region)

# check the character of the values 
str(checklists)
# if count is a character, convert to numeric
checklists$count <- as.numeric(as.character(checklists$count))




# EXPLORE THE DATA

# Total number of surveys performed
# 1479

# total number of sites surveyed
length(unique(checklists$square))
#OR
checklists |> 
  summarise(sites = n_distinct(square))
# 614 unique sites were surveyed

# Total number of surveys/checklists per year
Y_check<- checklists |> 
 count(Year)
  # this shows a decrease in the number of checklists.

# plot it
ggplot(Y_check, mapping = aes(x = Year, y = n))+
  geom_col()+
  labs(title = "Number of surveys per year",
       y = "Surveys",)+
  theme_bw()


# Total number of sites surveyed per year
checklists |> 
  group_by(Year) |> 
  summarise(sites = n_distinct(square)) |> 
  print(n = 30)
  # 1  1994    63
  # 2  1995    57
  # 3  1996    66
  # 4  1997    63
  # 5  1998    61
  # 6  1999    61
  # 7  2000    50
  # 8  2001    15
  # 9  2002    31
  # 10 2003    51

# plot it
u_sites<- checklists |> 
  group_by(Year) |> 
  summarise(sites = n_distinct(square))

  ggplot(u_sites, mapping = aes(x = Year, y = sites))+
  geom_col()+
    labs(title = "Sites surveyed per year")+
  theme_bw()


# Total number of checklists per site  
checklists |> 
  group_by(square) |> 
  summarise(number = n())|> 
  print(n = 30)

# number of sites that have only been surveyed one time
checklists |> 
  group_by(square) |> 
  filter(n() > 1)
# 327 sites have only been surveyed one time
# 287 sites have been surveyed more than once.

# Create a dataframe with only the squares that have more than one survey
Multi_sites<- checklists |> 
  group_by(square) |> 
  filter(n() > 1)



#total number of WT recorded from 1994-2023 across sites and visits.
sum(checklists$count)
# 2184

# total number of checklists for Manchester
MANCX <-checklists |> 
  filter(BBS_region == "MANCX")
# 100


# total number of surveys for a specific location (e.g.Manchester) for each year
checklists |>
  select(Year, BBS_region) |> 
  filter(BBS_region == "MANCX") |> 
  group_by(Year) |> 
  summarise(checklists_per_year = n(), .groups = 'drop') |> 
  View()
# OR

Man_survey<-MANCX |> 
  group_by(Year) |> 
  summarise(survey = n_distinct(square))

# plot
ggplot(Man_survey, mapping = aes(x = Year, y = survey))+
  geom_col()+
  labs(title = "Surveys per year for Manchester")+
  theme_bw()


# Number of unique sites in Manchester
length(unique(MANCX$square))
# 23

# Number of unique recorders
recorders<- merged_df |> 
  filter(species_code == "WT")

length(unique(recorders$observer))
# 631

# Number of unique recorders in Manchester
MancRecorders <-recorders |> 
  filter(BBS_region == "MANCX")
# Looking at the gps for these records, only north and east Manchester are represented.

length(unique(MancRecorders$observer))
# 25

# Number of recorders in Manchester per year
MancRecorders |> 
  group_by(year) |> 
  summarise(total = n_distinct(observer)) |> 
  View()


# For each year, visit, and unique square, find high count of WT
Highcounts<- checklists |> 
  select(Year, visit, square, count) |> 
  group_by(Year, visit, square) |> 
  summarise(highest_count = max(count, na.rm = TRUE))
  
# Find total number of WT per year and visit
WT<-Highcounts |> 
  group_by(Year, visit) |> 
  summarise(total = sum(highest_count, na.rm = TRUE))
# Find total of all counts
sum(WT$total)
# 2035 WT recorded across 30 years.
# The BTO study found that there were an estimate of 5,700 breeding pairs, but I'm not sure how they got that figure.





# Visualise the data

# Plot the WT data to see highcounts for early/late visits across all years
 WT |> 
   ggplot(mapping = aes(x=Year, y=total, fill = visit))+
   geom_bar(stat = "identity", position = "dodge")+
   labs(title = "Willow Tit high counts per year",
        y = "high count")+
   theme_minimal()

 
 # Which months were WT reported?
 
sort(unique(checklists$Month))
# Only the months of March, April, May, June, July, August are represented.


 
 # Plot the highcounts by month for a specific year.

Y2023<- checklists |> 
  filter(Year == 2023) |> 
  select(Month, Daym, visit, square, count) |> 
  group_by(Month, visit, square) |> 
  summarise(highest_count = max(count, na.rm = TRUE))
  
Y2023<-Y2023 |> 
  group_by(Month, visit) |> 
  summarise(total = sum(highest_count, na.rm = TRUE))

Y2023 |> 
ggplot(mapping = aes(x=Month, y=total, fill = visit))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()



# Plot counts across a single year
checklists |>  
  filter(Year == 2003) |> 
  ggplot(mapping = aes(x= Dayc, y= count))+
  geom_point()+
  xlim(1,366)    #set the x-axis range. To set the y-axis range, use ylim()


# Plot counts across the year for each year
checklists |> 
  ggplot(mapping =  aes(x = Dayc, y = count)) +
  geom_point() +
  facet_wrap(~ Year) +
  labs(title = "Data Visualized by Year",
       x = "X-axis label",
       y = "Y-axis label") +
  theme_bw()


# Population changes: plot highcounts on point graph for each year

WT |> 
  ggplot(mapping = aes(x=Year, y=total, colour = visit))+
  geom_line()+
  geom_point(position = position_dodge(width = 0.1)) + 
  labs(title = "High counts of Willow Tits", x = "year", y = "count", color = "Visit type")+
  scale_color_hue(labels = c("Early visit", "Late visit"))+
  theme(plot.title = element_text(hjust = 0.5))


# Population changes with regression line
WT |> 
  ggplot(mapping = aes(x=Year, y=total, colour = visit))+
  geom_point(position = position_dodge(width = 0.1)) + 
  geom_smooth(method = "lm", se = FALSE)+  # Add the regression line
  labs(title = "High counts of Willow Tits", x = "year", y = "count", color = "Visit type")+
  scale_color_hue(labels = c("Early visit", "Late visit"))+
  theme(plot.title = element_text(hjust = 0.5))


# Plot number of unique recorders per year

observers<- willowtit |> 
  group_by(Year) |> 
  summarise(recorders = n_distinct(observer))

observers |> 
ggplot(mapping = aes(x=Year, y=recorders))+
  geom_line()+
  geom_point()+
  labs(title = "Total number of observers")+
  theme()



# The highcounts and number of recorders looks similar. I would like to test to see if
# there is a correlation between the observations and the number of recorders.
# i.e. I want to check if two variables are related = correlation test

# First, visualise both dataframes on the same plot
ggplot() +
  geom_point(data=WT, mapping = aes(x = Year, y = total, colour = visit)) +
  geom_line(data = WT, mapping = aes(x = Year, y = total, colour = visit))+
  geom_point(data=observers, mapping = aes(x = Year, y = recorders))+
  geom_line(data=observers, mapping = aes(x = Year, y = recorders))+
  labs(title = "Highcounts and number of observers")
  

# To perform a correlation test for highcounts and observers, join the two dataframes
test <- inner_join(WT, observers, by = "Year")

# cor test for highcounts and sites
test_sites <- inner_join(WT, u_sites, by = "Year")

# Calculate the correlation coefficient
  # My data do not follow normal distribution so I want to use a non-parametric test
# Choose test: 
        # Use Pearson’s correlation coefficient when both variables are numeric and normally distributed; measures linear relationships.
        # Use Spearman’s or Kendall’s correlation when data are not normally distributed or ordinal; measures monotonic relationships (general increasing/decreasing trend).

# Highcounts and observers
cor(test$total, test$recorders, method = "spearman") #Comparing total WT per year with total observers
# 0.9111942
# this indicates a very strong positive monotonic relationship.

cor(test$total, test$recorders, method = "kendall")
# 0.7600216
# this indicates a strong positive association between the two variables.


# Highcounts and sites
cor(test_sites$total, test_sites$sites, method = "kendall")
# 0.7690367



# Correlation test to get significance and confidence intervals: Spearman test
cor.test(test$total, test$recorders, method = "spearman", exact = FALSE) # need to use exact=FALSE because data have tied ranks

# Spearman's rank correlation rho
  # data:  test$total and test$recorders
  # S = 3196.1, p-value < 2.2e-16
  # alternative hypothesis: true rho is not equal to 0

# Correlation test to get significance and confidence intervals: Kendall test
cor.test(test$total, test$recorders, method = "kendall")

#Kendall's rank correlation tau
  # data:  test$total and test$recorders
  # z = 8.3821, p-value < 2.2e-16
  # alternative hypothesis: true tau is not equal to 0

# If the p-value is less than 0.05, the correlation is considered statistically significant
# Both tests show that the p-value is 0.00000000000000022 which is significantly significant
# and means I can reject the null hypothesis.

# Corr test Highcounts and sites
cor.test(test_sites$total, test_sites$sites, method = "kendall")
#Kendall's rank correlation tau
   #data:  test_sites$total and test_sites$sites
   #z = 8.4728, p-value < 2.2e-16
   #alternative hypothesis: true tau is not equal to 0
   #sample estimates:
     #tau 
   #0.7690367 




  ## More explanation
    # If your data has many tied ranks or small sample size, Kendall’s tau is often preferred because it provides a more accurate and robust measure of association.

    # If your data is larger and you want a quick, rank-based correlation, Spearman’s rho is commonly used.

    # Both measure monotonic relationships, so they often agree on the direction and general strength of association.
    # A tie is when two or more variables have the same value. They need to be ranked in order, but with the same value
    # they cannot be ranked. So the average of the ranks is given for those values.


  ## Discussion
    # A Spearman’s rank correlation was conducted to assess the relationship between total and recorders. 
    # There was a strong, positive correlation, which was statistically significant,
    # ρ=0.91, and p<0.001
    # Similarly, a Kendall’s tau correlation confirmed this finding with a strong positive association, 
    # τ=0.76, and p<0.001.
    # These results indicate that as the values in total increase, the values in recorders tend to increase as well, demonstrating a strong monotonic relationship.
    # I chose a Kendall test because there are many ties and the sample size is small.



# Do the same with number of sites surveyed
survey<- willowtit |> 
  group_by(Year) |> 
  summarise(sites = n_distinct(square))


survey |> 
  ggplot(mapping = aes(x=Year, y=sites))+
  geom_line()+
  geom_point()+
  labs(title = "Total number of sites surveyed")
  theme()


ggplot() +
  #geom_point(data=WT, mapping = aes(x = Year, y = total, colour = visit)) +
  #geom_line(data = WT, mapping = aes(x = Year, y = total, colour = visit))+
  geom_point(data=observers, mapping = aes(x = Year, y = recorders, colour = "blue"))+
  geom_line(data=observers, mapping = aes(x = Year, y = recorders, colour = "blue"))+
  geom_point(data=survey, mapping = aes(x = Year, y = sites, colour= "red"))+
  geom_line(data=survey, mapping = aes(x = Year, y = sites, colour = "red"))+
  scale_color_hue(labels = c("recorders", "sites"))+
  labs(title = "Recorders and Sites Surveyed",
       y ="total")


# This looks like it correlates enough not to run any tests right now.


# Now I am curious to know how we are certain the population of WTs has declined.
# If WT numbers correlate to number of recorders and sites surveyed, how do we really know WT are declining?
# I will first look to see if correlation is similar in ebird data.


# Try the same plotting for ebird data
eHighcounts<- ebird |> 
  select(year, locality_id, observation_count) |> 
  group_by(year, locality_id) |> 
  summarise(highest_count = max(observation_count, na.rm = TRUE))

# Find total number of WT per year and visit
WTeb<-eHighcounts |> 
  group_by(year) |> 
  summarise(total = sum(highest_count, na.rm = TRUE))
# Find total of all counts
sum(WTeb$total)

# plot
WTeb |> 
  ggplot(mapping = aes(x=year, y=total))+
  geom_line()+
  geom_bar(position = position_dodge(width = 0.1)) + 
  labs(title = "High counts of Willow Tits", x = "year", y = "total")+
  theme(plot.title = element_text(hjust = 0.5))

WTeb |> 
  ggplot(mapping = aes(x=year, y=total))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "eBird Willow Tit high counts per year",
       y = "high count")+
  theme_minimal()


# This shows an entirely different picture, but it could be explained by the fact
# that the number of eBird users increases as does the data points.

# Can plot number of sites and number of observers:

#observers
eobservers<- ebird |> 
  group_by(year) |> 
  summarise(ebirders = n_distinct(observer_id))

eobservers |> 
  ggplot(mapping = aes(x=year, y=ebirders))+
  geom_line()+
  geom_point()+
  theme()

# sites
esurvey<- ebird |> 
  group_by(year) |> 
  summarise(hotspots = n_distinct(locality_id))

esurvey |> 
  ggplot(mapping = aes(x=year, y=hotspots))+
  geom_line()+
  geom_point()+
  theme()

esurvey |> 
  ggplot(mapping = aes(x=year, y = hotspots))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "eBird sites per year")+
  theme_minimal()


# Plot it all
ggplot() +
  geom_point(data=WTeb, mapping = aes(x = year, y = total, colour = "WTeb")) +
  geom_line(data = WTeb, mapping = aes(x = year, y = total, colour = "WTeb"))+
  geom_point(data=eobservers, mapping = aes(x = year, y = ebirders, colour = "eobservers"))+
  geom_line(data=eobservers, mapping = aes(x = year, y = ebirders, colour = "eobservers"))+
  geom_point(data=esurvey, mapping = aes(x =year, y = hotspots, colour = "esurvey"))+
  geom_line(data=esurvey, mapping = aes(x =year, y =hotspots, colour = "esurvey"))+
  scale_color_manual(name = "eBird Willow Tit trends", values = c("WTeb" = "forestgreen", "eobservers" = "pink", "esurvey" = "darkblue"),
  labels = c("WTeb" = "WT highcounts", "eobservers" = "total eBird Observers","esurvey" = "total hotspots surveyed")) +
  theme_minimal()

# BBS data, only used early visit data

early <-WT |> 
  filter(visit == "E")

ggplot() +
  geom_point(data=early, mapping = aes(x = Year, y = total, colour = "visit")) +
  geom_line(data = early, mapping = aes(x = Year, y = total, colour = "visit"))+
  geom_point(data = observers, mapping = aes(x = Year, y = recorders, colour = "recorders"))+
  geom_line(data=observers, mapping = aes(x=Year, y=recorders, colour = "recorders" ))+
  geom_point(data = survey, mapping= aes(x= Year, y = sites, colour = "sites"))+
  geom_line(data = survey, mapping = aes(x=Year, y=sites, colour = "sites"))+
  scale_color_manual(name = "BBS Willow Tit trends", values = c("visit"= "forestgreen", "recorders" = "pink", "sites" = "darkblue"),
  labels = c("visit" = "early visit", "recorders" = "total recorders","sites" = "total sites surveyed")) +
  theme_minimal()




# Perform a correlation test on the ebird data
etest <- inner_join(WTeb, eobservers, by = "year")

# Correlation coefficient
cor(etest$total, etest$ebirders, method = "kendall")
# 0.8863636

# Correlation test Kendall to get significance
cor.test(etest$total, etest$ebirders, method = "kendall", exact=FALSE) # use exact = FALSE if warning occurs
  #Kendall's rank correlation tau
    # data:  etest$total and etest$ebirders
    # z = 3.5162, p-value = 0.0004378
    # alternative hypothesis: true tau is not equal to 0
    # sample estimates:
      # tau 
      # 0.8863636 
# to ignore the warning
result <- cor.test(etest$total, etest$ebirders, method = "kendall")
print(result$estimate)  # Tau-b correlation coefficient
print(result$p.value)
#  0.000437781 # similar to the above where I used exact=FALSE in the code
# The p value is less than 0.05 so it is statistically significant


# However this significance and the BBS significance occur with opposite trends
# so this tells me that the number of observations increases/decreases with the number of observers
# NOT that the number of observations shows an increase/decrease. 
# Therefore, I have a bias.








# Map the data
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)



uk_sf <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")


ggplot(data = uk_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = checklists, aes(x = ETRS89Long, y = ETRS89Lat, colour = count), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows all the years separately
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "BTO Willow Tit recordings 1994-2023", x = "Longitude", y = "Latitude")


#Only plot the 2014-2023 data
checklists_new <-checklists |> 
  filter(Year %in% c(2014:2023))
# 271 observations

ggplot(data = uk_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = checklists_new, aes(x = ETRS89Long, y = ETRS89Lat, colour = count), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows all the years separately
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "BTO Willow Tit recordings 2014-2023", x = "Longitude", y = "Latitude")




# To create a map for a specific year

Y2022<- checklists |> 
  filter(Year =="2022")

ggplot(data = uk_sf)+
  geom_sf(fill = "gray90", color = "black")+
  geom_point(data = Y2022, aes(x = ETRS89Long, y = ETRS89Lat), color = "purple", size = 0.5)+
  theme_minimal()+
  labs(title = "Test")



# Map the BBS data for Manchester
# rnaturalearth does not go down to city-level. Use the Open Street Map data package
# Dataframe: MANCX = the GPS points assigned by the BTO as covering "Manchester" which actually go into Lancashire and Cheshire.


install.packages("osmdata")
library(osmdata)
library(sf)
library(ggplot2)

# Get Manchester boundary from OpenStreetMap
manchester <- opq("Greater Manchester, UK") |>  # Greater Manchester covers more of the BBS points than Manchester.
  add_osm_feature(key = "boundary", value = "administrative") |> 
  add_osm_feature(key = "admin_level", value = "8") |>  
  osmdata_sf()

  # OSM reference of admin levels
    # admin_level=2: Country (e.g., United Kingdom)
    # admin_level=4: Constituent country (e.g., England, Scotland)
    # admin_level=6: Metropolitan counties, non-metropolitan counties, or unitary authorities
    # admin_level=8: Districts, boroughs, cities, or civil parishes (often used for city boundaries)


# Extract the polygon
manchester_poly <- manchester$osm_multipolygons

# plot
ggplot(data = manchester_poly) +
  geom_sf(fill = NA, color = "black") +
  # geom_sf(data = riverbanks$osm_multipolygons, fill = "green", color = NA, alpha = 0.5) +
  geom_sf(data = rivers$osm_lines, color = "lightblue", size = 0.2) + 
  geom_point(data = MANCX, aes(x = ETRS89Long, y = ETRS89Lat, colour = count), size = 2) + # Order of objects here affects which ones are "on top" of others. Put points last so they map over the lines.
  geom_sf(data = town_points, color = "red", size = 0.5, shape = 21, fill = "red") +  # Town points
  geom_text(data = town_points, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = name), size = 3, nudge_y = 0.002)+
  labs(title = "BTO Willow Tit recordings Manchester, 1994-2023", x = "Longitude", y = "Latitude")
  theme_minimal()


# Include landmarks like rivers

manchester_bbox <- getbb("Greater Manchester, UK") # "Greater Manchester" adds a lot more rivers

rivers <- opq(bbox = manchester_bbox) |> 
  add_osm_feature(key = "waterway", value = c("river", "stream", "canal")) |> 
  osmdata_sf()


 # riverbanks <- opq(bbox = manchester_bbox) |> 
  # add_osm_feature(key = "natural", value = "water") |> 
  # add_osm_feature(key = "water", value = "river") |> 
  # osmdata_sf()


towns <- opq(bbox = manchester_bbox) |> 
  add_osm_feature(key = "place", value = c("city", "town")) |> 
  osmdata_sf()

town_points <- towns$osm_points

town_points<- town_points |> 
  filter(name %in% c("Bury", "Bolton", "Wigan", "Urmston", "Stalybridge", "Marple", "Altrincham", "Warrington", "Chorley", "Widnes"))





# Comparison with ebird data.

# First filter by manchester coordinates
ebird<- ebird |> 
  filter(between(latitude, 53.20000, 53.80000), # It looks like for coordinates, the values must be in numerical order, i.e. small to large.
    between(longitude, -3.0000000, -2.0000000)
  )

# remove observation counts of 0
ebird<- ebird |> 
  filter(observation_count != 0)

# map
ggplot(data = manchester_poly) +
  geom_sf(fill = NA, color = "black") +
  # geom_sf(data = riverbanks$osm_multipolygons, fill = "green", color = NA, alpha = 0.5) +
  geom_sf(data = rivers$osm_lines, color = "lightblue", size = 0.2) + 
  geom_point(data = ebird, aes(x = longitude, y = latitude, colour = observation_count), size = 2) + # Order of objects here affects which ones are "on top" of others. Put points last so they map over the lines.
  geom_sf(data = town_points, color = "red", size = 0.5, shape = 21, fill = "red") +  # Town points
  geom_text(data = town_points, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = name), size = 3, nudge_y = 0.002)+
  labs(title = "eBird Willow Tit recordings Manchester, 2014-2023", x = "Longitude", y = "Latitude")
  theme_minimal()






# Adding features to the maps that may show correlations with the data points

# Map reservoirs
res<- opq(bbox = manchester_bbox) |> 
  add_osm_feature(key = "natural", value = "water") |> 
  add_osm_feature(key = "water", value = "reservoir")

reservoir<- osmdata_sf(res) # create an object to hold the points.

ggplot(data = manchester_poly)+
  geom_sf(fill = NA, color = "black")+
  geom_sf(data = reservoir$osm_polygons, fill = "blue", color = "blue",alpha = 1)+  # the polygons value was found by looking at the reservoir object to see what values were held
  geom_sf(data = reservoir$osm_multipolygons, fill = "green", color = "green", alpha = 1)+
  geom_sf(data = town_points, color = "red", size = 0.5, shape = 21, fill = "red") +  # Town points
  geom_text(data = town_points, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = name), size = 3, nudge_y = 0.002)
  

# Map sewage works
sew<- opq(bbox = manchester_bbox) |> 
  add_osm_feature(key = "man_made", value = "wastewater_plant")

sewage_works<- osmdata_sf(sew)

ggplot(data = manchester_poly)+
  geom_sf(fill = NA, color = "black")+
  geom_sf(data = sewage_works$osm_polygons, fill = "blue", color = "blue",alpha = 1)+
  geom_sf(data = town_points, color = "red", size = 0.5, shape = 21, fill = "red")+
  geom_text(data = town_points, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = name), size = 3, nudge_y = 0.002)


# Putting all the water features together
ggplot(data = manchester_poly)+
  geom_sf(fill = NA, color = "black")+
  geom_sf(data = rivers$osm_lines, color = "lightblue", size = 0.2) +
  geom_sf(data = reservoir$osm_polygons, fill = "blue", color = "blue",alpha = 1)+  # the polygons value was found by looking at the reservoir object to see what values were held
  geom_sf(data = reservoir$osm_multipolygons, fill = "blue", color = "blue", alpha = 1)+
  geom_sf(data = sewage_works$osm_polygons, fill = "pink", color = "pink",alpha = 1)+
  geom_sf(data = town_points, color = "red", size = 0.5, shape = 21, fill = "red") +  # Town points
  geom_text(data = town_points, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = name), size = 3, nudge_y = 0.002)+
  labs(title = "Water features of surrounding Manchester area", x = "Longitude", y = "Latitude")
  #scale_color_manual(values = c("lightblue", "blue", "pink"), labels = c("rivers", "reservoirs", "sewage works")) 
