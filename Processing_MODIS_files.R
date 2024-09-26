# Processing MODIS files

# For identifying, downloading, and processing MODIS data for use in species distribution models.
## This code is modified from Spacial Data Science https://rspatial.org/modis/2-download.html on downloading MODIS data.
#NB: The MODIS package has changed to version 6.1 which comes through in the tiles.

# Load packages
library(terra)
library(luna)


# I will use the MODIS product MCD12Q1 (Terra+Aqua Land Cover Type Yearly L3 Global 500 m SIN Grid)
# https://lpdaac.usgs.gov/products/mcd12q1v061/

# Otherwise, to find the MODIS product:
prod <- getProducts()
modis <- getProducts("^MOD|^MYD|^MCD")   
modis
product<- "MCD12Q1" 
productInfo(product)

# set dates
start<- "2014-01-01"
end<- "2023-12-31"

# Define the area of interest. Set spatial extent.
uk <-geodata::gadm("GBR", level = 3, path = ".")
uk  # This is a SpatVector. 

# Subset the SpatExtent to the level desired. 
i<-uk$COUNTRY == "United Kingdom" 
aoi<-uk[i,]
# OR specify the SpatExtent by coordinates e.g. 
  # aoi<- c(-8.59142274740035, 1.78488358434294, 49.8790753014284, 60.8424085406236)

# plot to see if the extent
plot(uk, col="light gray")
lines(aoi,col="red", lwd=2)


# Find MODIS tiles for the SpatExtent
mf<-luna::getNASA(product, start, end, aoi = aoi, version = "061", download =FALSE) 
mf 
    # N.B. There are 60 MODIS tiles: tile 17.2; 17.3; 17.4; 18.2; 18.3; 18.4 for each of the 10 years.



# To use the MODIS tiles, they need to be downloaded, processed, and then cropped.


# Create directory for files to download into
data<-file.path(dirname("data"))
dir.create(data, showWarnings = FALSE)

# Download MODIS files. 
# Will need to create a profile on 
# Depending on how many files need downloading, this may take some time.
mf<-luna::getNASA(product, start, end, aoi=aoi, version = "061", download=TRUE,
                   path = "filepath/data", username="username", password = "password")


luna::getNASA
#Explore the files
datadir<- file.path(dirname("filepath/data"))
mf<- file.path("filepath/data", "MCD12Q1.A2014001.h17v02.061.2022165101511.hdf")
r<-rast(mf[1])
r
dim(r)
res(r)

#I want to use the Maryland classification system. This is landcover scheme LC2 which is the second product layer. See:https://lpdaac.usgs.gov/products/mcd12q1v061/

#Subset the raster so only a single layer is plotted. I have chosen LC2 Maryland landcover classification
r<-r[[2]]
plot(r)
