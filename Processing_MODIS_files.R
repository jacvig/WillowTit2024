# Processing MODIS files

  ## For identifying, downloading, and processing MODIS data for use in species distribution models.
  ## This code is modified from Spacial Data Science https://rspatial.org/modis/2-download.html on downloading MODIS data.
  ## NB: The MODIS package has changed to version 6.1 which comes through in the tiles.

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

# plot to see  the extent 
plot(uk, col="light gray")
lines(aoi,col="red", lwd=2)


# Find MODIS tiles for the SpatExtent
mf<-luna::getNASA(product, start, end, aoi = aoi, version = "061", download =FALSE) 
mf 
    # NB. There are 60 MODIS tiles: tile 17.2; 17.3; 17.4; 18.2; 18.3; 18.4 for each of the 10 years.



# To use the MODIS tiles, they need to be downloaded, processed, and then cropped.

getwd()
setwd("filepath/data")

# Create directory for files to download into
data<-file.path(dirname("data"))
dir.create(data, showWarnings = FALSE)

# Download MODIS files. 
# NB. You will need to create an account on EOSDIS https://urs.earthdata.nasa.gov/ to be able to provide your username and password.
# Depending on how many files need downloading, this may take some time.
mf<-luna::getNASA(product, start, end, aoi=aoi, version = "061", download=TRUE,
                   path = "filepath/data", username="username", password = "password")


# Explore the files
datadir<- file.path(dirname("filepath/data"))
mf<- file.path("filepath/data", "MCD12Q1.A2014001.h17v02.061.2022165101511.hdf")
r<-rast(mf[1])
r
dim(r)
res(r)
names(r) # To find the bands or layers

# I will use the Maryland Global Land Cover Classification system. This is landcover scheme LC2 which is the second product layer. See:https://lpdaac.usgs.gov/products/mcd12q1v061/
# The landcover codes for the MGLCC can be found at https://daac.ornl.gov/ISLSCP_II/guides/umd_landcover_xdeg.html 


# Subset the raster so only a single layer is plotted. I have chosen LC2 Maryland landcover classification
r<-r[[2]]
plot(r)


# Define the input directory and file pattern
input_dir <- "filepath/data"
file_pattern <- "MCD12Q1.A\\d{4}001.h\\d{2}v\\d{2}.061.\\d{13}.hdf$"

# List all HDF files
hdf_files <- list.files(input_dir, pattern = file_pattern, full.names = TRUE)

# Function to process a single HDF file
process_hdf <- function(file) {
  r <- rast(file)
  layer <- r[[2]]  # Assuming you always want the second layer
  year <- as.numeric(substr(basename(file), 10, 13))  # Extract year from filename
  names(layer) <- paste0("Year_", year)
  return(layer)
}

# Process all files
processed_rasters <- lapply(hdf_files, process_hdf)

# Merge tiles for each year
merged_rasters <- lapply(2014:2023, function(year) {
  year_rasters <- processed_rasters[sapply(processed_rasters, function(r) grepl(paste0("Year_", year), names(r)))]
  if (length(year_rasters) > 0) {
    merged <- do.call(merge, year_rasters)
    names(merged) <- paste0("Year_", year)
    return(merged)
  } else {
    return(NULL)
  }
})

# Remove any NULL elements (years without data)
merged_rasters <- merged_rasters[!sapply(merged_rasters, is.null)]

# If you want to stack all years into a single SpatRaster
all_years_stack <- rast(merged_rasters)

names(all_years_stack)

# Set layer names (optional)
names(all_years_stack) <- paste0("Year_", 2014:2023)

# #Write as multi-band GeoTIFF
writeRaster(all_years_stack, filename = "modis_mc12q1_2014-2023.tif", overwrite = TRUE)

# explore
print(all_years_stack)
summary(all_years_stack)

for (i in 1:nlyr(all_years_stack)) {
  plot(all_years_stack[[i]], main = names(all_years_stack)[i])
}

terra::plot(all_years_stack)  # Adjust nc (number of columns) as needed


# Assign new names if necessary. # The terra package seems to do this automatically.
# names(landcover) <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
# names(landcover)


# Now crop the SpatRaster so it is not so large.
# See https://rspatial.org/modis/5-processing.html
# To work with the SpatRaster in r, read it back in. May need to change the layer names (see above) in order to plot it i.e. call a year

library(geodata)

uk <- gadm(country="United Kingdom", level=0, path=".") # Use level=1 for more specific areas
uk
pol <- uk[uk$GID_0 == "GBR", ]
pol

getwd()
datadir <- file.path(dirname(tempdir()), "filepath/data")
mf <- file.path(datadir, "modis_mc12q1_2014-2023.tif")
rmask <- rast("data/modis_mc12q1_2014-2023.tif")
prj <- crs(rmask)
prj
poly <- project(pol, prj)
rcrop <- crop(rmask, poly)


plot(rcrop)

# plot for a specific year
plot(as.factor(rcrop[["Year_2023"]]),
     main = "MODIS Landcover 2023",
     axes = FALSE)

writeRaster(rcrop, filename = "modis_mc12q1_2014-2023_crop.tif", overwrite = TRUE)
