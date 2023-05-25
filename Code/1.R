##################################################################     
# Content: calculating the No. of days with optimal temperature  #
##################################################################
setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")
library(data.table)
library(RNetCDF)
library(ncmeta) 
library(tidyverse)
library(ncdfgeom) 
library(sf)
library(rgdal)
library(stringr)
library(lwgeom)

# Before you can run this script, make sure that you have also downloaded
# the `netCDF_extract_utils.R` script from ScienceBase 
# It contains all the functions required for this code to work. Edit the filepath
# below depending on where you saved it relative to this file.
source('Code/netCDF_extract_utils.R')

##### read the fish related data: 1.spatial range; 2.thermal metrics; 3. HUC8 shape file #####
# 1. spatial range
walleye_spatial_range = read.csv("Data/Fish/Fish_range/Sander_vitreus_range_20170531.csv")
walleye_spatial_range$HUC8 = str_pad(walleye_spatial_range$HUC8, width = 8, side = "left", pad = "0") # fix the HUC8 when some of the code only have 7 numbers
largemouth_bass_spatial_range = read.csv("Data/Fish/Fish_range/Micropterus_salmoides_range_20170531.csv")
largemouth_bass_spatial_range$HUC8 = str_pad(largemouth_bass_spatial_range$HUC8, width = 8, side = "left", pad = "0") # fix the HUC8 when some of the code only have 7 numbers

# 2. thermal metrics
fish_df = read.csv("Data/Fish/spp_thermal_metrics.csv")

# 3. HUC8 shape file
huc8_shapefile <- st_read("Data/Environment/HUC8/HUC8_US.shp")

##### check if the location is within the fish spatial range #####
# Change the file to read data for different states !
nc_file <- 'Data/Environment/compress_tmp/lake_temp_preds_GLM_NLDAS_WI.nc'

# Read in information about netCDF (variables, dates, etc.)
# Warning 'no altitude coordinate found' is expected
nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE)

# data frame to contain the infor of fish presence or absence for the given lake_ids
fish_present_lake_id = data.frame(lake_id = nc_info$timeseries_id, walleye_present=1, largemouth_bass_present=1)

# The groups of HUC8 codes of fish range of different species
huc_group_walleye <- walleye_spatial_range$HUC8
huc_group_largemouth_bass = largemouth_bass_spatial_range$HUC8

# Filter the shapefile to include only the HUC8 polygons in the group
huc8_group_shapefile_walleye <- huc8_shapefile[huc8_shapefile$HUC8 %in% huc_group_walleye, ]
huc8_group_shapefile_largemouth_bass <- huc8_shapefile[huc8_shapefile$HUC8 %in% huc_group_largemouth_bass, ]

# fix the when you are working with a geometry that has some invalid features or geometries. In this case, it indicates that one of the features has a loop (a ring in the polygon) with a duplicate vertex in two different edges, which is not allowed in a valid spherical geometry.
huc8_group_shapefile_walleye <- st_make_valid(huc8_group_shapefile_walleye)
huc8_group_shapefile_largemouth_bass <- st_make_valid(huc8_group_shapefile_largemouth_bass)

for (i in 1:length(nc_info$timeseries_id)){
  
  lat <- nc_info$lats[i]
  long <- nc_info$lons[i]
  
  # Create a point, set CRS, and convert to sf object
  # Create a point
  point <- st_point(c(long, lat))
  
  # Transform the point to the same CRS as the HUC8 shapefile
  point <- st_transform(st_sfc(point, crs = 4326), crs = st_crs(huc8_shapefile))
  
  # Check if the point is within any HUC polygon in the group 
  within_huc8 <- st_within(point, huc8_group_shapefile_walleye)
  fish_present_lake_id$walleye_present = length(within_huc8[[1]])
  within_huc8 <- st_within(point, huc8_group_shapefile_largemouth_bass)
  fish_present_lake_id$largemouth_bass_present = length(within_huc8[[1]])
  
  print(i)
}
write.csv(fish_present_lake_id,"fish_present_lake.csv")

# Define lake sites of interest
# Can list directly (e.g., c("nhdhr_109943476", "nhdhr_109943604")), or pull from `nc_info$timeseries_id` vector
lake_sites <- c(nc_info$timeseries_id[1:10])

# Pull temperature predictions (for all dates and all depths) for those lakes.
# Can be slow to run for more than a moderate # of sites at once, if
# pulling from a large netCDF file, or if pulling data for deep lakes
# Depth units = meters, temperature units = degrees Celsius
# can specify wide (long_format = FALSE) or long format (long_format = TRUE)
# if wide format, columns are named {site_id}_{depth}
temp_data_df <- pull_data_for_sites(nc_file, nc_info, var = 'temp', sites = lake_sites, long_format = TRUE)

# Convert data frames to data tables just to make the code more efficient
temp_data <- as.data.table(temp_data_df)

# Filter the data for temperature smaller than 24.4
filtered_data_smaller <- temp_data %>% filter(temperature < 22.5)
filtered_data_smaller <- unique(filtered_data_smaller[,-c("depth","temperature")])

# Filter the data for temperature larger than 24.4
filtered_data_larger <- temp_data %>% filter(temperature >= 22.5)
filtered_data_larger <- unique(filtered_data_larger[,-c("depth","temperature")])

filtered_data_smaller$year <- as.integer(format(as.Date(filtered_data_smaller$time, format = "%Y-%m-%d"), "%Y"))
filtered_data_larger$year <- as.integer(format(as.Date(filtered_data_larger$time, format = "%Y-%m-%d"), "%Y"))

# Perform a non-equi join on 'year', 'site_id', and 'time'
overlapping_data <- filtered_data_smaller[filtered_data_larger, 
                                                on = .(year, site_id, time),
                                                nomatch = 0L]

# Group by 'year' and 'site_id' and count the overlapping 'time' values
result_dt <- overlapping_data[, .(overlap_count = .N), by = .(year, site_id)]

# Print the result
print(result_dt)

