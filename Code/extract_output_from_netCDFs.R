##################################################################
# Author: Hayley Corson-Dosch                                    #
# Email: hcorson-dosch@usgs.gov                                  #
# Content: workflow for extracting predictions from netCDF files #
##################################################################
setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")
library(data.table)
library(RNetCDF)
library(ncmeta) # need version 0.3.5 or higher - devtools::install_github("https://github.com/hypertidy/ncmeta.git")
library(tidyverse)
library(ncdfgeom) # need version >= v1.1.2

# Before you can run this script, make sure that you have also downloaded
# the `netCDF_extract_utils.R` script from ScienceBase 
# It contains all the functions required for this code to work. Edit the filepath
# below depending on where you saved it relative to this file.
source('Code/netCDF_extract_utils.R') 

##### Read in temperature and ice predictions from a netCDF for a set of lakes #####

# Update the filepath for the NetCDF file you are extracting data from. This example 
# script assumes that you have already downloaded it from ScienceBase (item 6206d3c2d34ec05caca53071)
nc_file <- 'Data/Environment/compress_tmp/lake_temp_preds_GLM_NLDAS_WI.nc'

# Read in information about netCDF (variables, dates, etc.)
# read_data set to FALSE b/c netCDF is too large for all data to be read in at once
# Warning 'no altitude coordinate found' is expected
nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE)

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

