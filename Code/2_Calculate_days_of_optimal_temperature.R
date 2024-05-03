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
library(dplyr)
library(lubridate)
library(purrr)
library(dplyr)
library(tools)

# Before you can run this script, make sure that you have also downloaded
# the `netCDF_extract_utils.R` script from ScienceBase 
# It contains all the functions required for this code to work. Edit the filepath
# below depending on where you saved it relative to this file.
source('Code/netCDF_extract_utils.R')

#### 1. read the fish FTP ####
fish_ftp = read.csv('Data/spp_thermal_metrics.csv')
fish_ftp = fish_ftp[!is.na(fish_ftp$ftp),]

# check any rows with the same latin name but different ftp (because of different life stages)
# Add a row_number for later use
fish_ftp <- fish_ftp %>% mutate(row = row_number())

# Check if the same latin.name have different ftp
result <- fish_ftp %>%
  group_by(latin.name) %>%
  filter(n_distinct(ftp) > 1) %>%
  arrange(latin.name, row)

# print out the result
length(result$order) # it is empty

# Keep only unique latin.name
fish_ftp <- fish_ftp %>%
  distinct(latin.name, .keep_all = TRUE)

# Replace underscore with space and convert to title case
fish_ftp$latin.name <- toTitleCase(fish_ftp$latin.name)
fish_ftp$latin.name <- gsub("_", " ", fish_ftp$latin.name)

#### 2. read the fish presence in lakes data ####
fish_present_lake = read.csv("Output/Fish_present_lake.csv")
fish_present_lake = fish_present_lake[fish_present_lake$presence==1,]

#### 3. read temperature profile data and calcualte the No of days in each year with optimal temperature####
path <- "Data/Environment/compress_tmp/"
files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)

# creat the file for the data
output_file <- "Output/Days_optimal_temp_Results_monthly_1"

# Loop over each state lake nc file and each species
for(i in 1) {
  
  nc_file <- files[i]
  
  # Read in information about netCDF (variables, dates, etc.)
  nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE)
  
  # Define lake sites of interest
  lake_sites <- c(nc_info$timeseries_id)[5:10] # change to the lake site 
  lake_sites <-  "nhdhr_154112267"### ????
  
  # Pull temperature predictions (for all dates and all depths) for those lakes.
  temp_data <- pull_data_for_sites(nc_file, nc_info, var = 'temp', sites = lake_sites, long_format = TRUE)
  temp_data = temp_data[complete.cases(temp_data),]
  
  for (o in 1:length(unique(fish_present_lake$species))){
    optimal_temp = fish_ftp[fish_ftp$latin.name==unique(fish_present_lake$species)[o],]$ftp
    
    # only use the overlap between lake and fish range
    species_present_lake = fish_present_lake[fish_present_lake$species==unique(fish_present_lake$species)[o],]
    species_temp_data = temp_data[temp_data$site_id%in%species_present_lake$lake_id,]
    
    result <- species_temp_data %>%
      mutate(date = as.Date(time), year = year(time)) %>%
      group_by(year, site_id, date) %>%
      summarise(
        min_temp = min(temperature, na.rm = TRUE),
        max_temp = max(temperature, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(min_temp < optimal_temp, max_temp > optimal_temp) %>%
      group_by(year, site_id) %>%
      summarise(days_with_optimal_temp = n(), .groups = "drop")
    result$species = unique(fish_present_lake$species)[o]
    
    # Append the results to the CSV file
    if(!file.exists(output_file)) {
      write.csv(result, file = output_file, row.names = FALSE)
    } else {
      write.table(result, file = output_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
      print(c(i,o))
    }
  }
}


