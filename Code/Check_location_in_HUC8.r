# Install and load required packages
setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")
library(sf)
library(rgdal)

# Read HUC 8 shapefile
huc8_shapefile <- st_read("Data/Environment/HUC8/HUC8_US.shp")

# Define latitude and longitude
lat <- 35.52845
long <- -78.73272

# Create a point, set CRS, and convert to sf object
point <- st_point(c(long, lat))
point_sfc <- st_sfc(point, crs = st_crs(huc8_shapefile))
point_sf <- st_as_sf(point_sfc)

# Define the group of HUC8 codes you're interested in
huc_group <- c("03030004", "03030005", "03030007")  # Replace these with your HUC8 codes

# Filter the shapefile to include only the HUC8 polygons in the group
huc8_group_shapefile <- huc8_shapefile[huc8_shapefile$HUC8 %in% huc_group, ]

# Find which HUC polygon within the group contains the point
huc_containing_point <- huc8_group_shapefile[st_within(point_sf, huc8_group_shapefile, sparse = FALSE), ]

# Check if the point is within any HUC polygon in the group
is_point_in_huc_group <- nrow(huc_containing_point) > 0

# Display results
print(huc_containing_point)
print(is_point_in_huc_group)
