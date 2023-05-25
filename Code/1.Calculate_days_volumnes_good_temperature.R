setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")

library(feather)
library(arrow)
library(lubridate)
library(ncdf4)

# read the file
test = nc_open("Data/Environment/compress_tmp/lake_temp_preds_GLM_NLDAS_MN.nc")
test_1 = (test$var)

# Set the path to the directory containing feather files
path <- "C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3/Data/Environment/WI_ENV_all"

# Get the list of feather files in the directory
feather_files <- list.files(path, pattern = "\\.feather$")[1001:2000]

# Loop through the feather files and calculate the quantities of interests
# time period
time_year = 1980:2021
N_year = length(time_year)

# species-specific preferec temp
species_name = c("walleye","bluegill","smallmouth bass","largemouth bass","black crappie","yellow perch","northern pike") # the order matters 
N_species = length(species_name) # number of speceis
we_temp = c(20.6,23.2)
bg_temp = c(30.5,32.3)
sb_temp = c(30,31.3)
lb_temp = c(29,32)
bc_temp = c(21.7,24.6)
yp_temp = c(20.1,27)
np_temp = c(19,24)

# data frame used to contain the infor
temp_df = data.frame(site_id = rep(NA,length(feather_files)*N_year*N_species)*10)

# Define a custom function to check if any value in a row is within the specified range
# Define a custom function to check if any value in a row is within the specified range
check_values_in_range <- function(row, min_value, max_value) {
  row_without_first <- row[-1] # Remove the first column from the row
  any(row_without_first >= min_value & row_without_first <= max_value & !is.na(row_without_first))
}
# Define a custom function to check to calculate how many cells in which the value is within the specified range in a row
count_values_in_range <- function(row, min_value, max_value) {
  row_without_first <- row[-1] # Remove the first column from the row
  sum(row_without_first >= min_value & row_without_first <= max_value & !is.na(row_without_first))
}

i = 1 
for (file in feather_files) {
  file_path <- file.path(path, file)
  df <- read_feather(file_path)
  df$we_good_temp = df$bg_good_temp = df$sb_good_temp = df$lb_good_temp = df$bc_good_temp = df$yp_good_temp = df$np_good_temp = 0
  
  df$we_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = we_temp[1], max_value = we_temp[2]), 1, df$we_good_temp)
  df$we_good_vol <- apply(df, 1, count_values_in_range, min_value = we_temp[1], max_value = we_temp[2])# walleye
  
  df$bg_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = bg_temp[1], max_value = bg_temp[2]), 1, df$bg_good_temp)
  df$bg_good_vol <- apply(df, 1, count_values_in_range, min_value = bg_temp[1], max_value = bg_temp[2])# bluegill
  
  df$sb_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = sb_temp[1], max_value = sb_temp[2]), 1, df$sb_good_temp)
  df$sb_good_vol <- apply(df, 1, count_values_in_range, min_value = sb_temp[1], max_value = sb_temp[2])# smallmouth bass
  
  df$lb_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = lb_temp[1], max_value = lb_temp[2]), 1, df$lb_good_temp)
  df$lb_good_vol <- apply(df, 1, count_values_in_range, min_value = lb_temp[1], max_value = lb_temp[2])# largemouth bass
  
  df$bc_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = bc_temp[1], max_value = bc_temp[2]), 1, df$bc_good_temp)
  df$bc_good_vol <- apply(df, 1, count_values_in_range, min_value = bc_temp[1], max_value = bc_temp[2])# black crappie
  
  df$yp_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = yp_temp[1], max_value = yp_temp[2]), 1, df$yp_good_temp)
  df$yp_good_vol <- apply(df, 1, count_values_in_range, min_value = yp_temp[1], max_value = yp_temp[2])# yellow perch
  
  df$np_good_temp <- ifelse(apply(df, 1, check_values_in_range, min_value = np_temp[1], max_value = np_temp[2]), 1, df$np_good_temp)
  df$np_good_vol <- apply(df, 1, count_values_in_range, min_value = np_temp[1], max_value = np_temp[2])# northern pike
  
  df$year = year(df$time)
  
  temp_df$site_id[((i-1)*N_year*N_species+1):(i*N_year*N_species)] = rep(file, each=N_year*N_species)
  
  temp_df$number_layers[((i-1)*N_year*N_species+1):(i*N_year*N_species)] = rep(length(df)-3-N_species*2,N_year*N_species)
  
  temp_df$year[((i-1)*N_year*N_species+1):(i*N_year*N_species)] = rep(time_year,N_species)
  
  temp_df$species[((i-1)*N_year*N_species+1):(i*N_year*N_species)] = rep(species_name,each = N_year)
  
  temp_df$days_optimal_temp[((i-1)*N_year*N_species+1):(i*N_year*N_species)] =   c(aggregate(df$we_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$bg_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$sb_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$lb_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$bc_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$yp_good_temp,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$np_good_temp,by=list(df$year),sum)[,2])
  
  temp_df$volume_optimal_temp[((i-1)*N_year*N_species+1):(i*N_year*N_species)] =   c(aggregate(df$we_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$bg_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$sb_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$lb_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$bc_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$yp_good_vol,by=list(df$year),sum)[,2],
                                                                                   aggregate(df$np_good_vol,by=list(df$year),sum)[,2])
    
  i = i+1
  print(i)
  
}

# save the data
temp_df = temp_df[complete.cases(temp_df$site_id),]
write.csv(temp_df,"Data/Optimal_temperature_index_1001_2000.csv",row.names = FALSE)


