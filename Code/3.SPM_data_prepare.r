setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")

library(ggplot2)
library(stringr)
library(dplyr)

fish_df = read.csv("Data/Fish/all_input.csv")
xref_df = read.csv("Data/midwest_lake_lat_long_xref.csv")
temp_df = read.csv("Data/Optimal_temperature_index_1_1000.csv")
temp_df_1 = read.csv("Data/Optimal_temperature_index_1001_2000.csv")
temp_df = rbind(temp_df,temp_df_1)

# only consider BC WE YP and NP for now before we determine the alternative optimal thermal range for other species 
temp_df = temp_df[temp_df$species%in%c("black crappie","walleye","yellow perch","northern pike"),]

# merge
xref_df$casc.lake.id = as.integer(xref_df$casc.lake.id)
fish_df = left_join(fish_df,xref_df[,c("casc.lake.id","nhdhr.id")],by=c("wbic"="casc.lake.id"))

## extract site_id
temp_df$site_id_num = str_extract(temp_df$site_id, "(?<=GLM_nhdhr_)(.*)(?=_NLDAS.feather)")
fish_df$site_id_num = sub(".*nhdhr_", "", fish_df$nhdhr.id)

## change the name of species
## use recode to replace values in species column
fish_df$species <- recode(fish_df$species, 
                          'NP' = 'northern pike',
                          'WE' = 'walleye',
                          'BC' = 'black crappie',
                          'YP' = 'yellow perch')

## second merge 
fish_df = left_join (fish_df,temp_df,by=c("site_id_num","year","species"))
fish_df = fish_df[complete.cases(fish_df$days_optimal_temp),]

# save the data
write.csv(fish_df,"Data/SPM_input.csv",row.names = FALSE)
