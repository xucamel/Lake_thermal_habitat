setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")

library(ggplot2)
library(stringr)
library(dplyr)

temp_df = read.csv("Data/Optimal_temperature_index_1_1000.csv")
loc_df = read.csv("Data/midwest_lake_lat_long_xref.csv")

# only consider BC WE YP and NP for now before we determine the alternative optimal thermal range for other species 
temp_df = temp_df[temp_df$species%in%c("black crappie","walleye","yellow perch","northern pike"),]

# the boxplot 
# days_optimal_temp_by_species_and_year
p1 = ggplot(temp_df, aes(x = year, y = days_optimal_temp, group = year)) +
  geom_boxplot(position = "dodge",outlier.shape = NA) +
  facet_wrap(~ species, scales = "free") +
  labs(x = "Species", y = "Days Optimal Temperature",
       title = "Days Optimal Temperature by Species and Year") +
  scale_y_continuous(limits = c(0, 200))
ggsave("Output/Preliminary/days_optimal_temp_by_species_and_year.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# volumes_optimal_temp_by_species_and_year
p2 = ggplot(temp_df, aes(x = year, y = volume_optimal_temp, group = year)) +
  geom_boxplot(position = "dodge",outlier.shape = NA) +
  facet_wrap(~ species, scales = "free") +
  labs(x = "Species", y = "Days Optimal Temperature",
       title = "Volume Optimal Temperature by Species and Year") +
  scale_y_continuous(limits = c(0, 2000))
ggsave("Output/Preliminary/volume_optimal_temp_by_species_and_year.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# extract site_id
temp_df$site_id_num = str_extract(temp_df$site_id, "(?<=GLM_nhdhr_)(.*)(?=_NLDAS.feather)")
loc_df$site_id_num = sub(".*nhdhr_", "", loc_df$nhdhr.id)

# merge
temp_df = left_join (temp_df,loc_df,by="site_id_num")
temp_df = temp_df[complete.cases(temp_df$lat),]

# the trend of the optimal days
# Group the data frame by species and site_id
grouped_df <- group_by(temp_df, species, site_id)

# Calculate the correlation coefficient between year and days_optimal_temp for each group
cor_df <- summarise(grouped_df, cor = cor(year, days_optimal_temp))

# add lon and lat
cor_df = left_join(cor_df,temp_df[,c("site_id","state","lat","long")],by="site_id")

# plot cor for different species
p3 = ggplot(cor_df, aes(x = species, y = cor, group = species)) +
  geom_boxplot(position = "dodge") +
  theme_bw() +
  labs(x = "Species", y = "Correlation coefficients between year and optimal days") 
ggsave("Output/Preliminary/Correlation coefficients between year and optimal days.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# plot two figure example for increase and decrease optimal days
site_highest_cor = cor_df[cor_df$cor==max(cor_df$cor),]$site_id
site_lowest_cor = cor_df[cor_df$cor==min(cor_df$cor),]$site_id

temp_df_highest_cor = temp_df[temp_df$site_id%in%site_highest_cor,]
temp_df_lowest_cor = temp_df[temp_df$site_id%in%site_lowest_cor,]

# increase
p4 = ggplot(data = temp_df_highest_cor, aes(x = year, y = days_optimal_temp)) +
    facet_wrap(~ species, scales = "free") +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE) +
    theme_bw() +
  labs(x = "Year", y = "Days with Optimal Temperature",
                     title = "lake nhdhr_151959760; lat 44.5 long -87.5")
ggsave("Output/Preliminary/Positive Correlation_one_lake.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# decrease
p5 = ggplot(data = temp_df_lowest_cor, aes(x = year, y = days_optimal_temp)) +
  facet_wrap(~ species, scales = "free") +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Year", y = "Days with Optimal Temperature",
       title = "lake nhd_14768080; lat 42.5 long -88.3")
ggsave("Output/Preliminary/Negative Correlation_one_lake.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# plot spatial figures
#cor_df_we <- cor_df[cor_df$species == "walleye", ]
loc_map <- map_data(map = "state") %>%
  filter(region == "wisconsin")

p6 <- ggplot() +
  geom_polygon(data = loc_map, aes(long, lat, group = group), fill = "azure3", color = "black") +
  geom_point(data = cor_df, aes(long, lat, size = abs(cor), color = ifelse(cor >= 0, "Positive", "Negative")), shape = 15) +
  scale_size_continuous(range = c(1, 5)) +
  scale_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()+
  facet_wrap(~ species)
ggsave("Output/Preliminary/Correlation_spatial.png", plot = last_plot(), width = 12, height = 10, dpi = 300) 
