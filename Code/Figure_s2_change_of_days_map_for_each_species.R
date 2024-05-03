setwd("C:/Scratch/LLX/Project_3")
library(dplyr)
library(tools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(patchwork)
library(sf)
library(dplyr)
library(ggplot2)
library(egg)
library(cowplot)
library(tidyverse)

# 1.read the NOD results #
# Get the list of all CSV files in the directory that start with 'days'
csv_files <- list.files(path = "C:/Scratch/LLX/Project_3/Output", pattern = "^days_per_year.*\\.csv$", full.names = TRUE)
df_list <- lapply(csv_files, read.csv)
nod_result <- do.call(rbind,df_list)

# the number of lake species pair
unique_pairs_count <- nod_result %>%
  distinct(site_id, species) %>%
  nrow()

# 2. Read fish FTP
read.csv('Data/spp_thermal_metrics.csv') %>%
  filter(!is.na(ftp)) %>%
  distinct(latin.name, .keep_all = TRUE) %>%
  mutate(latin.name = gsub("_", " ", toTitleCase(latin.name))) %>%
  select(latin.name, ftp) -> fish_ftp

# 3. Read meta data of the lake model
lake_meta_df = read.csv("Data/lake_metadata.csv")

# 4. merge the data into nod_result
nod_result <- nod_result %>%
  left_join(
    fish_ftp %>% select(latin.name, ftp),
    by = c("species" = "latin.name")
  ) %>%
  left_join(
    lake_meta_df %>% select(site_id, state, centroid_lon, centroid_lat, max_depth, area, elevation, clarity),
    by = "site_id"
  )

# 5. thermal group by ftp
nod_result <- nod_result %>%
  mutate(Temp_Category = cut(ftp, breaks = c(-Inf, 18, 23, Inf), labels = c("Cold", "Cool", "Warm")))

# Calculate the mean for the firs and last 21 years
mean_result_1980 <- nod_result %>%
  group_by(site_id, species) %>%
  filter(year %in% (1980:2000)) %>%  # filter for the first 10 years
  summarise(mean_days_1980 = mean(days_with_optimal_temp, na.rm = TRUE), .groups = "drop")

mean_result_2001 <- nod_result %>%
  group_by(site_id, species) %>%
  filter(year %in% (2001:2021)) %>%  # filter for the first 10 years
  summarise(mean_days_2001 = mean(days_with_optimal_temp, na.rm = TRUE), .groups = "drop")

# Merge the two data frames to have a combined result
mean_result_every_20_years <- mean_result_1980 %>%
  left_join(mean_result_2001, by = c("site_id", "species"))

# read into all_result
all_result <- left_join(nod_result,mean_result_every_20_years,by = c("site_id", "species"))

# Filter out rows where both columns are 0
all_result <- all_result %>%
  filter(!(mean_days_1980 == 0 & mean_days_2001 == 0))

# Create bins for every 1 unit of ftp
all_result$bin <- cut(all_result$ftp, breaks = seq(floor(min(all_result$ftp)), ceiling(max(all_result$ftp)), by = 1), include.lowest = TRUE, labels = FALSE)

# Calculate the mean and median of mean_days for each bin
stats_df <- all_result %>%
  group_by(bin) %>%
  summarize(
    mean_first_half = mean(mean_days_1980),
    median_first_half = median(mean_days_1980),
    mean_last_half = mean(mean_days_2001),
    median_last_half = median(mean_days_2001),
    bin_midpoint = (floor(max(ftp)) + floor(min(ftp))) / 2
  )

#### figure. The shift of optimal temperature duration ####
## plot the difference for each species
# calculate the difference of days
dif_figure_df = all_result[,c("site_id","ftp","species","mean_days_1980","mean_days_2001","Temp_Category")]
dif_figure_df = unique(dif_figure_df)

# the diff of median of days 
dif_figure_df = dif_figure_df %>%
  group_by(species) %>%
  mutate(median_all_1980 = median(mean_days_1980), median_all_2001 = median(mean_days_2001), sum_days_1980 = sum(mean_days_1980), sum_days_2001 = sum(mean_days_2001)) %>%
  mutate(dif_median_all = median_all_2001-median_all_1980, dif_sum = sum_days_2001 - sum_days_1980, dif_sum_percentage = (sum_days_2001 - sum_days_1980)/sum_days_1980)

# add species_label
dif_figure_df$Species_label = paste(dif_figure_df$species, "(", sprintf("%.1f", dif_figure_df$ftp), ")", sep = "")

# not lake species
no_lake_spcies = c("Hybognathus placitus")
dif_figure_df = dif_figure_df[!dif_figure_df$species%in%no_lake_spcies,]

# plot the dif of median of days
median_figure_df = dif_figure_df[,c("Species_label","ftp","dif_median_all","median_all_1980")]
median_figure_df = unique(median_figure_df)

mean_positive <- mean(median_figure_df$dif_median_all[median_figure_df$dif_median_all > 0], na.rm = TRUE)
mean_negative <- mean(median_figure_df$dif_median_all[median_figure_df$dif_median_all < 0], na.rm = TRUE)

# plot the map
dif_figure_df$dif_each_lake_species = dif_figure_df$mean_days_2001-dif_figure_df$mean_days_1980

map_df = left_join(dif_figure_df,lake_meta_df,"site_id")

map_mean_trend_df <- map_df 
map_mean_trend_df$mean_Trend = map_mean_trend_df$dif_each_lake_species

# change the extreme values to 20 or -20
map_mean_trend_df <- map_mean_trend_df %>%
  mutate(mean_Trend_adjust_value_20 = case_when(
    mean_Trend > 20 ~ 20,
    mean_Trend < -20 ~ -20,
    TRUE ~ mean_Trend
  ))

# map 
wi <- map_data("state")

map_mean_trend_df_cold = map_mean_trend_df[map_mean_trend_df$Temp_Category=="Cold",]

g1 = ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +  # US states
  geom_point(data = map_mean_trend_df_cold, aes(x = centroid_lon, y = centroid_lat, color = mean_Trend_adjust_value_20), size = 0.25) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +  # Adjust xlim and ylim as needed
  facet_wrap(~Species_label, nrow = 16, strip.position = "left") +  # Use custom labeller for panel titles
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  scale_color_gradientn(
    colors = RColorBrewer::brewer.pal(11, "Spectral"),
    name = "",
    limits = c(-20, 20),
    breaks = c(-20, -10, 0, 10, 20),
    labels = c("-20<", "-10", "0", "10", "20>")
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "None"
  ) +
  labs(tag="a")


# Generate the histogram 
g2 <- ggplot(data = map_mean_trend_df_cold, aes(x = mean_Trend_adjust_value_20)) +
  geom_histogram(aes(fill = pmin(..x.., 20)), alpha = 1, bins = 40) +  # Cap fill values at 20
  scale_x_continuous(
    limits = c(-20, 20),  # Set x-axis limits
    breaks = c(-20, -10, 0, 10, 20),  # Set x-axis tick positions
    labels = c("-20<", "-10", "0", "10", "20>")  # Customize x-axis tick labels
  ) +
  scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(11, "Spectral"),
    limits = c(-20, 20),  # Adjust the scale to cap at 20
    breaks = c(-20, -10, 0, 10, 20),
    labels = c("-20<", "-10", "0", "10", "20>")
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.9),
    legend.title = element_blank() 
  ) +
  geom_vline(aes(xintercept = 0), color = "black", alpha = 0.99, size = 1, linetype = "dashed") +
  xlab("Change of preferred days") +
  ylab("Count") +
  facet_wrap(~Species_label, nrow = 16)+
  labs(tag="b")


# Combine the plots
combined_plot <- plot_grid(g1, g2, ncol = 2, rel_heights = c(1.5, 0.5), rel_widths = c(1, 0.85))

# save the plot
jpeg("Output/Figure/Figure_map_hist.jpeg",width=7,height=7,units = "in",res=600)
combined_plot 
dev.off()

library(ggplot2)
library(RColorBrewer)
library(gridExtra) # For plot_grid

# Assuming 'map_mean_trend_df_cold' is already defined and has a 'Species_label' column

# Split data into four subsets, each with four species

# Calculate species order based on mean FTP
species_ftp_order <- map_mean_trend_df %>%
  group_by(Species_label) %>%
  summarize(mean_ftp = mean(ftp, na.rm = TRUE)) %>%
  arrange(mean_ftp) %>%
  pull(Species_label)

# Function to create a plot for a given species group, ensuring species order by FTP
create_plots_for_group <- function(data, group_id, species_order) {
  # Reorder species labels in the data subset according to the specified order
  data$Species_label <- factor(data$Species_label, levels = species_order)
  
  g1 <- ggplot() +
    geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +
    geom_point(data = data, aes(x = centroid_lon, y = centroid_lat, color = mean_Trend_adjust_value_20), size = 0.25) +
    coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
    facet_wrap(~Species_label, nrow = 3, strip.position = "left") +
    xlab("Longitude (°W)") +
    ylab("Latitude (°N)") +
    scale_color_gradientn(
      colors = RColorBrewer::brewer.pal(11, "Spectral"),
      name = "",
      limits = c(-20, 20),
      breaks = c(-20, -10, 0, 10, 20),
      labels = c("-20<", "-10", "0", "10", "20>")
    ) +
    theme(panel.background = element_blank(), plot.background = element_blank(),
          panel.grid = element_blank(), legend.position = "None") +
    labs(tag="a")
  
  g2 <- ggplot(data = data, aes(x = mean_Trend_adjust_value_20)) +
    geom_histogram(aes(fill = pmin(..x.., 20)), alpha = 1, bins = 40) +
    scale_x_continuous(
      limits = c(-20, 20),
      breaks = c(-20, -10, 0, 10, 20),
      labels = c("-20<", "-10", "0", "10", "20>")
    ) +
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(11, "Spectral"),
      limits = c(-20, 20),
      breaks = c(-20, -10, 0, 10, 20),
      labels = c("-20<", "-10", "0", "10", "20>")
    ) +
    theme_minimal() +
    theme(strip.text = element_blank(), panel.background = element_blank(),
          plot.background = element_blank(), panel.grid = element_blank(),
          legend.position = c(0.85, 0.9), legend.title = element_blank()) +
    geom_vline(aes(xintercept = 0), color = "black", alpha = 0.99, size = 1, linetype = "dashed") +
    xlab("Change of preferred days") +
    ylab("Count") +
    facet_wrap(~Species_label, nrow = 3) +
    labs(tag="b")
  
  combined_plot <- plot_grid(g1, g2, ncol = 2, rel_heights = c(1.5, 0.5), rel_widths = c(1, 0.85))
  return(combined_plot)
}

# Create and save plots for each group, ensuring correct species order within each plot
for (i in 1:20) {
  species_subset <- species_ftp_order[((i-1)*3+1):(i*3)]
  data_subset <- map_mean_trend_df[map_mean_trend_df$Species_label %in% species_subset,]
  plot <- create_plots_for_group(data_subset, i, species_subset)
  o = 100+i
  ggsave(paste0("Output/Figure/species_group_", o, ".png"), plot, width = 7, height = 7)
}


