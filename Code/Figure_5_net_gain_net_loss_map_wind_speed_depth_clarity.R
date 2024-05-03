setwd("C:/Scratch/LLX/Project_3")
library(dplyr)
library(tools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(broom)
library(patchwork)
library(cowplot)

lake_meta_df = read.csv("Data/lake_metadata.csv")
wind_trend_and_difference = read.csv("Output/wind_speed_trend_difference.csv")
temperature_slopes = read.csv("Output/high_low_temperature_within_range_slope_method_1.csv")

temperature_slopes = left_join(temperature_slopes,wind_trend_and_difference,"site_id")

sum(temperature_slopes$min_temp_slope>temperature_slopes$max_temp_slope&temperature_slopes$max_temp_slope>0, na.rm = TRUE)/length(temperature_slopes$site_id)
sum(temperature_slopes$min_temp_slope<temperature_slopes$max_temp_slope&temperature_slopes$min_temp_slope>0, na.rm = TRUE)/length(temperature_slopes$site_id)

# temperatrue profile shifting pattern
temperature_slopes <- temperature_slopes %>%
  mutate(shift_pattern = case_when(
    min_temp_slope > max_temp_slope & max_temp_slope >0 ~ "Net loss",
    max_temp_slope > min_temp_slope & min_temp_slope >0 ~ "Net gain",
    TRUE ~ "Other" # In case they are equal
  ))
sum(temperature_slopes$shift_pattern=="Net gain")/nrow(temperature_slopes)
sum(temperature_slopes$shift_pattern=="Net loss")/nrow(temperature_slopes)

temperature_slopes = left_join(temperature_slopes,lake_meta_df,by="site_id")
# Calculate mean depth for each category
mean_depths <- temperature_slopes %>%
  group_by(shift_pattern) %>%
  summarise(mean_depth = mean(max_depth))

# Plotting
# Plot 1 depth
g1 <- ggplot(temperature_slopes, aes(x = max_depth, fill = shift_pattern)) +
  geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net gain",], 
                 aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 1, color = "black") +
  geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net loss",], 
                 aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 1, color = "black") +
  scale_fill_manual(values = c("Net loss" = "blue", "Net gain" = "red")) +
  labs(title = "c", x = "Lake maximum depth (meters)", y = "Pecentage") +
  theme_minimal() +
  xlim(0, 30)+
  scale_y_continuous(labels = scales::percent_format())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
                legend.box.just = "right", legend.margin = margin(0,0,0,0),
                legend.background = element_blank(), legend.title = element_blank())

# Plot 2 clarity
g2 <- ggplot(temperature_slopes, aes(x = clarity, fill = shift_pattern)) +
  geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net gain",], 
                 aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 0.1, color = "black") +
  geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net loss",], 
                 aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 0.1, color = "black") +
  scale_fill_manual(values = c("Net loss" = "blue", "Net gain" = "red")) +
  labs(title = "d", x = "Lake clarity (meters)", y = "Percentage") +
  theme_minimal() +
  xlim(0, 4)+
  scale_y_continuous(labels = scales::percent_format())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(0,0,0,0),
        legend.background = element_blank(), legend.title = element_blank())

# map of the lake with lose and Net gain pattern
temperature_slopes$ratio_max_min = temperature_slopes$max_temp_slope-temperature_slopes$min_temp_slope
temperature_slopes$shift_pattern[temperature_slopes$shift_pattern=="Other"] = "Other 3%"
temperature_slopes$shift_pattern[temperature_slopes$shift_pattern=="Net gain"] = "Net gain 26%"
temperature_slopes$shift_pattern[temperature_slopes$shift_pattern=="Net loss"] = "Net loss 71%"

wi <- map_data("state")
g3 = ggplot() +
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +  # US states
  geom_point(data = temperature_slopes, aes(x = centroid_lon, y = centroid_lat, color = shift_pattern), size = 0.4,alpha = 0.5) +
  coord_fixed(ratio = 1, xlim = c(-105, -80), ylim = c(30, 48.5)) +
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  scale_color_manual(
    values = c("Net gain 26%" = "red", "Net loss 71%" = "blue", "Other 3%" = "black"),
    name = ""  # Multiline legend title
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.92, 0.86)
  )+
  guides(
    color = guide_legend(override.aes = list(size = 1))  # Increase legend point size
  )+
  labs(title="a")

g4 = ggplot(temperature_slopes,aes(x = trend))+
  geom_histogram(aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 0.001, color = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(0,0,0,0),
        legend.background = element_blank(), legend.title = element_blank())+
  labs(title = "b", x = "Wind speed trends above lake surface (m/s/year)", y = "Percentage")+
  xlim(-0.03,0.03)+
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(
    label="99.6%\nStronger wind", 
    x=0.022,
    y=0.14,
    size = 3
  )
  
combined_plot_left <- plot_grid(g3, g4, ncol = 1,rel_heights = c(2, 1), rel_widths = c(2, 1))
combined_plot_right <- plot_grid(g4, g1, g2, ncol = 3)
combined_plot <- plot_grid(g3 , combined_plot_right, ncol = 1, rel_heights = c(2, 1), rel_widths = c(2, 1))
#combined_plot 

jpeg("Output/Figure/Fig5_map_gain_loss_wind_depth_clarity.jpeg",width=10,height=10,units = "in",res=600)
combined_plot 
dev.off()

jpeg("Output/Figure/Fig5_wind_depth_clarity_ppt.jpeg",width=10,height=4,units = "in",res=600)
plot_grid(g4, g1, g2, ncol = 3)
dev.off()
# # Plot 3 
# temperature_slopes$max_depth_divide_clarity = temperature_slopes$max_depth/temperature_slopes$clarity
# p3 <- ggplot(temperature_slopes, aes(x = max_depth_divide_clarity, fill = shift_pattern)) +
#   geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net gain",], 
#                  aes(y = ..density..), alpha = 0.5, binwidth = 0.2, color = "black") +
#   geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net loss",], 
#                  aes(y = ..density..), alpha = 0.5, binwidth = 0.2, color = "black") +
#   scale_fill_manual(values = c("Net loss" = "blue", "Net gain" = "red")) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
#         legend.box.just = "right", legend.margin = margin(0,0,0,0),
#         legend.background = element_blank(), legend.title = element_blank()) +
#   labs(title = "C", x = "Depth / Clarity", y = "Density") +
#   xlim(0, 50)
# 
# p3 <- ggplot(temperature_slopes, aes(x = max_depth_divide_clarity, fill = shift_pattern)) +
#   geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net gain",],
#                  aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 0.2, color = "black") +
#   geom_histogram(data = temperature_slopes[temperature_slopes$shift_pattern == "Net loss",],
#                  aes(y = ..count../sum(..count..)), alpha = 0.5, binwidth = 0.2, color = "black") +
#   scale_fill_manual(values = c("Net loss" = "blue", "Net gain" = "red")) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.position = c(0.95, 0.95), legend.justification = c("right", "top"),
#         legend.box.just = "right", legend.margin = margin(0,0,0,0),
#         legend.background = element_blank(), legend.title = element_blank()) +
#   labs(title = "C", x = "Depth / Clarity", y ="Percentage (%)") +
#   xlim(0, 50)+
#   scale_y_continuous(labels = scales::percent_format())
# 
# combined_plot <- (p1 | p2) / p3
# combined_plot

# jpeg("Output/Figure/gain_lose_more_pattern_with_depth_clarity_and_both.jpeg",width=7,height=7,units = "in",res=600)
# combined_plot
# dev.off()