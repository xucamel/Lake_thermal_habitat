setwd("C:/Scratch/LLX/Project_3")
library(dplyr)
library(tools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(tidyr)

nod_result = readRDS("Output/number_good_days_all_lakes_all_ftp.rds")
#nod_result = final_result

#  Read fish FTP
read.csv('Data/spp_thermal_metrics.csv') %>%
  filter(!is.na(ftp)) %>%
  distinct(latin.name, .keep_all = TRUE) %>%
  mutate(latin.name = gsub("_", " ", toTitleCase(latin.name))) %>%
  select(latin.name, ftp) -> fish_ftp


# 4. merge the data into nod_result
# nod_result <- nod_result_ftp_all_lake %>%
#   left_join(
#     fish_ftp %>% select(latin.name, ftp),
#     by = c("ftp")
#   ) 

# 5. thermal group by ftp
nod_result <- nod_result %>%
  mutate(Temp_Category = cut(ftp, breaks = c(-Inf, 18, 23, Inf), labels = c("Cold", "Cool", "Warm")))

names(nod_result)[3] = "days_with_optimal_temp"

#### figure. The shift of optimal temperature duration ####
# Calculate the mean for the firs and last 21 years
mean_result_1980 <- nod_result %>%
  group_by(site_id, ftp) %>%
  filter(year %in% (1980:2000)) %>%  # filter for the first 10 years
  summarise(mean_days_1980 = mean(days_with_optimal_temp, na.rm = TRUE), .groups = "drop")

mean_result_2001 <- nod_result %>%
  group_by(site_id, ftp) %>%
  filter(year %in% (2001:2021)) %>%  # filter for the first 10 years
  summarise(mean_days_2001 = mean(days_with_optimal_temp, na.rm = TRUE), .groups = "drop")

# Merge the two data frames to have a combined result
mean_result_every_20_years <- mean_result_1980 %>%
  left_join(mean_result_2001, by = c("site_id", "ftp"))

mean_result_every_20_years <- mean_result_every_20_years %>%
  mutate(across(everything(), ~replace_na(., 0)))

## plot the difference for each species
# calculate the difference of days
dif_figure_df = mean_result_every_20_years

# the diff of median of days 
dif_figure_df = dif_figure_df %>%
  group_by(ftp) %>%
  mutate(median_all_1980 = median(mean_days_1980), median_all_2001 = median(mean_days_2001), sum_days_1980 = sum(mean_days_1980), sum_days_2001 = sum(mean_days_2001)) %>%
  mutate(dif_median_all = median_all_2001-median_all_1980, dif_sum = sum_days_2001 - sum_days_1980, dif_sum_percentage = (sum_days_2001 - sum_days_1980)/sum_days_1980)

# add species_label
#dif_figure_df$Species_label = paste(dif_figure_df$species, "(", sprintf("%.1f", dif_figure_df$ftp), ")", sep = "")

# plot the dif of median of days
median_figure_df = dif_figure_df[,c("ftp","dif_median_all","median_all_1980")]
median_figure_df = unique(median_figure_df)

mean_positive <- mean(median_figure_df$dif_median_all[median_figure_df$dif_median_all > 0], na.rm = TRUE)
mean_negative <- mean(median_figure_df$dif_median_all[median_figure_df$dif_median_all < 0], na.rm = TRUE)
median_figure_df = median_figure_df[median_figure_df$ftp<30.5,]
g_dif_upper <- ggplot(median_figure_df, aes(x = ftp, y = dif_median_all)) +
  geom_col(aes(fill = ifelse(dif_median_all > 0, "blue", "red")), width = 0.4) +
  geom_hline(yintercept = mean_positive, linetype = "dashed", color = "blue", size = 0.5) +
  geom_hline(yintercept = mean_negative, linetype = "dashed", color = "red", size = 0.5) +
  theme(axis.text.x = element_text(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank()) +
  scale_fill_identity() +
  labs(title = "",
       x = "Temperature (°C)",
       y = "Change of days") +
  coord_cartesian(ylim = c(-10, 10))+
  # Add shaded regions instead of vertical lines
  annotate("rect", xmin = -Inf, xmax = 19, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 19, xmax = 23, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "springgreen1") +
  annotate("rect", xmin = 23 , xmax = Inf , ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "coral") 


jpeg("Output/Figure/dif_median_days_all_ftp_lakes_1.jpeg",width=8,height=4,units = "in",res=600)
g_dif_upper
dev.off()
