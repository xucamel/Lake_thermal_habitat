setwd("C:/Scratch/LLX/Project_3")

# Load necessary library
library(purrr)
library(tidyverse)
library(lubridate)
library(tools)
library(RColorBrewer)
library(dplyr)
library(tools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(patchwork)
library(egg)
library(cowplot)

# global value
lake_n = 12688
year_n = 21

# read the data
high_low_temp_df = readRDS("Output/high_low_temp_df.rds")
monthly_df = readRDS("Output/monthly_df.rds")
monthly_spatial_df = readRDS("Output/monthly_spatial_days_sum_below_contain_above.rds")
lake_meta_df = read.csv("Data/lake_metadata.csv")
wi <- map_data("state")

# monthly sum days
early_year_limit <- min(high_low_temp_df$year) + year_n - 1  # First 21 years

early_df = monthly_df[monthly_df$period=="early",]
late_df = monthly_df[monthly_df$period=="later",]
late_df$year = late_df$year - year_n
# Merge the early and late period data
dif_result <- early_df %>%
  left_join(late_df, by = c("year", "month"), suffix = c("_early", "_late"))

# the difference
dif_lake_days_df <- dif_result %>%
  mutate(
    difference_15 = monthly_contains_15_late -  monthly_contains_15_early,
    difference_21 = monthly_contains_21_late -  monthly_contains_21_early,
    difference_27 = monthly_contains_27_late -  monthly_contains_27_early,
    
    difference_below_15 = monthly_below_15_late - monthly_below_15_early,
    difference_below_21 = monthly_below_21_late - monthly_below_21_early,
    difference_below_27 = monthly_below_27_late - monthly_below_27_early,
    
    difference_above_15 = monthly_above_15_late - monthly_above_15_early,
    difference_above_21 = monthly_above_21_late - monthly_above_21_early,
    difference_above_27 = monthly_above_27_late - monthly_above_27_early
  ) %>%
  select(month, year, difference_15, difference_21, difference_27, difference_below_15, difference_below_21, difference_below_27, difference_above_15, difference_above_21, difference_above_27)

# rows with issues
row_positive = c(22,26,30,34,38,42)
row_negative = c(26,30,34,38,42)-1

# correction
dif_lake_days_df[row_positive,6:8] = dif_lake_days_df[row_positive,6:8]+lake_n
dif_lake_days_df[row_negative,6:8] = dif_lake_days_df[row_negative,6:8]-lake_n

# Add a month column and group by it
monthly_difference <- dif_lake_days_df %>%
  gather(key = "temperature", value = "difference", -year, -month) %>%
  group_by(month, temperature) %>%
  summarise(difference = sum(difference, na.rm = TRUE))

monthly_difference <- monthly_difference %>%
  separate(temperature, into = c("temp_type", "temp_value"), sep = "_(?=[^_]+$)")

monthly_difference$month <- factor(monthly_difference$month, 
                                   levels = 1:12, 
                                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
# Adding '℃' to temp_value
monthly_difference$temp_value <- paste0(monthly_difference$temp_value,"°C" )

# Replacing 'difference' with 'Contain' in temp_type
monthly_difference$temp_type <- gsub("difference", "Contain", monthly_difference$temp_type)
monthly_difference$temp_type <- gsub("Contain_above", "Above", monthly_difference$temp_type)
monthly_difference$temp_type <- gsub("Contain_below", "Below", monthly_difference$temp_type)

# Convert temp_type to a factor with specific order
monthly_difference$temp_type <- factor(monthly_difference$temp_type, levels = c("Below", "Contain", "Above"))

# combined figure 
# calculate the figure
text_1 = sum(dif_lake_days_df$difference_15)/year_n
text_2 = text_1/(sum(dif_result$monthly_contains_15_early)/year_n)*100

text_3 = sum(dif_lake_days_df$difference_21)/year_n
text_4 = text_3/(sum(dif_result$monthly_contains_21_early)/year_n)*100

text_5 = sum(dif_lake_days_df$difference_27)/year_n
text_6 = text_5/(sum(dif_result$monthly_contains_27_early)/year_n)*100

monthly_difference$panel_text <- ifelse(monthly_difference$temp_value == "15°C", round(text_1),
                                        ifelse(monthly_difference$temp_value == "21°C", round(text_3), round(text_5)))

g1 = ggplot(monthly_difference, aes(x = month, y = difference/year_n, fill = temp_type)) +
  geom_col() +
  labs(title = "", y = "Change of days", x = "Month") +
  scale_fill_manual(values = c("Contain" = "blue", "Below" = "green", "Above" = "red")) +
  theme_minimal() +
  facet_grid(~ temp_value) + 
  geom_text(aes(label = panel_text, y = 3.2*lake_n, x=6.5), vjust = -0.5, hjust = 0.5, color = "black") +  # Use 'panel_text' for annotations
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank())

# Print the plot
g1
jpeg("Output/Figure/monthly_dif_lake_days_certain_below_above_temp_combined.jpeg",width=9,height=5,units = "in",res=600)
g1
dev.off()

# only plot the variation of lake-days that contains certain temperature
contain_only_df = monthly_difference[monthly_difference$temp_type=="Contain",]

contain_only_df <- contain_only_df %>%
  arrange(month) %>%
  group_by(temp_value) %>%
  mutate(cumulative_difference = cumsum(difference/year_n))

g1 <- ggplot(contain_only_df, aes(x = month)) +
  geom_col(aes(y = difference/year_n, fill = difference/year_n > 0), show.legend = FALSE,alpha=0.9) +  # Conditional fill for the bars
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +  # Set bar colors
  labs(title = "", y = "Change of days", x = "Month") +
  theme_minimal() +
  facet_wrap(~ temp_value, nrow=1) + 
  geom_text(aes(label = panel_text, y = 1.7*lake_n, x=6.5), vjust = -0.5, hjust = 0.5, color = "black") +  # Annotations
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  geom_line(aes(y = cumulative_difference/4, group = 1), color = "black", size = 1.2, linetype = "twodash", alpha=1) +  # Cumulative curve modifications
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Cumulative change of days")) +  # Secondary y-axis
  labs(tag="a")

custom_labels <- setNames(c("Cold-days", "Cool-days", "Warm-days"), unique(contain_only_df$temp_value))

g1_2 <- ggplot(contain_only_df, aes(x = month)) +
  geom_col(aes(y = difference/year_n, fill = difference/year_n > 0), show.legend = FALSE, alpha = 0.9) +  # Conditional fill for the bars
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +  # Set bar colors
  labs(title = "", y = "Change of days", x = "Month") +  # Adding title
  theme_minimal() +
  facet_wrap(~ temp_value, nrow = 1, labeller = labeller(temp_value = custom_labels)) +  # Custom labels for facets
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14),  # Rotate and enlarge x-axis text
    axis.text.y = element_text(size = 14),  # Enlarge y-axis text
    strip.text = element_text(size = 16),  # Enlarge facet strip text
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  ) +
  geom_line(aes(y = cumulative_difference/4, group = 1), color = "black", size = 1.2, linetype = "twodash", alpha = 1) +  # Cumulative curve
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Cumulative change of days", labels = scales::comma)) +   # Secondary y-axis with comma labels
  labs(tag="a")
 
ggsave("Output/Figure/Figure_3a_monthly_change_of_days_2.jpeg", g1_2, width=12, height=7, units = "in", dpi=600)

# Fig 3b
# Read the highest and lowest temp for each day
rds_files <- list.files(path = "C:/Scratch/LLX/Project_3/Output", pattern = "^highest_lowest_temp_.*\\.rds$", full.names = TRUE)
df_list <- lapply(rds_files, readRDS)
high_low_temp_df <- do.call(rbind,df_list)

# release the RAM and make the data size smaller
rm(df_list)
# high_low_temp_df <- high_low_temp_df %>%
#   sample_frac(0.15)
high_low_temp_df$max_temp = round(high_low_temp_df$max_temp, 1)
high_low_temp_df$min_temp = round(high_low_temp_df$min_temp, 1)
gc()

# Extract month from the time column
high_low_temp_df$month <- month(high_low_temp_df$time)
high_low_temp_df$year <- year(high_low_temp_df$time)

# Prepare data for plotting as before
prepare_for_plot <- function(df) {
  df %>%
    group_by(month, site_id) %>%
    summarize(month_mean_max = mean(max_temp, na.rm = TRUE),
              month_mean_min = mean(min_temp, na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = "TempType", value = "Temperature", month_mean_min, month_mean_max)
}


# mean of the monthly temperature of all years
high_low_temp_long <-  prepare_for_plot(high_low_temp_df)

# Create the combined violin plot
g2 <- ggplot(high_low_temp_long, aes(x = factor(month), y = Temperature, fill = TempType, color = TempType)) +  # Add color aesthetic
  geom_violin(position = position_dodge(width = 0.3),linewidth=0.8) +
  labs(x = "Month", y = "Temperature", title = "") +
  scale_fill_manual(values = c("month_mean_min" = "blue", "month_mean_max" = "red"), labels = c("Min Temp", "Max Temp")) +
  scale_color_manual(values = c("month_mean_min" = "blue", "month_mean_max" = "red")) +  # Match outline colors to fill colors
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 11, ymax = 19, alpha = 0.15, fill = "blue") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 19, ymax = 23, alpha = 0.15, fill = "springgreen1") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 23, ymax = 31, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 11, alpha = 0.25, fill = "gray") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 31, ymax = Inf, alpha = 0.25, fill = "gray") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Removes major gridlines
        panel.grid.minor = element_blank(),  # Removes minor gridlines
        legend.position = "none") +
  scale_x_discrete(labels = c('1' = 'Jan', '2' = 'Feb', '3' = 'Mar', '4' = 'Apr', '5' = 'May', '6' = 'Jun', '7' = 'Jul', '8' = 'Aug', '9' = 'Sep', '10' = 'Oct', '11' = 'Nov', '12' = 'Dec'))+
  labs(tag="b")
#coord_flip()

g2_2 <- ggplot(high_low_temp_long, aes(x = factor(month), y = Temperature, fill = TempType, color = TempType)) +  # Add color aesthetic
  geom_violin(position = position_dodge(width = 0.3),linewidth=0.8) +
  labs(x = "Month", y = "Temperature", title = "") +
  scale_fill_manual(values = c("month_mean_min" = "blue", "month_mean_max" = "red"), labels = c("Min Temp", "Max Temp")) +
  scale_color_manual(values = c("month_mean_min" = "blue", "month_mean_max" = "red")) +  # Match outline colors to fill colors
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 11, ymax = 19, alpha = 0.15, fill = "blue") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 19, ymax = 23, alpha = 0.15, fill = "springgreen1") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 23, ymax = 31, alpha = 0.15, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 11, alpha = 0.25, fill = "gray") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 31, ymax = Inf, alpha = 0.25, fill = "gray") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Removes major gridlines
        panel.grid.minor = element_blank(),  # Removes minor gridlines
        legend.position = "none") +
  scale_x_discrete(labels = c('1' = 'Jan', '2' = 'Feb', '3' = 'Mar', '4' = 'Apr', '5' = 'May', '6' = 'Jun', '7' = 'Jul', '8' = 'Aug', '9' = 'Sep', '10' = 'Oct', '11' = 'Nov', '12' = 'Dec'))+
  labs(tag="b") +
  theme(
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text.x = element_text(size = 14),  # Rotate and enlarge x-axis text
    axis.text.y = element_text(size = 14),  # Enlarge y-axis text
    strip.text = element_text(size = 16),  # Enlarge facet strip text
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  ) 


ggsave("Output/Figure/Figure_3a_monthly_change_of_days.jpeg", g1, width=12, height=7, units = "in", dpi=600)
ggsave("Output/Figure/Figure_3b_lake_highest_lowest_temp.jpeg", g2, width=10, height=4, units = "in", dpi=600)

# combine g1 and g2 
combined_plot <- plot_grid(g1_2, g2_2, ncol = 1, rel_heights = c(1.5, 1), rel_widths = c(1, 1))
ggsave("Output/Figure/monthly_dif_lake_days_contain_certain_temp_with_cumulative_days_color_with_violin_plot_2.jpeg", combined_plot, width=10, height=10, units = "in", dpi=600)

# surface temperature violin plot 

