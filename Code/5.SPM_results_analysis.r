setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")

# read the parameter estiamtes (rda and csv)
output_rda = readRDS("Output/WI_SPM.rda")
quantity_csv = read.csv("Output/WI_SPM.csv")

# check the convergency
split_r = data.frame(split_r = rep(0,dim(output_rda)[1]))
for (i in 1:dim(output_rda)[1]){
  split_r$split_r[i] = max(output_rda[i,,6])
}
sum(split_r$split_r<1.05)

# warming impact figure df
figure_2_df = data.frame(warming_impact = output_rda[,8,1], high = output_rda[,8,5], low = output_rda[,8,4],split_r = split_r$split_r,stock_id = quantity_csv$stock_id,initial_depletion = output_rda[,4,1])

# species and wbic
figure_2_df$species =  substr(figure_2_df$stock_id, nchar(figure_2_df$stock_id) - 1, 35)
figure_2_df$wbic = substr(figure_2_df$stock_id, 1, nchar(figure_2_df$stock_id) - 2)

# change species from short name to full name
figure_2_df <- figure_2_df %>%
  mutate(species = case_when(
    species == "WE" ~ "Walleye",
    species == "BC" ~ "Black crappie",
    species == "CC" ~ "Cisco",
    species == "YP" ~ "Yellow perch",
    species == "NP" ~ "Northern pike",
    species == "SB" ~ "Smallmouth bass",
    species == "BG" ~ "Bluegill",
    species == "LB" ~ "Largemouth bass",
    TRUE ~ species
  ))

# plot
# warming significant
# reorder 
figure_temp_influence = figure_2_df[figure_2_df$split_r<1.05,]
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$warming_impact),]

# col
figure_temp_influence$col_line <- ifelse(figure_temp_influence$low > 0, "blue", 
                                         ifelse(figure_temp_influence$high < 0, "red", "black"))
g1 = ggplot(figure_temp_influence, aes(x = warming_impact, y = rev(row.names(figure_temp_influence)), color = col_line)) +
  geom_segment(aes(x = low, xend = high, y = rev(row.names(figure_temp_influence)), yend = rev(row.names(figure_temp_influence)), color = col_line), size = 0.6) +
  geom_hline(yintercept = 0, linetype = 3, color = "black", size = 0.8) +
  geom_vline(xintercept = mean(figure_temp_influence$warming_impact), color = "blue", size = 0.8, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", size = 0.8, linetype = "dashed") +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, 0), breaks = seq(-4, 4, by = 1), name = "θ") +
  scale_y_discrete(limits = factor(row.names(figure_temp_influence), levels = rev(row.names(figure_temp_influence))), name = "") +
  scale_color_manual(values = c("gray15", "blue", "red")) +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(),
        axis.ticks = element_line(color = "black", size = 0.6),
        axis.title = element_text(),
        plot.title = element_text(),
        panel.grid = element_blank()) + 
  geom_point(size = 2, shape = 19)
ggsave("Output/Preliminary/sita_value.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# warming vs species
impact_explain = figure_2_df[figure_2_df$split_r<1.05,]
g2 = ggplot(impact_explain, aes(x=factor(species,levels = c("CC","YP","WE","SB","BG","BC","LB")), y=warming_impact)) + 
  geom_boxplot(notch=T)+
  theme_bw()+
  ylab("θ")+
  xlab("Species")+
  geom_hline(yintercept = 0, linetype="dashed",color = "red", size=1)+
  ylim(-2,2)
ggsave("Output/Preliminary/sita_species.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

cor.test(impact_explain$initial_depletion,abs(impact_explain$warming_impact))
g3 = ggplot(impact_explain, aes(x=initial_depletion, y=abs(warming_impact))) + 
  geom_point( color="black") +
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE)+
  theme_bw()+
  ylab("Absolute value of θ")+
  xlab("Depletion in initial year \n (ratio of biomass to carrying capacity \nin the initial year)")+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))
ggsave("Output/Preliminary/sita_depletion.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


#### final arrange figure ####
layout_matrix <- matrix(c(1,2,
                          1,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3,
                             layout_matrix=layout_matrix, 
                             widths=c(0.3,0.35),
                             heights=c(0.3,0.28))
plotdir <- "Output/Figure"
ggsave(g, filename=file.path(plotdir, "Fig2_main_final.png"), 
       width=7, height=8.5, units="in", dpi=600)

#### correlation of warming impact in the same lakes
###### CORRELATION of impact_E in the same lake for different species
figure_3_cor = figure_2_df[figure_2_df$split_r<10.05,]
cor_lake_species = data.frame(wbic=figure_3_cor$wbic)

cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Largemouth bass",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Smallmouth bass",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Black crappie",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Walleye",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Yellow perch",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Cisco",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Northern pike",],"wbic")
cor_lake_species = left_join(cor_lake_species,figure_3_cor[figure_3_cor$species=="Bluegill",],"wbic")

cor_lake_species = cor_lake_species[,c(1,2,9,16,23,30,37,44,51)]
names(cor_lake_species) = c("wbic","Largemouth bass","Smallmouth bass","Black crappie","Walleye","Yellow perch","Cisco","Northern pike","Bluegill")

cor_lake_species = cor_lake_species[,c("wbic","Largemouth bass","Black crappie","Bluegill","Smallmouth bass","Northern pike","Walleye","Yellow perch","Cisco")]

# cor figure

cor_matrix <- cor(cor_lake_species[,c(2:8)], use = "pairwise.complete.obs")
df <- as.data.frame(cor_matrix)
df$x <- rownames(df)
df$y <- colnames(df)[1:7]
df <- reshape2::melt(df)
df$variable <- factor(df$variable, levels = c("Largemouth bass","Black crappie","Bluegill","Smallmouth bass","Northern pike","Walleye","Yellow perch"))
df$x <- factor(df$x, levels = c("Largemouth bass","Black crappie","Bluegill","Smallmouth bass","Northern pike","Walleye","Yellow perch"))

g4 = ggplot(df, aes(x = x, y = variable)) +
  geom_tile(fill="white") +
  theme_minimal() +
  coord_fixed() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 0.5),plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm")) +
  geom_text(aes(label = format(round(value, 2), nsmall = 2), color = ifelse(value >= 0, "red", "blue")))#+
#labs(tag="D")

g4

#### last figure 
fig_4_data <- quantity_csv %>% 
  # Calculate fishing effect
  mutate(effect_fishing=(P_terminal-P_no_fishing)*-1,
         effect_warming=(P_terminal-P_no_env)*-1)

fig_4_data = fig_4_data[figure_2_df$split_r<1.05,]

fig_4_data = fig_4_data %>%
  mutate(ccol="red")

fig_4_data[fig_4_data$effect_warming>0&fig_4_data$effect_fishing>fig_4_data$effect_warming,]$ccol = "coral"

fig_4_data[fig_4_data$effect_warming<0&fig_4_data$effect_fishing>abs(fig_4_data$effect_warming),]$ccol = "blue"

fig_4_data[fig_4_data$effect_fishing>0&fig_4_data$effect_warming<0&fig_4_data$effect_fishing<abs(fig_4_data$effect_warming),]$ccol = "darkgreen"


# Plot data
jpeg("Output/Figure/Fig4_compare_quantify_4_CPUE.jpeg", width=6, height=6, res=600, units = "in")
ggplot(fig_4_data, aes(x=effect_fishing,y=effect_warming,col=ccol)) +
  geom_point(aes(col=ccol)) +
  # Reference lines
  geom_hline(yintercept=0, color="black", linetype = "dashed") +
  geom_abline(slope=-1, color="black", linetype = "dashed") +
  geom_abline(slope=1, color="black", linetype = "dashed") +
  # Label quadrants
  annotate(geom="text", x=0.1, y=0.4, 
           label="Warming \ndepletes the population \nmore than fishing\n1%", color="red") +
  annotate(geom="text", x=0.6, y=0.4, 
           label="Fishing depletes the\npopulation more\n than warming\n47%", color="coral3") +
  annotate(geom="text", x=0.1, y=-0.4, 
           label="Warming \nfully offsets fishing\n6%", color="blue") +
  annotate(geom="text", x=0.6, y=-0.4, 
           label="Warming \npartially offsets fishing\n46%", color="darkgreen") +
  # Labels
  labs(x="Contribution of fishing\nto stock depletion", 
       y="Contribution of warming\nto stock depletion") +
  # Theme
  theme_bw() +
  xlim(0, 0.7) +
  ylim(-0.5, 0.5) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("darkgreen", "coral3","blue", "red"))
dev.off()
