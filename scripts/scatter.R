##############################################################################
## Project: unnamed Euroflow modelling paper - scatter plots
## Capabilities: process variables pulled with main_analyses.R, usually called yo
## author: cordula.wittekind@ufz.de
##############################################################################
## create yo in main_analysis, two to four scenarios, left_join with distinctive suffix

# if extra info like catchment size is needed
GRAND_data <- read.dbf("input/GRanD_reservoirs_v1_3.dbf", as.is=T)

gr = tab %>% filter(GRAND_ID %in% damids) %>% select(c(GRAND_ID, CATCH_SKM, MAIN_USE, Q_km3_yr, CAP_MCM.x,WSL ))

gr = gr %>% left_join(yo, by = c("GRAND_ID" = "damID")) %>% left_join(GRAND_data%>%select(COUNTRY,GRAND_ID, MAIN_USE, DAM_NAME), by = "GRAND_ID")

## make color for estuaries
colors_40_curated <- c(
  "#FF0000", "#0000FF", "#00FF00", "#FFD700", "#FF69B4", "#8A2BE2", "#00CED1", "#FF4500",
  "#32CD32", "#FF1493", "#1E90FF", "#ADFF2F", "#FF6347", "#4169E1", "#00FA9A", "#DC143C",
  "#7FFF00", "#9932CC", "#00BFFF", "#FFA500", "#98FB98", "#FF00FF", "#87CEEB", "#F0E68C",
  "#DDA0DD", "#90EE90", "#FFB6C1", "#20B2AA", "#F5DEB3", "#87CEFA", "#D2691E", "#B0E0E6",
  "#FFEFD5", "#C0C0C0", "#F4A460", "#2E8B57", "#DAA520", "#6B8E23", "#BC8F8F", "#4682B4"
)
names(colors_40_curated) = sort(unique(yo$basin))

#check range
   names(yo)
  range(yo[,1:7]);range(yo[,9:15])

sc = list()
# gr_half = gr %>% filter(GRAND_ID %in% alldams)

for(scen in names(a)[3:9]){
  
# subset needed (e.g. for color)?
  # gr_sub <- gr %>%
  # mutate(col_var = ifelse(.data[["GRAND_ID"]] %in% alldams, "1", "0"))
  # gr_sub[[col_var]] <- factor(gr_sub[[col_var]], levels = c("0", "1"))
  
# pull the right column for y_var
  # x_var = past e0(scen,"_dry")
  # x_var = paste0(scen, "_dry")
  y_var = paste0(scen,"_fix")

# third/color/shape variables  
  # x_var = paste0(scen,"_tno")
  col_var = paste0(scen,"_den")

# model <- lm(yo[[y_var]] ~ yo[[x_var]])
# ic = round(summary(model)$coefficients[1,1], 2)#intercept
# ic_se = round(summary(model)$coefficients[1,2], 2)
# 
# sl = round(summary(model)$coefficients[2,1], 2)#slope
# sl_se = round(summary(model)$coefficients[2,2], 2)
# r_squared <- round(summary(model)$r.squared,2)
  
sc[[k]] =
  
  # leg =
  ggplot(yo,  aes(x = .data[["scenario -1"]], y = .data[["scenario 0"]], color = .data[["basin"]]#, size = .data[["basin"]]
                      ))+
  geom_point(size = 4)+theme_bw() +
  ggtitle(scen)+
  # geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "lm", se = T, color="#5971A9") +
  geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
  # annotate("text", x = Inf, y = -Inf,
           # label = paste(paste0("burial = ",ic," (SE=",ic_se,") + "),paste0(sl," (SE=",sl_se,") x residence time (","R² =", round(r_squared, 3),")"),sep ="\n"),
           # hjust = 1, vjust = 0, size = 3.5) +
  # viridis::scale_color_viridis(
    # alpha = 0.8,
    # limits = c(-87, 40),
    # breaks =c(-80,-60,-40,-20,0,20,40),
    # name = "%change, Denitrification"
  # )+
  scale_color_manual(values =  colors_40_curated)+  #
  # scale_size(range = c(0.3, 2.2), breaks =c(-80,-60,-40,-20,0,20,40),guide = 'none') +
  ylab("with dams (scenario 0)")+#scale_y_continuous(labels = scales::label_comma(),limits = c(0,43),breaks = seq(0,100,10))+
  xlab("without dams (scenario -1)")#+
    # scale_x_log10(limits = c(0.0001,1e04) ,expand = c(0, 0) ,breaks = c(0.001, 0.1,10,  1000))#+
  # scale_x_continuous(limits = c(0,142),labels = scales::label_comma(),breaks = seq(-100,200,20))

}#mit 6 1200x850, mit 1 570/480

#getting the legend
legend_only <- cowplot::get_legend(leg) #create leg element above manually
# sc[[8]] = legend_only
#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)
#square
m <- matrix(c(1,2,3,
              4,5,6,
              7,8,9),ncol =3, byrow = T)
# sc[[9]] = legend_only
grid.arrange(grobs = sc, layout_matrix = m)  #1000x500 with 9 landscape, 1200x850 with 9 square 
