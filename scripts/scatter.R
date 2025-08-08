##############################################################################
## Project: unnamed Euroflow modelling paper - scatter plots
## Capabilities: process variables pulled with main_analyses.R, usually called yo
## author: cordula.wittekind@ufz.de
##############################################################################
## create yo in main_analysis, two to four scenarios left_join with disctinctive suffix

# with extra info like catchment size is needed
GRAND_data <- read.dbf("input/GRanD_reservoirs_v1_3.dbf", as.is=T)

gr = tab %>% filter(GRAND_ID %in% damids) %>% select(c(GRAND_ID, CATCH_SKM, MAIN_USE, Q_km3_yr, CAP_MCM.x,WSL ))

gr = gr %>% left_join(yo, by = c("GRAND_ID" = "damID")) %>% left_join(GRAND_data%>%select(COUNTRY,GRAND_ID, MAIN_USE, DAM_NAME), by = "GRAND_ID")

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
  x_var = paste0(scen,"_rt")
  y_var = scen
  # y_var = paste0(scen,"_wet")

# third/color/shape variables  
  # col_var = paste0(scen,"_ti")
  # col_var = "color_var"

model <- lm(yo[[y_var]] ~ yo[[x_var]])
ic = round(summary(model)$coefficients[1,1], 2)#intercept
ic_se = round(summary(model)$coefficients[1,2], 2)

sl = round(summary(model)$coefficients[2,1], 2)#slope
sl_se = round(summary(model)$coefficients[2,2], 2)
r_squared <- round(summary(model)$r.squared,2)
  
sc[[scen]] = ggplot(yo,
                    aes(x = .data[[x_var]], y = .data[[y_var]]),size=2) +
  geom_point()+theme_bw() +
  ggtitle(scen)+
  geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "lm", se = T, color="#5971A9") +
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
  annotate("text", x = Inf, y = -Inf,
           label = paste(paste0("burial = ",ic," (SE=",ic_se,") + "),paste0(sl," (SE=",sl_se,") x residence time (","R² =", round(r_squared, 3),")"),sep ="\n"),
           hjust = 1, vjust = 0, size = 3.5) +
  # viridis::scale_color_viridis(alpha = 0.8, limits=c(-36,875 ),n.breaks=10,
                               # guide = guide_colorbar("TN inflow")) +
  # scale_color_manual(values = c("0" = "#335145", "1" = "#ff8427"),labels = c("0" = "< 500 kg/day and/or < 10*inflow","1"=">500 kg/day & >10*inflow"))  +
  # scale_size(range = c(0.5, 2.5)) +
  ylab("% change N burial")+scale_y_continuous(limits=c(-87,38),labels = scales::label_comma())+
  xlab("% change residence time")+scale_x_continuous(limits=c(-100,24),labels = scales::label_comma())+labs(color ="Groups")#+
 # guides(color = guide_colorbar(title = "Groups"), size="none")
                                                                                                         
# 
}#mit 6 1200x850

#getting the legend
# legend_only <- cowplot::get_legend(leg) #create leg element above manually
# sc[[8]] = legend_only

m <- matrix(c(1, 2, 3,
              4, 5, 6,
              7,8,9), ncol=3, byrow=TRUE)
sc[[8]] = grid::nullGrob()
sc[[9]] = grid::nullGrob()
grid.arrange(grobs = sc, layout_matrix = m)  #1000x850


### Assessing tn inflow/outflow and reasons for high tn outflow
yo_filtered <- yo %>%
  filter(`scenario 4_` > 10 * `scenario 4_ti`)


## Horizontal scatter plot

for(scen in names(a)[3:9]){
  col_var = paste0(scen,"_rt")
  sc[[scen]] = ggplot(gr,
                      aes(x = .data[[scen]], y = .data[["WSL"]],
                          colour = .data[[col_var]]),size=2) +
    geom_point()+theme_bw() +

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
    viridis::scale_color_viridis(alpha = 0.8, limits=c(-100,25 ),n.breaks=10,
                                 guide = guide_colorbar("Residence time"))+
    ylab("Watershed Level")+#scale_y_continuous(limits=c(-14,100),labels = scales::label_comma())+
    xlab("% change, TN outflow")+scale_x_continuous(limits=c(-4,100),labels = scales::label_comma())#+
  # guides(color = guide_colorbar(title = "Groups"), size="none")
  
  
}