##############################################################################
## Project: unnamed Euroflow modelling paper - main analyses and prep for figures
## Capabilities: plot model outputs
## Date created:  8.4.2026
## author: cordula.wittekind@ufz.de
##############################################################################
rm(list=ls(all=TRUE))
gc()
library(here)
setwd("..")
#main_analysis.R pulls modelled variables and other data needed for the functions it defines:
source("scripts/main_analysis.R")

#### standard procedure (yo is what gets passed to scatter plots) ####

##1 get the variable of interest (3285 days = 9 years, spin-up already removed)
# v =  pull_scen_var(vars = "TN_Rs_r",stat = mean, resolution = "dam")

##2 pull the right resolution dam or estuary
# a = df_dam(v)

##3 (optional) calculate the difference to scenario 0
# c = pct_vs_scenario0(a, suffix = "_tno")

##4 merge different datasets to pass to scatter.R
# yo = b %>% left_join(c, by ="damID")


################################### FIGURES ####################################

## AB - retention vs residence time scenario 0 | both TN and TP ####
# retention = (outflow-inflow)/inflow

# TN retention 
v =  pull_scen_var(vars = "TN_inflow",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
ci = a[,c(1,3)]

v =  pull_scen_var(vars = "TN_Rs_r",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
co = a[,c(1,3)]

ret <- data.frame(damID=ci[, 1], ret_dam = ((ci[, 2] - co[, 2]) / ci[, 2])*100)

#difference in percentage retention between scenario -1 and 0
# delta_ret = data.frame(damID = ci1[, 1], delta_ret = ret2$ret_dam - ret1$ret_nodam)

# residence time median(S_Rs_r)/sum(Q_Rs_r) 
v =  pull_scen_var(vars = "S_Rs_r",stat = median, resolution = "dam")
sr = df_dam(v, scen_nodams = F)
sr = sr[,c(1,3)]

v =  pull_scen_var(vars = "Q_Rs_r",stat = sum, resolution = "dam")
qr = df_dam(v, scen_nodams = F)
qr = qr[,c(1,3)]

RT = sr[,2]/qr[,2] 
RT = RT*3285
RT= data.frame(damID = sr[,1], RT = RT)

# RT_delta = ((RT2-RT1)/RT1)*100
# RT_delta[is.na(RT_delta)] = 0
# RT_delta = data.frame(damID = ci1[, 1], RT_delta = RT_delta)


yo = ret %>% left_join(RT, by = "damID")

# AB1 plot 1000X800
ggplot(yo,  aes(x =`scenario -1` , y =`scenario 0` ))+

  geom_point(size=3)+theme_bw() +
  
  ## add abline if needed
  # geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size =16),#,angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 22),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
scale_x_continuous(limits = c(0,10000))+
scale_y_continuous(limits = c(-62,100))+ xlab("Residence time (days)") + ylab("TN retention (%)")

# TP retention 
v =  pull_scen_var(vars = "TP_inflow",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
ci = a[,c(1,3)]

v =  pull_scen_var(vars = "TP_Rs_r",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
co = a[,c(1,3)]

ret <- data.frame(damID=ci[, 1], ret_dam = ((ci[, 2] - co[, 2]) / ci[, 2])*100)

yo = ret %>% left_join(RT, by = "damID")

# AB2 plot 1000X800
ggplot(yo,  aes(x =RT , y =ret_dam ))+
  
  geom_point(size=3)+theme_bw() +
  
  ## add abline if needed
  # geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size =16),#,angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 22),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
  scale_x_continuous(limits = c(0,10000))+
  scale_y_continuous(limits = c(-62,100))+ xlab("Residence time (days)") + ylab("TP retention (%)")



## BC - percentage change in TN retention vs. percentage change in residence time (7 scenarios) ####
# BC1 - TN retention 
v =  pull_scen_var(vars = "TN_inflow",stat = sum, resolution = "dam")
tni = df_dam(v)

v =  pull_scen_var(vars = "TN_Rs_r",stat = sum, resolution = "dam")
tno = df_dam(v)


ret <- data.frame(damID = tni[, 1], ret = ((tni[, 2:9] - tno[, 2:9]) / tni[, 2:9])*100)
names(ret) = names(tno)
ret = pct_vs_scenario0(ret, suffix = "_tn")

# residence time median(S_Rs_r)/sum(Q_Rs_r)esi
v =  pull_scen_var(vars = "S_Rs_r",stat = median, resolution = "dam")
sr = df_dam(v)
v = pull_scen_var(vars = "Q_Rs_r",stat = sum, resolution = "dam")
qr = df_dam(v)

RT = sr[,2:9]/(qr[,2:9])
# RT = RT*3285
RT[RT==0] = NA

RT = ((RT[,2:8]-RT[,1])/RT[,1])*100
RT[is.na(RT)] = 0
RT$damID = qr$damID
## merge with other variables
yo = RT  %>% left_join(ret, by = "damID")

#custon transformation
custom_trans <- function() {
  transform <- function(x) {
    sign(x) * ifelse(
      abs(x) <= 100,
      abs(x),
      100 + (abs(x) - 100) / (7000 - 100) * 20
    )
  }
  
  inverse <- function(x) {
    sign(x) * ifelse(
      abs(x) <= 100,
      abs(x),
      100 + (abs(x) - 100) * (7000 - 100) / 20
    )
  }
  
  scales::trans_new(
    name = "custom",
    transform = transform,
    inverse = inverse
  )
}

# BC1 plot 1200x850
range(yo[,1:7]) #range retention time 
sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  
  x_var = scen
  y_var = paste0(scen,"_tn")

  sc[[scen]] =
  
  # leg =
  ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+

    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    

    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-400,6842),
      trans = custom_trans(),breaks = c(-100,-50,0,50,100,6000),
      labels = function(x) ifelse(abs(x) >= 1000, paste0(x / 1000, "k"), x))+
    
     scale_x_continuous(limits = c(-100,7))+
    xlab("Change in residence time (%)") +
    ylab("Change in TN retention (%)")

}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 


# BC2 - TP retention 
v =  pull_scen_var(vars = "TP_inflow",stat = sum, resolution = "dam")
tni = df_dam(v)

v =  pull_scen_var(vars = "TP_Rs_r",stat = sum, resolution = "dam")
tno = df_dam(v)


ret <- data.frame(damID = tni[, 1], ret = ((tni[, 2:9] - tno[, 2:9]) / tni[, 2:9])*100)
names(ret) = names(tno)
ret = pct_vs_scenario0(ret, suffix = "_tp")

yo = RT  %>% left_join(ret, by = "damID");range(yo[,9:15])

# plot BC2
sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  
  x_var = scen
  y_var = paste0(scen,"_tp")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-100,530),
                       trans = custom_trans(),breaks = c(-100,-50,0,50,100),
                       labels = function(x) ifelse(abs(x) >= 1000, paste0(x / 1000, "k"), x))+
    
    scale_x_continuous(limits = c(-100,7))+
    xlab("Change in residence time (%)") +
    ylab("Change in TP retention (%)")
  
}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 

## CD - percentage change TN burial vs percentage change residence time, color = denit ########
#RT from above
v =  pull_scen_var(vars = "F_Nb_Rs_r",stat = sum, resolution = "dam")
bur = df_dam(v)
bur = pct_vs_scenario0(bur, suffix = "_bur")

v =  pull_scen_var(vars = "denitrification",stat = sum, resolution = "dam")
den = df_dam(v)
den = pct_vs_scenario0(den, suffix = "_den")

yo = RT  %>% left_join(den, by = "damID")%>%left_join(bur, by ="damID")

# CD plot
sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  
  x_var = scen
  y_var = paste0(scen,"_bur")
  col_var = paste0(scen,"_den")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[col_var]]))+
    
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-82,28)
                       )+
    
    scale_x_continuous(limits = c(-100,7))+
    viridis::scale_color_viridis(
    name = "% change Denitrification",
    limits = c(-81,40),
    breaks =c(-80,-40,-60,-20,0,20,40) ,
    alpha = 0.8
    )+
    
    xlab("Change in residence time (%)") +
    ylab("Change in TN burial (%)")
  
}
legend_only <- cowplot::get_legend(leg) #create leg element above manually
sc[[8]] = legend_only
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 


## DE - hydrograph and percentage change residence time (previously 5.1 and 9) ########
# DE, part1 bar plot
l <- melt(RT[,1:7])

ggplot(l) + geom_boxplot(aes(x=variable, y=value,fill=variable))+
  # ggtitle(expression("Change in residence time (%)"))  +
  theme_bw()+theme(axis.title.y=element_text(size=25),axis.title.x=element_blank(), plot.title = element_text(size=15),
                   axis.text = element_text(size=18),legend.position = "none")+
  ylab("% change")#+scale_y_continuous( limits = c(-1,2.5))

# DE, part 2 hydrograph
v =  pull_scen_var(vars = "inflow",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
inflow = a[,1:2]
top4 = inflow  %>% arrange(desc(`scenario -1`))%>%slice(1:20)
dam_basin_match %>% filter(damID %in% top4$damID)
# dam_chosen = 4629# (volga)
# dam_chosen = 2664
dam_chosen = 4374 #Dnepr
# dam_chosen = 6841 #Rhone
# dam_chosen = 4363    
##ENVIRONMENTAL FLOW
setwd("C:/Users/wittekin/Documents/cle2024/projects/EuroFlow/data/eflow_scen")
list.files() #is the right order

all_scen = list.files(pattern = ".csv");all_scen
if(length(all_scen)==7) {
  alles = list()
  
  for (i in 1:7) {
    sc = all_scen[i]
    sc_read = read.csv(sc,header=TRUE,stringsAsFactors=FALSE,nrows =450)#set nrows = 12, enough for Volga
    eflow = sc_read %>% filter(X == dam_chosen) %>% select(-X)
    names(eflow) = 1:3650
    alles[[paste0("scenario",i)]] = eflow ;rm(eflow)
  }
}
eflows = bind_rows(alles, .id ="scenario")

#convert to same format as outflow below
eflows = as.data.frame(t(eflows))
colnames(eflows) = eflows[1,]
eflows = eflows[-1,]
names(eflows) = gsub("([0-9])", " \\1", names(eflows))

#has to align
bas = "Dnepr"

scens = list()#fill a list with all scenarios subset to basin
for(eflowscen in names(scena)){ 
  sim = scena[[eflowscen]]
  sim = sim[[bas]] #subset scenario to basin
  scens[[eflowscen]] = sim  
}


pull_scen_series = function(vars,dam){
  dov_list = list() #one variable pulled for dam 
  for(n in names(scens)){
    val_list = scens[[n]]
    one_val_list = val_list[[vars]]
    
    dov_list[[n]] = one_val_list[as.character(dam),]
  }
  va = bind_rows(dov_list, .id ="scenario")
  return(va)
}

## OUTFLOW
#Q_Rs_r = water outflow from reservoir
#Q_down_r = qspill+max(qeflow, qnconsum) CONTAINS EFLOW!

outflow = pull_scen_series(vars = "Q_Rs_r",dam = dam_chosen )

outflow = outflow[,3:9]#not looking at nodam/noef for now


## INFLOW
infl = pull_scen_series(vars = "inflow", dam = dam_chosen)
inflow = infl[,3:9]


## NATURAL FLOW
# load simulation results for naturalized flow
sim <- readRDS("../results_scen/sim_all_boxm_seasT_resOP_scen-1.RDS")#same as nores2

# extract inflows (so far only European rivers were modelled)
nat_flow <- sim[[bas]]$Q_Rs_r #probably the same as scenario -1
nat_flow = nat_flow[as.character(dam_chosen),]

### ALIGN STR AND MERGE ALL TOGETHER
melt_for_ggplot_tidy <- function(df, date_col = "date") {
  df %>%
    pivot_longer(cols = -all_of(date_col), 
                 names_to = "series", 
                 values_to = "value")
}
pl = list()

#fix range of y to max range, get title of scenario, pull legend and get the right label
dfs <- list(eflows, outflow, inflow)
all_values <- unlist(lapply(dfs, function(df) unlist(df[sapply(df, is.numeric)])))
# ymax = max(all_values, na.rm = TRUE)+0.5 #if the whole hydrograph shall be shown
ymax = 250

col_val = c( "#136F63","#7E7CF5"  ,"#241E4E", "coral")
names(col_val) = c("environmental flow","natural flow","outflow","inflow")

for(eflowsc in names(eflows)){ #fix loop later and fill plot list
  
  eflo = as.numeric(eflows[,eflowsc])
  outflo = outflow[,eflowsc]
  inflo = inflow[[eflowsc]]
  
  topl = cbind(eflo,  nat_flow, outflo, inflo) %>% as.data.frame()
  
  #date column, starting in winter...
  topl$date <- as.Date("1989-12-21") + (seq_len(nrow(topl)) - 1)
  
  # topl = topl[-c(1:365),] #remove only spin-up
  topl = topl[which(topl$date>as.Date("1995-01-01")),]
  topl = topl[which(topl$date<as.Date("1997-01-01")),]
  
  topl_seas = topl
  topl_seas$seas = seas::mkseas(topl, width = "DJF")
  topl_seas$year = year(topl_seas$date) 
  topl_fin = topl_seas%>%
    group_by(year,seas) %>%
    summarise(across(where(is.numeric) & !contains("date"), \(x) mean(x, na.rm = TRUE)))
  
  
  dat = melt_for_ggplot_tidy(topl)
  dat = dat %>% mutate(series = replace(series, series ==eflowsc,"outflow"))%>%
    mutate(series = replace(series, series =="inflo","inflow"))%>%
    mutate(series = replace(series, series =="eflo","environmental flow"))%>%
    mutate(series = replace(series, series =="nat_flow","natural flow"))
  
  
  
  pl[[eflowsc]] =
    # leg =
    ggplot(dat, aes(x = date, y = value, color = series)) +ggtitle(eflowsc)+
    geom_line(size = 1.1, alpha = 0.8) +
    scale_color_manual(values =col_val) +
    theme_minimal() +
    scale_x_date(labels = scales::date_format("%m-%Y"))+
    theme(
      legend.position  = "none",
      plot.title = element_text(size= 21),
      axis.title = element_text(size = 18),
      legend.title = element_blank(), 
      legend.text = element_text(size = 17)
    )+
    ylim(0,ymax)+labs(x = "time", y = bquote("million m"^3))
  
}
legend_only <- cowplot::get_legend(leg) #create leg element above manually

pl[[8]] = legend_only

m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)
grid.arrange(grobs = pl, layout_matrix = m)  #1000x500 with 8, 

## EF = change in high and low flow season releases ###########

v =  pull_scen_var(vars = "Q_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "wet")
wet =  df_dam(v)

wet = pct_vs_scenario0(wet, suffix = "_wet")

v =  pull_scen_var(vars = "Q_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "dry")
dry =  df_dam(v)

dry = pct_vs_scenario0(dry, suffix = "_dry")

yo = wet %>% left_join(dry, by = "damID") # we're cutting off a few outliers c(-66,100)


sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  x_var = paste0(scen,"_dry")
  y_var = paste0(scen,"_wet")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-66,250))+
    
    scale_x_continuous(limits = c(-66,250))+
    xlab("Change during dry season (%)") +
    ylab("Change during wet season (%)")
  
}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 


## FG - changes in denit and nitrogen fixation in dry and wet season ###########
# FG1 - denitrification
v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam", dry_vs_wet = "wet")
denw =  df_dam(v)

denw = pct_vs_scenario0(denw, suffix = "_denw")

v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam", dry_vs_wet = "dry")
dend =  df_dam(v)

dend = pct_vs_scenario0(dend, suffix = "_dend")

yo = denw %>% left_join(dend, by = "damID") # we're cutting off a few outliers c(-66,100)

#FG2 - fixation (run above, change variable to fixation instead of denitrification)
# yo$denfix = "Denitrification"
# yo2$denfix = "Fixation"

# yo3 = bind_rows(yo,yo2) 



sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  x_var = paste0(scen,"_dend")
  y_var = paste0(scen,"_denw")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    # ggplot(yo3,  aes(x = .data[[x_var]], y = .data[[y_var]],colour = denfix))+
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-90,126))+
    
    scale_x_continuous(limits = c(-90,126))+
    # scale_color_discrete(name = "Process", labels = c("Denitrification", "Nitrogen fixation"), palette = c("#7B0178","#005B4E" ))+
    xlab("Change during dry season (%)") +
    ylab("Change during wet season (%)")
  
}
legend_only <- cowplot::get_legend(leg) #create leg element above manually
sc[[8]] = legend_only
#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 



## GH - changes in retention in dry/wet season
# TN retention 
v =  pull_scen_var(vars = "TN_inflow",stat = sum, resolution = "dam", dry_vs_wet = "wet")
a = df_dam(v)
ci = a

v =  pull_scen_var(vars = "TN_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "wet")
a = df_dam(v)
co = a

retw <- data.frame(damID=ci[, 1], ret_dam = ((ci[, 2:9] - co[, 2:9]) / ci[, 2:9])*100)
dif_ret = retw[,3:9]-retw[,2]
retd_w = data.frame(damID = retw[,1], wet = dif_ret)

v =  pull_scen_var(vars = "TN_inflow",stat = sum, resolution = "dam", dry_vs_wet = "dry")
a = df_dam(v)
ci = a

v =  pull_scen_var(vars = "TN_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "dry")
a = df_dam(v)
co = a

retd <- data.frame(damID=ci[, 1], ret_dam = ((ci[, 2:9] - co[, 2:9]) / ci[, 2:9])*100)

dif_ret = retd[,3:9]-retd[,2]
retd_d = data.frame(damID = retd[,1], dry = dif_ret)

##
custom_trans <- function() {
  transform <- function(x) {
    sign(x) * ifelse(
      abs(x) <= 100,
      abs(x),
      100 + (abs(x) - 100) / (1000 - 100) * 20
    )
  }
  
  inverse <- function(x) {
    sign(x) * ifelse(
      abs(x) <= 100,
      abs(x),
      100 + (abs(x) - 100) * (1000 - 100) / 20
    )
  }
  
  scales::trans_new(
    name = "custom",
    transform = transform,
    inverse = inverse
  )
}
##


yo = retd_d %>% left_join(retd_w, by = "damID") 
names(yo) = c("damID", paste0("scenario ",seq(1,7),"_dry"), paste0("scenario ",seq(1,7),"_wet"))

sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  x_var = paste0(scen,"_dry")
  y_var = paste0(scen,"_wet")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-100,100))+
    
    scale_x_continuous(limits = c(-1000,1000),trans = custom_trans(),breaks = c(-100,-50,0,50,100),
                       labels = function(x) ifelse(abs(x) >= 1000, paste0(x / 1000, "k"), x))+
    xlab("Change during dry season (%)") +
    ylab("Change during wet season (%)")
  
}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 


## FGB - FG with two sideways columns
#wet
v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam", dry_vs_wet = "wet")
denw =  df_dam(v)

denw = pct_vs_scenario0(denw, suffix = "_denw")

v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam", dry_vs_wet = "dry")
dend =  df_dam(v)

dend = pct_vs_scenario0(dend, suffix = "_dend")


#dry
v =  pull_scen_var(vars = "denitrification",stat = sum, resolution = "dam", dry_vs_wet = "wet")
denw =  df_dam(v)

denw = pct_vs_scenario0(denw, suffix = "_denw")

v =  pull_scen_var(vars = "denitrification",stat = sum, resolution = "dam", dry_vs_wet = "dry")
dend =  df_dam(v)

dend = pct_vs_scenario0(dend, suffix = "_dend")

#called fixi but it can also be deni
fixi = denw %>% left_join(dend, by = "damID") # we're cutting off a few outliers c(-66,100)

library(tidyverse)
fixi_filtered <- fixi %>%
  filter(if_any(ends_with("_dend") | ends_with("_denw"), ~ abs(.x) >= 15))

fixi_long <- fixi_filtered %>%
  select(damID, ends_with("_dend"), ends_with("_denw")) %>%
  pivot_longer(
    cols = -damID,
    names_to  = c("scenario", "season"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    damID = factor(damID, levels = rev(unique(fixi$damID)))
  )



ggplot(data = fixi_long, aes(x = value, y = damID, fill = season)) +
  geom_col(width = 0.65, position = position_dodge(width = 0.75), alpha = 0.85) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey30") +
  
  facet_wrap(~ scenario, nrow = 1, labeller = label_value) +
  scale_fill_manual(
    values = c("dend" = "#D85A30", "denw" = "#1D9E75"),
    labels = c("dend" = "Low flow days", "denw" = "High flow days")
  ) +labs(x = "Change in fixation (%)", y = "Dam ID") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # axis.title = element_blank(),
    axis.text.x        = element_text(size = 13),
    strip.text         = element_text(face = "bold"),
    panel.spacing      = unit(0.9, "lines"),
    legend.position    = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  )
#850x1050

## HI - N:P ratio in estuary, baseline and no dam scenario ##########

v =  pull_scen_var(vars = "TN_inflow",stat = mean, resolution = "estuary")
tn = df_estuary(v, scen_nodams = F)

tn = tn  / 14.01 #standard atomic weights

v =  pull_scen_var(vars = "TP_inflow",stat = mean, resolution = "estuary")
tp = df_estuary(v, scen_nodams = F)

tp = tp  / 30.97 #standard atomic weights

b = tn/tp
b = b[,1:2]
b$basin = rownames(tp)

##Table info under GH
#no dams
no_dam = b %>% select(basin, `scenario -1`)

nlim = no_dam %>% filter(`scenario -1` <10) 
length(unique(nlim$basin));length(unique(nlim$basin))/40*100

mod =no_dam %>% filter(`scenario -1` > 10) %>% filter(`scenario -1` <16)
length(unique(mod$basin));length(unique(mod$basin))/40*100

trans =no_dam %>% filter(`scenario -1` > 16) %>% filter(`scenario -1` <20)
length(unique(trans$basin));length(unique(trans$basin))/40*100

plim =no_dam %>% filter(`scenario -1` > 20) %>% filter(`scenario -1` <25)
length(unique(plim$basin));length(unique(plim$basin))/40*100

plusplim = no_dam %>% filter(`scenario -1` > 25)
length(unique(plusplim$basin));length(unique(plusplim$basin))/40*100

#baseline
bas = b %>% select(basin, `scenario 0`)

nlim = bas %>% filter(`scenario 0` <10) 
length(unique(nlim$basin));length(unique(nlim$basin))/40*100

mod =bas %>% filter(`scenario 0` > 10) %>% filter(`scenario 0` <16)
length(unique(mod$basin));length(unique(mod$basin))/40*100

trans =bas %>% filter(`scenario 0` > 16) %>% filter(`scenario 0` <20)
length(unique(trans$basin));length(unique(trans$basin))/40*100

plim =bas %>% filter(`scenario 0` > 20) %>% filter(`scenario 0` <25)
length(unique(plim$basin));length(unique(plim$basin))/40*100

plusplim = bas %>% filter(`scenario 0` > 25)
length(unique(plusplim$basin));length(unique(plusplim$basin))/40*100

#plot prep
library(sf)
library(leaflet)
library(webshot)
library(dplyr)

setwd("C:/Users/wittekin/Documents/cle2024/projects/EuroFlow/data/input")
efb = read_sf(dsn = ".", layer = "EuroFlowBasins") #has Name, needs basin_id_match

#EITHER
bas_id = bas %>% mutate(basin = as.numeric(basin)) %>%left_join(basin_id_match, by = c("basin" = "id"))%>%
  dplyr::rename(Name = basin.y)
#OR
no_dam_id = no_dam %>% mutate(basin = as.numeric(basin)) %>%left_join(basin_id_match, by = c("basin" = "id"))%>%
  dplyr::rename(Name = basin.y)

#adapt with the right scenario
europe = efb %>% left_join(no_dam_id, by = "Name")

breaks <- c(6, 10, 16, 20, 25, 65)
mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
#adapt scenario
pal <- colorBin(palette = "viridis",
                domain = europe$`scenario -1`,
                bins = breaks)
legend_cols = pal(mids) #pulled for label in legend
labels <- c("N-limited (6-10)","Moderate (10-16)",
            "Transition (16-20)", "P-limited (20-25)", "Strongly P-limited (>25)")

#adapt scenario name
n = leaflet(europe, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = europe,
              weight = 0,
              fillColor   = "darkgray", fillOpacity = 1) %>%
  addPolygons(
    weight = 0.5,
    color = ~ "#363636",
    fillColor = ~ pal(europe$`scenario -1`),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    # pal = pal,
    values = ~ pal(europe$`scenario -1`),
    colors = legend_cols,
    labels = labels, #TURN OFF FOR PCT CHANGE
    title = "TN:TP ratio",
    position = "bottomright"
  )%>%
  htmlwidgets::onRender("
  function(el, x) {
    var legend = el.querySelector('.legend');
    if (legend) {
      legend.style.fontSize = '24px';
      legend.style.padding = '7px 10px';
      legend.style.lineHeight = '1.3';
    }
    
    var legendItems = el.querySelectorAll('.legend i');
    for (var i = 0; i < legendItems.length; i++) {
      legendItems[i].style.verticalAlign = 'middle';
    }
  }
")
htmlwidgets::saveWidget(n, "temp.html", selfcontained = FALSE)
##ADAPT NAME depending on scenario!!
webshot::webshot("temp.html", file = "../../output/maps/scenario-1.png", cliprect = "viewport", vwidth = 980, vheight = 940)


## IJ - percentage change in TN:TP ratio ##########
##IJa - maps
# b pulled top of GH 
# > names(b)
# [1] "scenario -1" "scenario 0"  "basin" 
pcb = data.frame(basin = b$basin, delta = round(((b[,2]-b[,1])/b[,1])*100))

setwd("C:/Users/wittekin/Documents/cle2024/projects/EuroFlow/data/input")
efb = read_sf(dsn = ".", layer = "EuroFlowBasins") #has Name, needs basin_id_match

pcb = pcb %>% mutate(basin = as.numeric(basin)) %>%left_join(basin_id_match, by = c("basin" = "id"))%>%
  dplyr::rename(Name = basin.y)
europe = efb %>% left_join(pcb, by = "Name")


breaks = seq(-5,35,5);breaks
pal <- colorBin(palette = "viridis",
                domain = europe$delta,
                bins = breaks)
mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
legend_cols = pal(mids)

k = leaflet(europe, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = europe,
              weight = 0,
              fillColor   = "darkgray", fillOpacity = 1) %>%
  addPolygons(
    weight = 0.5,
    color = ~ "#363636",
    fillColor = ~ pal(europe$delta),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal,
    values = ~ pal(europe$delta),
    # colors = legend_cols,
    title = "difference TN:TP ratio (%)",
    position = "bottomright"
  )%>%
  htmlwidgets::onRender("
  function(el, x) {
    var legend = el.querySelector('.legend');
    if (legend) {
      legend.style.fontSize = '22px';
      legend.style.padding = '7px 9px';
      legend.style.lineHeight = '1';
    }
    
    var legendItems = el.querySelectorAll('.legend i');
for (var i = 0; i < legendItems.length; i++) {
  legendItems[i].style.marginTop = '-2px'; 
}
  }
")
########
htmlwidgets::saveWidget(k, "temp.html", selfcontained = FALSE)
webshot::webshot("temp.html", file = "../../output/maps/delta_scen0_scen-1.png", cliprect = "viewport", vwidth = 980, vheight = 940)

#IJb scatter plots
v =  pull_scen_var(vars = "F_Nb_Rs_r",stat = sum, resolution = "dam")
den = df_dam(v, scen_nodams = F)
den = den[,1:3]

ggplot(den,  aes(x = `scenario -1` , y =`scenario 0` ))+
  
  geom_point(size=3)+theme_bw() +
  
  ## add abline if needed
  geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size =16),#,angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 22),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
 xlab("without dams (t/day)") + ylab("baseline / with dams (t/day)")

## JK - changes in burial, high and low flow (A3 is the same for Phosphorus) #######


v =  pull_scen_var(vars = "F_Nb_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "wet")
a = df_dam(v)
burw = pct_vs_scenario0(a, suffix = "_wet")
  
v =  pull_scen_var(vars = "F_Nb_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "dry")
a = df_dam(v)
burd = pct_vs_scenario0(a, suffix = "_dry")
  
yo = burw %>% left_join(burd, by = "damID") ; range(yo[,1:7]); range(yo[,9:15])

# JK plot
sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  x_var = paste0(scen,"_dry")
  y_var = paste0(scen,"_wet")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-90,40), breaks = seq(-80,40,20))+
    
    scale_x_continuous(limits = c(-90,90),breaks = seq(-80,80,20))+
    xlab("Change during dry season (%)") +
    ylab("Change during wet season (%)")
  
}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 



## A1 - comparison total N fluxes (bur + den - fix) scenario 0 vs. -1  ######

v =  pull_scen_var(vars = "F_Nb_Rs_r",stat = sum, resolution = "dam")
bur = df_dam(v, scen_nodams = F)
bur = bur[,1:3]

v =  pull_scen_var(vars = "denitrification",stat = sum, resolution = "dam")
den = df_dam(v, scen_nodams = F)
den = den[,1:3]

v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam")
fix = df_dam(v, scen_nodams = F)
fix = fix[,1:3]

TN = data.frame(damID = bur[,1], no_dam = bur[,2]+den[,2]-fix[,2], scen_0 = bur[,3]+den[,3]-fix[,3])

ggplot(TN,  aes(x = no_dam, y = scen_0))+
  
  geom_point(size=3)+theme_bw() +

  ## add abline if needed
  geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
 scale_y_continuous(labels = scales::label_comma())+
  scale_c_continuous(labels = scales::label_comma())+
  
  xlab("without dams (t/day)") +
  ylab("baseline / with dams (t/day)")




## A2 - change fix vs change rt, colored by denit ##########
v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "dam")
a = df_dam(v)
fix = pct_vs_scenario0(a, suffix = "_fix")

v =  pull_scen_var(vars = "denitrification",stat = sum, resolution = "dam")
a = df_dam(v)
den = pct_vs_scenario0(a, suffix = "_den")

#rt
v =  pull_scen_var(vars = "S_Rs_r",stat = median, resolution = "dam")
sr = df_dam(v)
v = pull_scen_var(vars = "Q_Rs_r",stat = sum, resolution = "dam")
qr = df_dam(v)

RT = sr[,2:9]/(qr[,2:9])
# RT = RT*3285
RT[RT==0] = NA

RT = ((RT[,2:8]-RT[,1])/RT[,1])*100
RT[is.na(RT)] = 0
RT$damID = qr$damID

yo = RT %>% left_join(fix, by = "damID")%>%left_join(den, by ="damID")

sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  
  x_var = scen
  y_var = paste0(scen,"_fix") 
  col_var = paste0(scen,"_den")

  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[col_var]] ))+
    
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_x_continuous(limits = c(-100,10),breaks = seq(-100,100,20))+
    scale_y_continuous(limits = c(-20,60),breaks = seq(-20,60,20))+
    viridis::scale_color_viridis(
      name = "% change Denitrification",
      limits = c(-81,40),
      breaks =c(-80,-40,-60,-20,0,20,40) ,
      alpha = 0.8
    )+
    xlab("Change in residence time (%)") +
    ylab("Change in N fixation (%)")
  
}
legend_only <- cowplot::get_legend(leg) #create leg element above manually
sc[[8]] = legend_only
#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 

## A3 - P burial high flows versus low flows

v =  pull_scen_var(vars = "F_Pb_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "wet")
a = df_dam(v)
burw = pct_vs_scenario0(a, suffix = "_wet")

v =  pull_scen_var(vars = "F_Pb_Rs_r",stat = sum, resolution = "dam", dry_vs_wet = "dry")
a = df_dam(v)
burd = pct_vs_scenario0(a, suffix = "_dry")

yo = burw %>% left_join(burd, by = "damID") ; range(yo[,1:7]); range(yo[,9:15])

# A3 plot
sc = list()

scens = c("scenario 1", "scenario 2", "scenario 3" ,"scenario 4" ,"scenario 5",
          "scenario 6", "scenario 7")

for(scen in scens){
  
  ##select columns for x_var, y_var, col_var
  x_var = paste0(scen,"_dry")
  y_var = paste0(scen,"_wet")
  
  sc[[scen]] =
    
    # leg =
    ggplot(yo,  aes(x = .data[[x_var]], y = .data[[y_var]]))+
    
    geom_point(size=3)+theme_bw() +
    ggtitle(scen)+
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_y_continuous(limits = c(-90,40), breaks = seq(-80,40,20))+
    
    scale_x_continuous(limits = c(-90,106),breaks = seq(-80,100,20))+
    xlab("Change during dry season (%)") +
    ylab("Change during wet season (%)")
  
}

#landscape
m <- matrix(c(1, 2, 3,4, 
              5, 6, 7,8), ncol=4, byrow=TRUE)

grid.arrange(grobs = sc, layout_matrix = m)  #1000x500/1600x800 with 9 landscape, 1200x850 with 9 square 


## MN - three panels no dams vs. dams (scen -1 vs. scen 0) ########
# TN:TP molar ratio
v =  pull_scen_var(vars = "TN_inflow",stat = mean, resolution = "estuary")
tn = df_estuary(v, scen_nodams = F)

tn = tn  / 14.01 #standard atomic weights

v =  pull_scen_var(vars = "TP_inflow",stat = mean, resolution = "estuary")
tp = df_estuary(v, scen_nodams = F)

tp = tp  / 30.97 #standard atomic weights

b = tn/tp
b = b[,1:2]
b$basin = rownames(tp)

tntp_bas = b %>% mutate(basin = as.numeric(basin)) %>%left_join(basin_id_match, by = c("basin" = "id"))%>%
  dplyr::rename(Name = basin.y)


colors_40_curated <- c(
  "#FF0000", "#0000FF", "#00FF00", "#FFD700", "#FF69B4", "#8A2BE2", "#00CED1", "#FF4500",
  "#32CD32", "#FF1493", "#1E90FF", "#ADFF2F", "#FF6347", "#4169E1", "#00FA9A", "#DC143C",
  "#7FFF00", "#9932CC", "#00BFFF", "#FFA500", "#98FB98", "#FF00FF", "#87CEEB", "#F0E68C",
  "#DDA0DD", "#90EE90", "#FFB6C1", "#20B2AA", "#F5DEB3", "#87CEFA", "#D2691E", "#B0E0E6",
  "#FFEFD5", "#C0C0C0", "#F4A460", "#2E8B57", "#DAA520", "#6B8E23", "#BC8F8F", "#4682B4"
)
# names(colors_40_curated) = sort(unique(yo$basin))

basin_names <- sort(unique(tntp_bas$Name))
basin_colors <- setNames(colors_40_curated[1:length(basin_names)], basin_names)

  


  ggplot(tntp_bas,  aes(x =`scenario -1`, y = `scenario 0` , color = Name))+
  
  
  geom_point(size=5)+theme_bw() +

  ## add abline if needed
  geom_abline(slope = 1, intercept = 0)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=15))+
    scale_color_manual(values = basin_colors, na.value = "lightgrey")+
    xlab("without dams") + ylab("baseline / with dams")
    
# TN retention  
  v =  pull_scen_var(vars = "TN_inflow",stat = sum, resolution = "dam")
  tni = df_dam(v, scen_nodams = F)
  
  v =  pull_scen_var(vars = "TN_Rs_r",stat = sum, resolution = "dam")
  tno = df_dam(v, scen_nodams = F)
  
  
  ret <- data.frame(damID = tni[, 1], ret = ((tni[, 2:3] - tno[, 2:3]) / tni[, 2:3])*100)
  names(ret)[2:3] = names(tno)[2:3]

  
  ret = ret %>%left_join(dam_basin_match, by = "damID")
  
  
  colors_40_curated <- c(
    "#FF0000", "#0000FF", "#00FF00", "#FFD700", "#FF69B4", "#8A2BE2", "#00CED1", "#FF4500",
    "#32CD32", "#FF1493", "#1E90FF", "#ADFF2F", "#FF6347", "#4169E1", "#00FA9A", "#DC143C",
    "#7FFF00", "#9932CC", "#00BFFF", "#FFA500", "#98FB98", "#FF00FF", "#87CEEB", "#F0E68C",
    "#DDA0DD", "#90EE90", "#FFB6C1", "#20B2AA", "#F5DEB3", "#87CEFA", "#D2691E", "#B0E0E6",
    "#FFEFD5", "#C0C0C0", "#F4A460", "#2E8B57", "#DAA520", "#6B8E23", "#BC8F8F", "#4682B4"
  )
  # names(colors_40_curated) = sort(unique(yo$basin))
  
  bn <- sort(unique(ret$basin))
  basin_colors <- setNames(colors_40_curated[1:length(bn)], bn)
  
  
  
  
  ggplot(ret,  aes(x =`scenario -1`, y = `scenario 0` , color = basin))+
    
    
    geom_point(size=3)+theme_bw() +
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_color_manual(values = basin_colors, na.value = "lightgrey")+
    scale_x_continuous(limits = c(-1,2))+scale_y_continuous(limits = c(-62,100))+
    xlab("without dams (%)") + ylab("baseline / with dams (%)")
 
  
   
  
  #TP retention
  v =  pull_scen_var(vars = "TP_inflow",stat = sum, resolution = "dam")
  tni = df_dam(v, scen_nodams = F)
  
  v =  pull_scen_var(vars = "TP_Rs_r",stat = sum, resolution = "dam")
  tno = df_dam(v, scen_nodams = F)
  
  
  ret <- data.frame(damID = tni[, 1], ret = ((tni[, 2:3] - tno[, 2:3]) / tni[, 2:3])*100)
  names(ret)[2:3] = names(tno)[2:3]
  
  
  ret = ret %>%left_join(dam_basin_match, by = "damID")
  
  
  colors_40_curated <- c(
    "#FF0000", "#0000FF", "#00FF00", "#FFD700", "#FF69B4", "#8A2BE2", "#00CED1", "#FF4500",
    "#32CD32", "#FF1493", "#1E90FF", "#ADFF2F", "#FF6347", "#4169E1", "#00FA9A", "#DC143C",
    "#7FFF00", "#9932CC", "#00BFFF", "#FFA500", "#98FB98", "#FF00FF", "#87CEEB", "#F0E68C",
    "#DDA0DD", "#90EE90", "#FFB6C1", "#20B2AA", "#F5DEB3", "#87CEFA", "#D2691E", "#B0E0E6",
    "#FFEFD5", "#C0C0C0", "#F4A460", "#2E8B57", "#DAA520", "#6B8E23", "#BC8F8F", "#4682B4"
  )
  # names(colors_40_curated) = sort(unique(yo$basin))
  
  bn <- sort(unique(ret$basin))
  basin_colors <- setNames(colors_40_curated[1:length(bn)], bn)
  
  
  
  
  ggplot(ret,  aes(x =`scenario -1`, y = `scenario 0` , color = basin))+
    
    geom_point(size=3)+theme_bw() +
    
    ## add abline if needed
    geom_abline(slope = 1, intercept = 0)+
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size =12),#,angle = 45, vjust = 1, hjust=1),
          axis.title = element_text(size = 16),
          legend.position = "none",
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))+
    scale_color_manual(values = basin_colors, na.value = "lightgrey")+
    scale_x_continuous(limits = c(-1,2))+scale_y_continuous(limits = c(-62,100))+
        xlab("without dams (%)") + ylab("baseline / with dams (%)")
#######################

















  
  
  
  
  
  
  
  
  



v =  pull_scen_var(vars = "TP_Rs_r",stat = sum, resolution = "dam")
tpo = df_dam(v)
tpo = pct_vs_scenario0(tpo, suffix = "_tno")


v =  pull_scen_var(vars = "TP_inflow",stat = sum, resolution = "dam")
tpi = df_dam(v)  #tpi = df_dam(v,scen_nodams = F)
tpi = pct_vs_scenario0(tpi, suffix = "_tni")

v =  pull_scen_var(vars = "TP_Rs_r",stat = mean, resolution = "dam")
tno = df_dam(v) # tno = df_dam(v,scen_nodams = F)
tno = pct_vs_scenario0(tno, suffix = "_tno")


## Residence time = median(S_Rs_r)/sum(Q_Rs_r) 
v =  pull_scen_var(vars = "S_Rs_r",stat = median, resolution = "dam")

sr = df_dam(v)

RT = sr[,2:9]/(qr[,2:9])
RT = RT*3285
RT[RT==0] = NA

RT = ((RT[,2:8]-RT[,1])/RT[,1])*100
RT[is.na(RT)] = 0


RT$damID = qr$damID
## merge with other variables
yo = RT  %>% left_join(den, by = "damID")%>%left_join(bur, by ="damID")

## analyses of dam effects scenario 0 vs. -1
v =  pull_scen_var(vars = "fixation",stat = mean, resolution = "dam")
c = df_dam(v, scen_nodams = F)
c = c[,1:3]
c = ((c[,3]-c[,2])/c[,2
])*100

yo = RT %>% left_join(dam_basin_match, by = "damID")

#bur+den-fix 
yo = yo %>% mutate(scen1 = `scenario -1_bur`+ `scenario -1_den` - `scenario -1_fix`)
yo = yo %>% mutate(scen0 = `scenario 0_bur`+ `scenario 0_den` - `scenario 0_fix`)

yo = yo %>% select(c(damID,scen1,scen0))

## TN:TP sometimes translation to molar weights is needed
v =  pull_scen_var(vars = "fixation",stat = sum, resolution = "estuary")
tp = df_estuary(v, scen_nodams = F)
tp = tp[,1:2]
tp$id = as.numeric(rownames(tp))
tp = tp %>% left_join(basin_id_match, by = "id")

v =  pull_scen_var(vars = "TN_inflow",stat = mean, resolution = "estuary")
tn = df_estuary(v)
# tn = ((tn[,2:8]-tn[,1])/tn[,1])*100

tn = tn  / 14.01 #standard atomic weights
tp = tp  / 30.97

b = tn/tp
b$basin = rownames(b)


yo = b #%>% left_join(dam_basin_match, by = "basin")

b = ((b[,2:8]-b[,1])/b[,1])*100
b$basin = rownames(b)

## dry and wet season comparison 
v =  pull_scen_var(vars = "denitrification",stat = mean, resolution = "dam", dry_vs_wet = "wet")
tno =  df_dam(v)

tnow = pct_vs_scenario0(tno, suffix = "_tno_wet")
#absolute difference
# tno <- (a[,3:9]-a[,2])
# names(tno)[1:7] = paste0(names(tno)[1:7],"_tno_wet")
# tno$damID = a$damID


yo = tnod %>% left_join(tnow, by = "damID") 


## simple bar plot  

#subset to estuary groups
# b2 = b %>% filter(basin %in% trans$basin)

#melty
l <- melt(b)

ggplot(l) + geom_boxplot(aes(x=variable, y=value,fill=variable, color = variable))+
  ggtitle(expression("Change in TN:TP flowing into estuary, compared to scenario 0; Transition (16-20) estuaries (n = 10)"))  +
  theme_bw()+theme(axis.title.y=element_text(size=25),axis.title.x=element_blank(), plot.title = element_text(size=15),
                   axis.text = element_text(size=18),legend.position = "none")+
  ylab("% change")#+scale_y_continuous( limits = c(-1,2.5))

# #1000x650 