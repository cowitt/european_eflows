# source main_analysis functions first

##### Find the largest dam/biggest inflow
v =  pull_scen_var(vars = "inflow",stat = sum, resolution = "dam")
a = df_dam(v, scen_nodams = F)
inflow = a[,1:2]
top4 = inflow  %>% arrange(desc(`scenario -1`))%>%slice(1:4)
dam_basin_match %>% filter(damID %in% top4$damID)
dam_chosen = 4629# (volga)
# dam_chosen = 2664

##ENVIRONMENTAL FLOW
  setwd("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/EMP/eflow_scenarios")
  list.files() #is the right order

  all_scen = list.files(pattern = ".csv");all_scen
  if(length(all_scen)==7) {
   alles = list()
  
   for (i in 1:7) {
     sc = all_scen[i]
     sc_read = read.csv(sc,header=TRUE,stringsAsFactors=FALSE,nrows =500)#set nrows = 12, enough for Volga
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


## PREPARE VARIABLE PULL
 setwd("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/") #adapt path here


# load scenarios into list called scena RUN ONCE
 scena = list()
 for(scen in c(-1,0,1,2,3,4,5,6,7)){
   print(scen);gc()
   scena[[paste0("scenario ",scen)]] = readRDS(paste("results_scen/sim_all_boxm_seasT_resOP_scen",scen,".RDS",sep=""))
 }

#we choose Volga for now
 bas = "Volga"
 # bas = "Ebro"
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
 sim <- readRDS("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/EMP/eflow_scenarios/sim_all_boxm_noRes2.RDS")#same as scenario -1
 
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
 ymax = max(all_values, na.rm = TRUE)+0.5
 
 
 col_val = c("#7E7CF5",  "#136F63" ,"#241E4E", "coral")
 names(col_val) = c("eflow","natural flow","outflow","inflow")
 
 for(eflowsc in names(eflows)){ #fix loop later and fill plot list
  
 eflo = as.numeric(eflows[,eflowsc])
 outflo = outflow[,eflowsc]
 inflo = inflow[[eflowsc]]
 
 topl = cbind(eflo,  nat_flow) %>% as.data.frame()

#date column, starting in winter...
 topl$date <- as.Date("1989-12-21") + (seq_len(nrow(topl)) - 1)
 topl = topl[-c(1:365),]

 dat = melt_for_ggplot_tidy(topl)
 dat = dat %>% mutate(series = replace(series, series ==eflowsc,"outflow"))%>%
   mutate(series = replace(series, series =="inflo","inflow"))%>%
   mutate(series = replace(series, series =="eflo","eflow"))%>%
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
 grid.arrange(grobs = pl, layout_matrix = m)  #1000x500 with 9, 
 