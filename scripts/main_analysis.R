##############################################################################
## Project: unnamed Euroflow modelling paper - main analyses and prep for figures
## Capabilities: Open, analyse, write model outputs 
## Date created:  4.11.2021
## author: cordula.wittekind@ufz.de
##############################################################################

library(foreign);library(dplyr);library(reshape);library(ggplot2);library(tidyverse);library(gridExtra);library(ggpubr);library(ncdf4)
library(matrixStats) #for looking at ranks
source("scripts/filter_catchment.R") # function to filter reservoir IDs for selected river basins

script_dir <- dirname(normalizePath(sys.frames()[[1]]$ofile))
setwd(file.path(script_dir, "..","data")) 

# load scenarios into list called scena RUN ONCE
scena = list()
for(scen in c(-1,0,1,2,3,4,5,6,7)){
  print(scen);gc()
  scena[[paste0("scenario ",scen)]] = readRDS(paste("results_scen/sim_all_boxm_seasT_resOP_scen",scen,".RDS",sep=""))
}

#load required input

 ##routing scheme
  tab <- read.csv("input/routing_tab_v5.csv",header=TRUE,stringsAsFactors=FALSE) #routing scheme w/ average residence times in rivers, dams and estuaries, maximum storage capacity and year of dam erection
  
 ##basin names in Europe (basin/estuaryID is damID + 100000 and then highest number across all dams feeding into estuary)
  dams_analysed <- read.csv("input/grand_ID_dam_basin_euroflow.csv",as.is=T)
  
  probablyremoved = read.csv("results_scen/Dams_with_unrealistic_Qsim.csv",header=TRUE, stringsAsFactors = F) #>>>> CORDI <<<<# I am removing those that were defined as having unrealistic Qsim
  dams_analysed = dams_analysed[-which(dams_analysed$grand_ID %in% probablyremoved$GRAND_ID),]
  
  NEWS_2000a <- read.csv("input/c00run5_NEWSInputOutput.csv", as.is=T) # GlobalNEWS database to get basin names and reference nutrient loads
  basins <- NEWS_2000a$BASINID[NEWS_2000a$continent=='Europe' & substr(NEWS_2000a$basinname,1,5)!='GHAAS']+100000 #all European basins
  basin_names <- NEWS_2000a$basinname[which(NEWS_2000a$BASINID%in%(basins-100000))]
  basin_names[c(86,100)] <- c("VENTA1","VENTA2") #make unique
  basin_id_match = data.frame(basin = basin_names, id = basins)
 ##original GRAND database
  GRAND_data <- read.dbf("input/GRanD_reservoirs_v1_3.dbf", as.is=T)
  
  # create daily matrix of monthly water demand
  # ndays <- rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),10)
  # period <- c(1:3650) #simulation period (in total 3650 days, from 1991 to 2000)
  # might need repairing, I don't know in which season the model starts (needed for box model version)
  # day_season <- rep(c(rep("win",59),rep("spr",92),rep("sum",92),rep("fal",92),rep("win",31)),10)
  # day_season<-day_season[period];rm(period)
  # day_season <- day_season[-c(1:365)]
  
  # wet and dry based on Q_Rs_r of scenario -1 / no reservoirs, does not require removal of spinup (set to 0)
  day_season = readRDS("input/wet_dry_season.RDS")

  ## variables we've modelled
    names(scena[[1]]$Volga)
 
  
  
 #### eflow scenarios
  # 0 = none: eflow = 0
  # 1 = constant: Q90 # need to fix length of the dataset, do calculation again!!
  # 2 = variable on monthly basis: VMF (Pastor et al., 2014)
  # 3 = variable on monthly basis: Tessman, 1980
  # 4 = variable on monthly basis: Smakhtin et al., 2004
  # 5 = variable on monthly basis: Tennant, 1976
  # 6 = variable on monthly basis: Q90-QQ50 (Pastor et al., 2014)
  
  #not looking at these anymore (don't know when that was decided)
  # 7 = proportional to input: VMF (Pastor et al., 2014)
  # 8 = proportional to input: Tessman, 1980
  # 9 = proportional to input: Smakhtin et al., 2004
	# 10 = proportional to input: Tennant, 1976
	# 11 = proportional to input: Q90-QQ50 (Pastor et al., 2014)
   
  
  ###  Several scenarios across dams (as opposed to looking at individual dams) ####
  
    pull_scen_var = function(vars = "S_Rs_r", #which variable shall be pulled, 
                             # options: 
                             # "S_Rs_r" - water storage
                             # nutrient inflow
                             # "TP_inflow" 
                             # "TN_inflow"
                             # nutrient outflow from reservoir
                             # "TP_Rs_r"  #(TP_Rs<-S_P_Rs/S_Rs_old*Q_Rs)
                             # "TN_Rs_r"
                             # "S_N_Rs_r"
                             #fixation
                             # "F_Nfix_Rs_r"
                             # "F_Nfix_dn_r"
                             # "F_Nfix_up_r"
                             # "fixation" ==> to run all fixation processes
                             #denitrification
                             # "F_Nd_Rs_r"
                             # "F_Nd_dn_r"
                             # "F_Nd_up_r"
                             # "denitrification" ==> to run all denitrification processes
                             #release
                             # "Q_Rs_r"
                             # "Q_down_r"
                             #burial
                             # "F_Nb_Rs_r"
                             # "F_Pb_Rs_r"
                             
                             
                             stat = mean, #which statistic shall be performed, options: sum, mean, median
                             resolution = "dam", #output resolution, options: "dam", "estuary", "plot" 
                             dry_vs_wet = FALSE) #shall dry_vs_wet be analysed, options: FALSE, wet, dry, other values imply 0 which includes the spinup period
    
    
    {
      v = list()
      basin_list = list()
      #check if dry/wet distinction needed
      if(dry_vs_wet != FALSE){
        seas = ifelse(dry_vs_wet == "wet", 2, ifelse(dry_vs_wet == "dry", 1, 0))
      }
      
      #main loop
      for(basinX in rev(basin_names)){ #mask of basins/dams where eflow applies would start/subset here
        # basinX = rev(basin_names)[97];basinX
        scens = list()#fill a list with all scenarios subset to basin
        for(eflowscen in names(scena)){ 
          sim = scena[[eflowscen]]
          sim = sim[[basinX]] #subset scenario to basin
          scens[[eflowscen]] = sim  
        }
        
        if(length(scens)==0){next}  #remove empty scenarios SHOULD NOT BE NEEDED
      
        basin_idx <- which(basin_names == basinX) #which number in European basin (?)
        reservoirs_basinX <- left_join(filter_catchment(tab, basins[basin_idx])[-1,-c(2,3)],
                                       GRAND_data[,c(1,3,5)], by="GRAND_ID")
        
        # print(paste0("Basin = ", basinX))
        # print(any(reservoirs_basinX$GRAND_ID>100000))}
        # print(reservoirs_basinX$DAM_NAME)
        if(length(reservoirs_basinX$DOWNSTREAM_ID)==0){next}
        # basin_list[[basinX]] <- reservoirs_basinX$DAM_NAME
        
        
        #only take those we analyse  
        reservoirs_basinX <-  reservoirs_basinX[(reservoirs_basinX$GRAND_ID %in% dams_analysed$grand_ID), ]
        
        #sort dams by size
        res_sort <- reservoirs_basinX[order(reservoirs_basinX$CATCH_SKM), ]
        dams_bas <- res_sort$GRAND_ID
        
        #empty dataframe w/ scenarios as columns
        ind_bas <- as.data.frame(array(NA, dim = c(length(dams_bas)+1, length(scens) +
                                                     1)))  #looking at x scnearios (+1 for damID)
        names(ind_bas) <- c("damID", names(scens))
        
        #get estuary id and add it
        downid <- res_sort$DOWNSTREAM_ID %>% unique()
        down = downid[which(downid>100000)]
        
        ind_bas$damID <- c(dams_bas,down)
        
        
        no_spinup <- if (dry_vs_wet == FALSE) 366:3650 else  NULL
        
        dams_char = as.character(ind_bas$damID)
        
      for (sc in names(scens)) {
        
        onescen = scens[[sc]]
        
        if (vars == "denitrification") {
          allres <- onescen[["F_Nd_Rs_r"]]
          allres2 <- onescen[["F_Nd_dn_r"]]
          allres3 <- onescen[["F_Nd_up_r"]]
          several_needed <- TRUE
        } else if (vars == "fixation") {
          allres <- onescen[["F_Nfix_Rs_r"]]
          allres2 <- onescen[["F_Nfix_dn_r"]]
          allres3 <- onescen[["F_Nfix_up_r"]]
          several_needed <- TRUE
        } else {
          allres <- onescen[[vars]]
          several_needed <- FALSE
        }
        
        
        # stat(<variable>[Res,<timeframe>])
        
        if (dry_vs_wet != FALSE) {#main loop for filling
         if(several_needed){
           vp = sapply(dams_char, function(res){
             ids = which(day_season[res,] == seas)
             stat(allres[res, ids]) + stat(allres2[res, ids]) + stat(allres3[res, ids] )
           })
         }else{
           vp = sapply(dams_char, function(res){
           ids = which(day_season[res,] == seas)
           stat(allres[res, ids])
           })
         }
        }else{
          if(several_needed){
            vp = sapply(dams_char, function(res){
              stat(allres[res, no_spinup])+
                stat(allres2[res, no_spinup])+
                stat(allres3[res, no_spinup])
            })
          }else{
            vp = sapply(dams_char, function(res){stat(allres[res, no_spinup])})
          }
        }
        
        dam_indic = match(dams_char, ind_bas$damID) #ind_bas$damID == Res 
        ind_bas[dam_indic, sc] = vp #pulled values
        
      }  
        
          # MISSING: divide by catchment size if needed using /reservoirs_basinX[reservoirs_basinX$GRAND_ID==Res,"CATCH_SKM"]
          # ind_bas[ind_bas$damID == Res, sc] = median(allres[Res, 366:3650])#/reservoirs_basinX[reservoirs_basinX$GRAND_ID==Res,"CAP_MCM.y"]# ,"CATCH_SKM"

     
      ### fill with the right subset
      if(resolution == "dam"){        #Individual Dams

        ind_bas <- ind_bas[-which(ind_bas$damID == down),] #for dam values and plotting remove downstream/estuary
        v[[paste0(basinX,vars,"_")]] <- ind_bas
        
      }else if(resolution == "estuary"){#Estuary
        # lowerdams <- ind_bas$damID[which(ind_bas$down>100000)] #damIDs of those basins adjacent to estuary
        # estuary <- colSums(ind_bas[which(ind_bas$damID %in% lowerdams),names(scena)]) #fill estuary with sum of basins filling it
        # denn wieso einfach wenn es auch kompliziert geht go brr brr
        
        v[[paste0(basinX,"_", vars,"_",down)]]<- ind_bas[which(ind_bas$damID == down),names(scena)]
      }
    
      }
      return(v)
      }

## convert to dataframe/get the right subset
    
    df_dam = function(lisa = v, scen_nodams = T){#no dams only needed for first analyses
      a = as.data.frame(array(NA, c(0, 10)))
      names(a) <- names(lisa[[1]])
      for (i in 1:length(lisa)) {
        bas <- lisa[[i]]
        a <- rbind(a, bas)
      }
      if(scen_nodams){
      a = a %>% select(-`scenario -1`)
      }
      return(a)#this contains a column of damIDs
    }
    
        
    df_estuary = function(lisa = v, scen_nodams = T){
      a <- as.data.frame(array(NA,c(0,9)))
      names(a) <- names(lisa[[1]])
      
      # basn <- sub("\\_.*", "", names(lisa)) # before: \\_.*
      basn <- sub(".*_", "", names(lisa))
      k = length(basn)
      
      for(i in 1:k){
        bas <- lisa[[i]]
        a[i,] <- bas
        rownames(a)[i] <- basn[i]
      }
      if(scen_nodams){
        a = a %>% select(-`scenario -1`)
      }
      return(a)#does not contain a column of damIDs, rownames are basin names
    }
    
## convert to percentage of scenario 0
    pct_vs_scenario0 = function(a, suffix = "_new_data"){
      
      if(ncol(a)!= 9 | nrow(a) != 768)return(NULL)
      if(names(a)[2] != "scenario 0")return(NULL)
      
      a[a == 0] <- NA 
      
      b <- ((a[,3:9]-a[,2])/a[,2])*100
      
      b[is.na(b)] = 0
      names(b)[1:7] = paste0(names(b)[1:7],suffix)
      
      b$damID = a$damID
      return(b)
    }
    
#### get dams belonging to basin/estuary ####
    dam_basin_match = data.frame()
    for(basinX in rev(basin_names)){ #mask of basins/dams
      
      basin_idx <- which(basin_names == basinX) #which number in European basin (?)
      reservoirs_basinX <- left_join(filter_catchment(tab, basins[basin_idx])[-1,-c(2,3)],#get all dams belonging to this basin
                                     GRAND_data[,c(1,3,5)], by="GRAND_ID")
      
      res = reservoirs_basinX$GRAND_ID
      res = data.frame(damID = res, basin = rep(basinX,length(res)))
      
      dam_basin_match = rbind(dam_basin_match,res)
    }    