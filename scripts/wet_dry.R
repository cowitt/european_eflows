######################## SERVER ####################################
# Project: unnamed Euroflow modelling paper - Define dry & wet seasons for dams
# comments: creates wet_dry_season.RDS: 0 = "normal"/spin-up, 1 = dry, 2 = wet
# input: scenario -1 /sim_all_boxm_noRes.RDS = sim_all_boxm_seasT_resOP_scen-1
# author: cordula.wittekind@ufz.de
####################################################################
rm(list=ls())
gc()

library(ncdf4)
library(dplyr)
library(tibble)

setwd("C:/Users/wittekin/Documents/cle2024/projects/EMP/model") #adapt path here
nores = readRDS(paste("no_res/sim_all_boxm_noRes.RDS"))#scenario -1

# define period (10 years with in total 3650 days) 
period <- c(1:3650) #>>>> MICHA <<<<#

wd = matrix(ncol = 3650, nrow = 0)

for(i in 1:length(nores)){#loop through basins
  basin = nores[[i]]$Q_Rs_r
  if(!is.null(ncol(basin))){ #some are not filled/have not been modelled
  wd = rbind(wd, basin)}
}

wd = wd[, 366:3650]# removed spinup but we'll add it back below

# calculate thresholds, rowwise
dry_t = apply(wd, 1, function(row) quantile(row,0.4))
  wet_t = apply(wd, 1, function(row) quantile(row,0.8)) # alternative ==> 0.8 annual flow


# comparison matrix
dry_matrix = matrix(rep(dry_t, ncol(wd)), nrow = nrow(wd), byrow = F)
  wet_matrix = matrix(rep(wet_t, ncol(wd)), nrow = nrow(wd), byrow = F)
  
dwn = (wd < dry_matrix) * 1 + (wd > wet_matrix) * 2

# add spinup back
dwn <- cbind(matrix(0, nrow = nrow(dwn), ncol = 365), dwn)

saveRDS(dwn,"wet_dry_season.RDS")
