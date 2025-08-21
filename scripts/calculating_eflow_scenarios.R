################################################################################
#### Pre-processing for eflow scenarios: Calculating inflow statistics and eflow scenarios####

setwd("Y:/Gruppen/cle/MichaS/Hatem/EuroFLOW_Paper/Group3/Task_3p3") #Change this path to the folder wher you have all your data in

library(dplyr)
library(lubridate)
library(ggplot2)

# load simulation results for naturalized flow
sim <- readRDS("sim_all_boxmodel_seasT.RDS")

# extract inflows (so far only European rivers were modelled)
inflow_ <- sim[[1]]$inflow
#Elbe_inflow <- sim$Elbe$inflow
#row.names(Elbe_inflow)

for(i in 2:length(sim)){
  inflow <- rbind.data.frame(inflow_,sim[[i]]$inflow)
  inflow_ <- inflow
}

## exclude 1st two years for filling up storages
period_ <- c(731:3650)

# create a date vector
ts_date <- seq(from = as.Date("1993-01-01"), to = as.Date("2000-12-31"), by = 1)
# remove leap years
ts_date <- data.frame("Date" = ts_date[-which(ts_date == "1996-02-29" | ts_date == "2000-02-29")])

# separate year, month and day vectors
ts_date <- ts_date%>%
  mutate(year= year(Date), 
         month=month(Date),
         day=day(Date))

period <- c(1:3650)
q_tt <- cbind.data.frame(ts_date, t(inflow[,period_]))

## calculate statistics

# mean monthly flow values
MMF_tt_ <- aggregate.data.frame(q_tt[,c(5:length(q_tt))], by=list(paste(q_tt$year,q_tt$month,sep="_")),FUN=mean)
# separate year and month vectors
MMF_tt_$year <- unlist(strsplit(MMF_tt_$Group.1, "_"))[c(TRUE, FALSE)]
MMF_tt_$month <- unlist(strsplit(MMF_tt_$Group.1, "_"))[c(FALSE, TRUE)]
# rearrange columns for better handling
MMF_tt_ <- MMF_tt_[,c(length(MMF_tt_),(length(MMF_tt_)-1),1:(length(MMF_tt_)-2))]

# average monthly means (MMF)
MMF_ <- aggregate.data.frame(MMF_tt_[,c(4:length(MMF_tt_))], by=list(MMF_tt_$month),FUN=mean)
# add reasonable names
row.names(MMF_) <- paste("MMF",MMF_$Group.1,sep="_")
# transpose matrix
MMF_df <- t(MMF_[-1])
MMF_df <- as.data.frame(MMF_df[,c(1,5:12,2:4)])

# total average (MAF) 
MAF_df <- data.frame(MAF = colMeans(MMF_[,c(2:length(MMF_))]))

# quantiles
quantiles <- as.data.frame(t(as.data.frame(lapply(q_tt[,c(5:length(q_tt))], quantile, probs=c(.03,.1,.5), na.rm=TRUE))))
names(quantiles) <- c("q97","q90","q50")
q97 <- quantiles$q97
q90 <- quantiles$q90
q50 <- quantiles$q50

#### Calculating eflow scenarios ####
setwd("F:/mstrauch/EuroFLOW/Inputs/eflow_scenarios") #Change this path to the folder wher you have all your data in

#i_efty <- 3 #Choose between 1-3 for the type of eflow method
#  Eflow_Type (variable name: i_efty)
#1: constant whole year
#2: variable on monthly basis
#3: proportional to input

#i_mvef <- 5 #Choose between 1-5 scenarios
#1: VMF, 2014
#2: Tessman, 1980
#3: Smakhtin et al., 2004
#4: Tennant, 1976
#5: Q90-QQ50, 2014

#function for variable eflow on a monthly basis
var_eflow_monthly <- function(i_mvef, MMF, MAF) {
  if (i_mvef==1){         # --------------- VMF (2014)
    for(k in 1:dim(qefm)[1]){
      for (i in 1:ncol(MMF_df)) {
        if (MMF_df[k,i] <= 0.4*MAF_df[k,]) {
          qefm[k,i] <- 0.6*MMF_df[k,i]
          LIHflo[k,i] <- 1    #i denotes a low flow month
        } else if(MMF_df[k,i] > 0.4*MAF_df[k,] && MMF_df[k,i] <= 0.8*MAF_df[k,]) {
          qefm[k,i] <- 0.45*MMF_df[k,i] #SK:I added as.numeric() here, otherwise it produces a list of different classes. There might be a more efficient solution like structruing the lists from the begining, but for now this works ok. 
          LIHflo[k,i] <- 2    #i denotes an intermediate flow month
        } else {
          qefm[k,i] <-0.3*MMF_df[k,i]
          LIHflo[k,i] <- 3 #i denotes a high flow month
        }
      }
    }
  } 
  else if (i_mvef==2){ # --------------- Tessmann (1980)
    for(k in 1:dim(qefm)[1]){
      for (i in 1:ncol(MMF_df)) {
        if (MMF_df[k,i] <= 0.4*MAF_df[k,]) {
          qefm[k,i] <- MMF_df[k,i] #sk:see note above
          LIHflo[k,i] <- 1 #i denotes a low flow month
        } else if(MMF_df[k,i] > 0.4*MAF_df[k,] && 0.4*MMF_df[k,i] <= 0.4*MAF_df[k,]) {
          qefm[k,i] <- 0.4*MAF_df[k,]
          LIHflo[k,i] <- 2 #i denotes an intermediate flow month
        } else {
          qefm[k,i] <- 0.4*MMF_df[k,i]
          LIHflo[k,i] <- 3 #i denotes a high flow month
        }
      }
    }
  } 
  else if (i_mvef==3) { #--------------- Smakhtin et al. (2004b)
    for(k in 1:dim(qefm)[1]){
      for (i in 1:ncol(MMF_df)) {
        if (MMF_df[k,i] <= MAF_df[k,]) {
          qefm[k,i] <- q90[k] #sk:also for quantile data, instead of as.numeric, I just indicated the data to extract, need to test later on
          LIHflo[k,i] <- 1 #i denotes a low flow month
        } else {
          qefm[k,i] <- 0.2*MAF_df[k,] # check this rule
          LIHflo[k,i] <- 3 #i denotes a high flow month
        }
      }
    }
  } 
  else if (i_mvef==4) { #--------------- Tennant (1976)
    for(k in 1:dim(qefm)[1]){
      for (i in 1:ncol(MMF_df)) {
        if (MMF_df[k,i] <= MAF_df[k,]) {
          qefm[k,i] <- 0.2*MAF_df[k,]
          LIHflo[k,i] <- 1 #i denotes a low flow month
        } else {
          qefm[k,i] <- 0.4*MAF_df[k,]
          LIHflo[k,i] <- 3 #i denotes a high flow month
        }
      }
    }
  } 
  else if (i_mvef==5) { #--------------- Q90-Q50 (2014)
    for(k in 1:dim(qefm)[1]){
      for (i in 1:ncol(MMF_df)) {
        if (MMF_df[k,i] <= MAF_df[k,]) {
          qefm[k,i] <-q90[k]
          LIHflo[k,i] <- 1 #i denotes a low flow month
        } else {
          qefm[k,i] <- q50[k]
          LIHflo[k,i] <- 3 #i denotes a high flow month
        }
      }
    }
  }
  return(qefm)
}

### calculate all 11 eflow scenarios and store them in a separate file
ny <- 10                               #number of years in the dataset
# eflow type 1: constant (here q90) throughout whole period
period <- c(1:3650)
q_tt_ <- as.data.frame(t(inflow[,period]))

qef_efty1 <- as.data.frame(t(q_tt_))

for(k in 1:dim(qef_efty1)[1]){
  qef_efty1[k,] <- q90[k]
}

write.csv(qef_efty1, "qef_efty1.csv")

# eflow types 2 and 3 with all five methods to derive monthly variable eflows

for(i_efty in 2:3){
  for(i_mvef in 1:5){
    
    qefm <- MMF_df
    qefm[,] <- NA
    LIHflo <- MMF_df
    LIHflo[,] <- NA
    
    ##################################################################################################################
    #DP:modifications made to include case3
    thresholdQ <- MAF_df #assumption.User defined parameter
    qprop <- 0.5 #assumption.User defined parameter
    qef <- data.frame(matrix(data=NA, nrow=dim(MAF_df)[1], ncol=3650))
    row.names(qef) <- row.names(MAF_df)
    
    # case1-2 calculating qefm (monthly flow)
    if (i_efty==1){ #case 1:constant eflow throughout the whole year
      
      for(j in 1:dim(qefm)[1]){
        qefm[j,] <- q90[j]
      }
      
    } else if (i_efty==2){#case 2: variable eflow on a monthly basis
      qefm <- var_eflow_monthly(i_mvef=i_mvef, MMF_df, MAF_df) 
    } 
    
    #deriving daily low flows for case 3
    if (i_efty==3){ #case 1:constant eflow throughout the whole year
      qefm3 <- var_eflow_monthly(i_mvef=i_mvef, MMF_df, MAF_df)
      ndays3 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      qefy3 <- matrix(data=NA, nrow=dim(MAF_df)[1], ncol=365)
      nsum3 <- 0
      for (i in seq_along(ndays3)){
        for(k in 1:dim(qefm)[1]){
          qefy3[k,(nsum3+1):(nsum3+(ndays3[i]))] <- rep(qefm3[k,i], ndays3[i])
        }
        nsum3 <- ndays3[i]+nsum3
      }
      
      qef_case3 <- data.frame(matrix(data=NA, nrow=dim(MAF_df)[1], ncol=3650))
      ny <- 10
      for(k in 1:dim(qefm)[1]){
        qef_case3[k,] <- rep(qefy3[k,], ny)
      }
    }
    
    #case 1-3 calculating qef (daily flow)
    period <- c(1:3650) #now use full period
    
    # create a date vector
    ts_date <- seq(from = as.Date("1991-01-01"), to = as.Date("2000-12-31"), by = 1)
    # remove leap years
    ts_date <- data.frame("Date" = ts_date[-which(ts_date == "1992-02-29" | ts_date == "1996-02-29" | ts_date == "2000-02-29")])
    
    # separate year, month and day vectors
    ts_date <- ts_date%>%
      mutate(year= year(Date), 
             month=month(Date),
             day=day(Date))
    
    q_tt <- cbind.data.frame(ts_date, t(inflow[,period]))
    
    if (i_efty==1 | i_efty==2){
      #Conversion of monthly data to daily data
      ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      qefy <- matrix(data=NA, nrow=dim(MAF_df)[1], ncol=365)
      
      for(k in 1:dim(qefm)[1]){
        nsum <- 0
        for (i in seq_along(ndays)){
           qefy[k,(nsum+1):(nsum+(ndays[i]))] <- rep(qefm[k,i], ndays[i])
           nsum <- ndays[i]+nsum
        }
        qef[k,] <- rep(qefy[k,], ny)
      }

      
    } else { #case 3 = proportional flow
      for (i in period){
        for(k in 1:dim(qefm)[1]){
          if (q_tt[i,4+k] <= thresholdQ[k,]){
            qef[k,i] <- qef_case3[k,i]
          } else{
            qef[k,i] <- qprop*(q_tt[i,4+k] - thresholdQ[k,])+qef_case3[k,i]
          }
        }
      }
    }
    write.csv(qef, paste("qef_efty",i_efty,"_mvef",i_mvef,".csv",sep=""))
  }
}


