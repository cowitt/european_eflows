# Function to route water and nutrients (including vertical nutrient fluxes)

routing <- function(GRAND_ID){
  # filter for European river basins
  tab_ <- filter_catchment(tab, GRAND_ID)

  # subsets for respective river basin
  qtot_ <- qtot[G_ID%in%tab_$GRAND_ID,]
  TNtot_ <- TNtot[G_ID%in%tab_$GRAND_ID,]
  TPtot_ <- TPtot[G_ID%in%tab_$GRAND_ID,]
  Tw_up_ <- Tw_up[G_ID%in%tab_$GRAND_ID,]
  Tw_dam_ <- Tw_dam[G_ID%in%tab_$GRAND_ID,]
  Tw_dn_ <- Tw_dn[G_ID%in%tab_$GRAND_ID,]
  qef_ <- qef[row.names(qef)%in%tab_$GRAND_ID,]
  WD_up_ <- WD_up[G_ID%in%tab_$GRAND_ID,]
  WD_Rs_ <- WD_Rs[G_ID%in%tab_$GRAND_ID,]

  ### create arrays to store the model outputs
  #for water
  S_up_r<-qtot_*0.0 #water storgae in stream network of undammed catchment above the dam
  S_Rs_r<-qtot_*0.0	#water storgae in reservoir or estuary
  S_dn_r<-qtot_*0.0  #water storgae in river downstream of the dam which may flow into another dam or estuary
  Q_up_r<-qtot_*0.0  #water outflow from stream network of undammed catchment above the dam
  Q_Rs_r<-qtot_*0.0  #water outflow from reservoir or estuary
  Q_dn_r<-qtot_*0.0  #water outflow from river downstream of the dam which may flow into another dam or estuary
  Q_updam_r<-qtot_*0.0 #water inflow from usptream dams
  Q_spill_r<-qtot_*0.0 #flow through spillways of reservoirs
  Q_ncons_r<-qtot_*0.0 #non-consumptive water use (here water used for hydro-power production
  Q_down_r<-qtot_*0.0 #qspill+max(qeflow, qnconsum)
  Q_cons_r<-qtot_*0.0 #consumptive water use (here only from reservoirs)

  #for TN
  S_N_up_r<-TNtot_*0.0 # TN storage in upstream stream network
  F_Nfix_up_r<-TNtot_*0.0 # fixed N in upstream stream network
  F_Nb_up_r<-TNtot_*0.0 # buried N in upstream stream network #NO BURIAL IN STREAMS AND RIVERS
  F_Nd_up_r<-TNtot_*0.0 # denitrified N in upstream stream network
  #F_Nbd_up_r<-TNtot_*0.0 # removed N in upstream stream network when using residence time approach (Chen et al., 2016)
  TN_up_r<-TNtot_*0.0 # TN outflow from upstream stream network
  TN_cons_up_r<-TNtot_*0.0 # TN outflow with upstream stream water demand
  S_N_Rs_r<-TNtot_*0.0 # TN storage in reservoir or estuary
  F_Nfix_Rs_r<-TNtot_*0.0 # fixed N in reservoir or estuary
  F_Nb_Rs_r<-TNtot_*0.0 # buried N in reservoir or estuary
  F_Nd_Rs_r<-TNtot_*0.0 # denitrified N in reservoir or estuary
  #F_Nbd_Rs_r<-TNtot_*0.0 # removed N in reservoir or estuary when using residence time approach (Chen et al., 2016)
  TN_Rs_r<-TNtot_*0.0 # TN outflow from reservoir or estuary
  TN_cons_Rs_r<-TNtot_*0.0 # TN outflow with dam water consumption
  S_N_dn_r<-TNtot_*0.0 # TN storage in river downstream of the dam
  F_Nfix_dn_r<-TNtot_*0.0 # fixed N in river downstream of the dam
  F_Nb_dn_r<-TNtot_*0.0 # buried N in river downstream of the dam
  F_Nd_dn_r<-TNtot_*0.0 # denitrified N in river downstream of the dam
  #F_Nbd_dn_r<-TNtot_*0.0 # removed N in river downstream of the dam when using residence time approach  (Chen et al., 2016)
  TN_dn_r<-TNtot_*0.0 # TN outflow from river downstream of the dam
  TN_updam_r<-TNtot_*0.0 # TN inflow from usptream dams

  #for TP
  S_P_up_r<-TPtot_*0.0 # TP storage in upstream stream network
  F_Pb_up_r<-TPtot_*0.0 # buried P in upstream stream network
  TP_up_r<-TPtot_*0.0 # TP outflow from upstream stream network
  TP_cons_up_r<-TPtot_*0.0 # TP outflow with upstream stream water demand
  S_P_Rs_r<-TPtot_*0.0 # TP storage in reservoir or estuary
  F_Pb_Rs_r<-TPtot_*0.0 # buried P in reservoir or estuary
  TP_Rs_r<-TPtot_*0.0 # TP outflow from reservoir or estuary
  TP_cons_Rs_r<-TPtot_*0.0 # TP outflow with dam water consumption
  F_Pb_dn_r<-TPtot_*0.0 # TP storage in river downstream of the dam
  S_P_dn_r<-TPtot_*0.0 # buried P in river downstream of the dam
  TP_dn_r<-TPtot_*0.0 # TP outflow from river downstream of the dam
  TP_updam_r<-TPtot_*0.0 # TP inflow from usptream dams

  #vector for unmet demand
  if(dim(tab_)[1]>1) {
    unmdem_r <- as.matrix(1:dim(qtot_)[1])*0
    row.names(unmdem_r) <- row.names(qtot_)
    unmdays_r <- as.matrix(1:dim(qtot_)[1])*0
    row.names(unmdays_r) <- row.names(qtot_)
  } else{
    unmdem_r <- as.matrix(0)
    unmdays_r <- as.matrix(0)
  }

  k=1 #counter for recording in subsets
  for (w in 1:max(tab_$WSL)) {
    # loop over "watershed levels", which facilitates the routing
    sel<-tab_[tab_$WSL==w,]
    for (i in 1:length(sel$GRAND_ID)) { # loop over single dams
      #go through each GRAND_ID and get the runoff from the undammed area over time
      g_id<-sel$GRAND_ID[i]
      g<-which(G_ID==g_id)
      q_und<-qtot[g,]*86400/1e6 #gives daily runoff in million cubic meters (unit consistent with storage volume) from undammed areas
      idx <- which(row.names(qtot_)==g_id)

      TN_und<-TNtot[g,]*86400 # convert seconds to day, but I doubt TNtot_ is in tons per sec, values are extremely high
      TP_und<-TPtot[g,]*86400 # convert seconds to day, but I doubt TPtot_ is in tons per sec, values are extremely high

      #read residence times and convert unit from years to days
      t_up<-sel$TAU_up_yrs[i]*365
      #t_Rs<-sel$TAU_RES_yrs_model[i]*365
      t_Rs<-1 #set to 1 to turn off reservoirs
      t_dn<-sel$TAU_down_yrs[i]*365

      #read reservoir operation inputs
      rvmax	<- sel$CAP_MCM.x[i]
      rvmin <- sel$CAP_MCM.x[i] * 0.001 # minimum assumed to be 0.1% of maximum
      damhei <- sel$DAM_HGT_M[i] #(m)
      power <- as.numeric(sel$Electric_Capacity_MW[i]) * 10^6 #(W)

      #initialize reservoir operation variables
      unmdem <- 0
      unmdays <- 0

      #initialize storages
      #this is done for each dam/estuary individually, and only at beginning of the simulation
      if(istor==1){ #use final scen0 storages
        #initialize the water storages
        S_up<-storage$S_up[which(storage$GRAND_ID==g_id)] #water storage in stream network of undammed catchment above the dam
        #S_Rs<-storage$S_Rs[which(storage$GRAND_ID==g_id)]	#water storage in reservoir or estuary
        S_Rs<-0	#set to 0 to turn off reservoirs
        S_dn<-storage$S_dn[which(storage$GRAND_ID==g_id)]  #water storage in river downstream of the dam which may flow into another dam or estuary

        #initialize the TN and TP storages # used 1e-24 to avoid NA generation due to division by zero
        # RONNY some of the variables here are indeed fluxes, not storages, they re thus initialized in the next for-loop
        S_N_up<-storage$S_N_up[which(storage$GRAND_ID==g_id)]
        #S_N_Rs<-storage$S_N_Rs[which(storage$GRAND_ID==g_id)]
        S_N_Rs<-0	#set to 0 to turn off reservoirs
        S_N_dn<-storage$S_N_dn[which(storage$GRAND_ID==g_id)]
        S_P_up<-storage$S_P_up[which(storage$GRAND_ID==g_id)]
        #S_P_Rs<-storage$S_P_Rs[which(storage$GRAND_ID==g_id)]
        S_P_Rs<-0
        S_P_dn<-storage$S_P_dn[which(storage$GRAND_ID==g_id)]
      }else{
        #initialize the water storages
        S_up<-1e-24 #water storage in stream network of undammed catchment above the dam
        S_Rs<-1e-24	#water storage in reservoir or estuary
        S_dn<-1e-24  #water storage in river downstream of the dam which may flow into another dam or estuary

        #initialize the TN and TP storages # used 1e-24 to avoid NA generation due to division by zero
        # RONNY some of the variables here are indeed fluxes, not storages, they re thus initialized in the next for-loop
        S_N_up<-1e-24
        S_N_Rs<-1e-24
        S_N_dn<-1e-24
        S_P_up<-1e-24
        S_P_Rs<-1e-24
        S_P_dn<-1e-24
      }

      for (t in period) { #loop over all the time steps
        # first we calculate the outflows from each water storage based on the water volume and the residence time
        #here, they are initialized, at the beginning of each time step
        Q_up<-0.0  #water outflow from stream network of undammed catchment above the dam
        Q_Rs<-0.0  #water outflow from reservoir or estuary
        Q_dn<-0.0  #water outflow from river downstream of the dam which may flow into another dam or estuary
        Q_updam<-0.0 #water inflow from usptream dams

        qspill <- 0
        qdown <- 0
        cons_up_flag <- 0
        cons_Rs_flag <- 0

        TN_up<-0.0
        TN_cons_up<-0.0
        TN_Rs<-0.0
        TN_cons_Rs<-0.0
        TN_dn<-0.0
        TN_updam<-0.0

        TP_up<-0.0
        TP_cons_up<-0.0
        TP_Rs<-0.0
        TP_cons_Rs<-0.0
        TP_dn<-0.0
        TP_updam<-0.0

        F_Nfix_up<-1e-24
        F_Nb_up<-1e-24
        F_Nd_up<-1e-24
        F_Nfix_Rs<-1e-24
        F_Nb_Rs<-1e-24
        F_Nd_Rs<-1e-24
        F_Nfix_dn<-1e-24
        F_Nb_dn<-1e-24
        F_Nd_dn<-1e-24
        F_Pb_up<-1e-24
        F_Pb_Rs<-1e-24
        F_Pb_dn<-1e-24

        #for reservoirs receiving from upstream reservoirs, assign Q_dn as inflow to the next downstream following dam Q_updam
        if (w>1 & t>1) {
          updams<-tab_$GRAND_ID[tab_$DOWNSTREAM_ID==g_id]
          #now we loop over that list to update Q_updam, TN_updam, and TP_updam
          for (u in 1:length(updams)) {
            Q_updam<-Q_updam+Q_dn_r[as.numeric(row.names(Q_dn_r))==updams[u],t-1]
            TN_updam<-TN_updam+TN_dn_r[as.numeric(row.names(TN_dn_r))==updams[u],t-1] #
            TP_updam<-TP_updam+TP_dn_r[as.numeric(row.names(TP_dn_r))==updams[u],t-1] #
          } #end for (u in 1:length(updams))
        }#end if t>1 % w>1

        #get consumptive water use
        qconsum_up<-WD_up[g,t] #consumptive water use in upstream area
        qconsum_Rs<-WD_Rs[g,t] #consumptive water use from reservoir

        # average discharge (m3/s) for power production #MS: 9810 - specific weight of water N/m?
        qnconsum <- 0.75*S_Rs/rvmax*power/(damhei*9810/86400*1e6)  # function of volume and electric capacity
        #qnconsum <- as.numeric(sel$DIS_AVG_LS.x[i])*0.8/1e9*86400 # 80% of mean discharge, as qnconsum values using power are too high

        #get environmental flow
        qeflow<-qef[row.names(qef)==g_id,t]

        ## undammed upstream river network
        if (S_up > qconsum_up){
          S_up <- S_up - qconsum_up # substract upstream water demand, #2nd update of storage
          cons_up_flag <- 1
        }
        if (t_up<=1) { # in most cases the water residence time is < 1 day, then we assume that whole S_up flows out each time-step
          Q_up<-S_up
        } else {
          Q_up<-S_up/t_up
        }
        #end if (t_up<=1)

        S_up<-S_up+q_und[t]-Q_up #2nd update of storage
        if(S_up<1e-24) S_up <- 1e-24

        #Calculating outflows of dams controlled by reservoir operations
        if (k < dim(tab_)[1]){ #if estuary (final WS level) is not yet achieved
          #if (is.na(damhei) | is.na(power) | is.na(rvmax) | g_id %in% GID_exclude){ #if data for reservoir OP is NOT given or if it is a problematic dam use residence time routing approach
            if (S_Rs > qconsum_Rs){
              S_Rs <- S_Rs - qconsum_Rs # substract water demand from dam
              cons_Rs_flag <- 1
            }
            if (t_Rs<=1) { # if residence time is < 1 day, then we assume that whole S_Rs flows out each time-step
              Q_Rs<-S_Rs
            } else {
              Q_Rs<-S_Rs/t_Rs
            }#end if (t_Rs<=1)
            S_Rs <- S_Rs+Q_up+Q_updam-Q_Rs #update storage
          # } else{
          #   S_Rs <- S_Rs + Q_up + Q_updam - max(qconsum_Rs+qeflow, max(qeflow,qnconsum))
          #   if (S_Rs >= rvmax){
          #     qspill <- S_Rs-rvmax
          #     qdown <- qspill + max(qconsum_Rs+qeflow, max(qeflow,qnconsum))
          #     S_Rs <- rvmax
          #     cons_Rs_flag <- 1
          #   }
          #   if (S_Rs <= rvmin){
          #     qdown <- max(qconsum_Rs+qeflow, max(qeflow,qnconsum)) - (rvmin - S_Rs)
          #     unmdem <- rvmin - S_Rs # total unmet demand (consumpt + non consumpt)
          #     S_Rs <- rvmin
          #     unmdays <- unmdays+1 # n. of days in which demand is unmet
          #   }
          #   if (S_Rs > rvmin & S_Rs < rvmax){
          #     qdown <- max(qconsum_Rs+qeflow, max(qeflow,qnconsum))
          #     cons_Rs_flag <- 1
          #   }
          #   Q_Rs <- max(qdown-qconsum_Rs,0)
          # }#end if (is.na(damhei)...)
        } else {
          Q_Rs <- 0 #at estuary no reservoir should be modelled
        }#end if (k < dim(tab_)[1])

        #Calculating outflows from river stretch below a dam which connects to the downstream dam or estuary
        if (t_dn<=1) { # in some cases the water residence time is < 1 day, then we assume that whole S_dn flows out each time-step
          Q_dn<-S_dn
        } else {
          Q_dn<-S_dn/t_dn
        }#end if (t_dn<=1)

        S_dn<-S_dn+Q_Rs-Q_dn
        if(S_dn<1e-24) S_dn <- 1e-24

        #RL: Now update of N and P storages based on lateral advection
        #RONNY: now update based on lateral flows (should be seperate to avoid negative storages)
        if ((S_up+Q_up)>1e-24) {
          if (cons_up_flag==1) {
            if(qconsum_up>0){
              TN_cons_up <- S_N_up/(S_up+qconsum_up)*qconsum_up #TN outflow with water abstraction
              S_N_up<-S_N_up-TN_cons_up# substract TN lost with upstream water demand
              TP_cons_up <- S_P_up/(S_up+qconsum_up)*qconsum_up #TN outflow with water abstraction
              S_P_up<-S_P_up-TP_cons_up# substract TN lost with upstream water demand
            }
          }
          TN_up<-S_N_up/(S_up+Q_up)*Q_up # N outflow coupled to water outflow  #RONNY: important to compute before you use it. In version before, you have used the initialized values of 0
          TP_up<-S_P_up/(S_up+Q_up)*Q_up
        } else {
          TN_up<-0.0
          TP_up<-0.0
        } #end if ((S_up+Q_up)>0.0)
        S_N_up<-S_N_up+TN_und[t]-TN_up # storage update
        if(S_N_up<1e-24) S_N_up <- 1e-24
        S_P_up<-S_P_up+TP_und[t]-TP_up # storage update
        if(S_P_up<1e-24) S_P_up <- 1e-24

        if ((S_Rs+Q_Rs)>1e-24) {
          if (cons_Rs_flag==1) {
            if(qconsum_Rs>0){
              TN_cons_Rs <- S_N_Rs/(S_Rs+qconsum_Rs)*qconsum_Rs #TN outflow with water abstraction
              S_N_Rs<-S_N_Rs-TN_cons_Rs# substract TN lost with Rsstream water demand
              TP_cons_Rs <- S_P_Rs/(S_Rs+qconsum_Rs)*qconsum_Rs #TN outflow with water abstraction
              S_P_Rs<-S_P_Rs-TP_cons_Rs# substract TN lost with Rsstream water demand
            }
          }
          TN_Rs<-S_N_Rs/(S_Rs+Q_Rs)*Q_Rs # N outflow coupled to water outflow, RONNY: this flux needs to be calculated first before you use it
          TP_Rs<-S_P_Rs/(S_Rs+Q_Rs)*Q_Rs # P outflow coupled to water outflow, RONNY: flux calculated before used
        } else {
          TN_Rs<-0.0
          TP_Rs<-0.0
        } # end if ((S_Rs+Q_Rs)>1e-24)
        S_N_Rs<-S_N_Rs+TN_up+TN_updam-TN_Rs # storage update, RONNY: note that I ave removed "+TN_und[t]", as this is the inflow to the "up" storage
        if(S_N_Rs<1e-24) S_N_Rs <- 1e-24
        S_P_Rs<-S_P_Rs+TP_up+TP_updam-TP_Rs # storage update, RONNY: TP_und was wrong here
        if(S_P_Rs<1e-24) S_P_Rs <- 1e-24

        if ((S_dn+Q_dn)>1e-24) {
          TN_dn<-S_N_dn/(S_dn+Q_dn)*Q_dn # N outflow coupled to water outflow, RONNY: now calculated before used
          TP_dn<-S_P_dn/(S_dn+Q_dn)*Q_dn # P outflow coupled to water outflow, RONNY: flux calculated beforeused
        } else {
          TN_dn<-0.0
          TP_dn<-0.0
        }	# end   if ((S_dn+Q_dn)>1e-24)
        S_N_dn<-S_N_dn+TN_Rs-TN_dn # storage update
        if(S_N_dn<1e-24) S_N_dn <- 1e-24
        S_P_dn<-S_P_dn+TP_Rs-TP_dn # storage update
        if(S_P_dn<1e-24) S_P_dn <- 1e-24

        # Nutrients
        # Box model fractions for nutrient elimination (note, residence time is converted back to years!)
        # T_st is the mean water temperature for which the seasonal coefficients where derived
        if(day_season[t]=="spr"){ # spring
          denit_up <- coef[1,4]*(1-(1/(1+(coef[2,4]*t_up/365))))/t_up #fraction of denitrified N on S_N_up
          denit_Rs <- coef[1,4]*(1-(1/(1+(coef[2,4]*t_Rs/365))))/t_Rs #fraction of denitrified N on S_N_Rs
          denit_dn <- coef[1,4]*(1-(1/(1+(coef[2,4]*t_dn/365))))/t_dn #fraction of denitrified N on S_N_dn
          N_burial <- coef[1,3]*(1-(1/(1+(coef[2,3]*t_Rs/365))))/t_Rs #fraction of buried N on S_N_Rs
          P_burial <- coef[1,5]*(1-(1/(1+(coef[2,5]*t_Rs/365))))/t_Rs #fraction of buried P on S_P_Rs
          T_st <- 8.9309
        }
        if(day_season[t]=="sum"){ # summer
          denit_up <- coef[4,4]*(1-(1/(1+(coef[5,4]*t_up/365))))/t_up #fraction of denitrified N on S_N_up
          denit_Rs <- coef[4,4]*(1-(1/(1+(coef[5,4]*t_Rs/365))))/t_Rs #fraction of denitrified N on S_N_Rs
          denit_dn <- coef[4,4]*(1-(1/(1+(coef[5,4]*t_dn/365))))/t_dn #fraction of denitrified N on S_N_dn
          N_burial <- coef[4,3]*(1-(1/(1+(coef[5,3]*t_Rs/365))))/t_Rs #fraction of buried N on S_N_Rs
          P_burial <- coef[4,5]*(1-(1/(1+(coef[5,5]*t_Rs/365))))/t_Rs #fraction of buried P on S_P_Rs
          T_st <- 17.8152
        }
        if(day_season[t]=="fal"){ # fall
          denit_up <- coef[7,4]*(1-(1/(1+(coef[8,4]*t_up/365))))/t_up #fraction of denitrified N on S_N_up
          denit_Rs <- coef[7,4]*(1-(1/(1+(coef[8,4]*t_Rs/365))))/t_Rs #fraction of denitrified N on S_N_Rs
          denit_dn <- coef[7,4]*(1-(1/(1+(coef[8,4]*t_dn/365))))/t_dn #fraction of denitrified N on S_N_dn
          N_burial <- coef[7,3]*(1-(1/(1+(coef[8,3]*t_Rs/365))))/t_Rs #fraction of buried N on S_N_Rs
          P_burial <- coef[7,5]*(1-(1/(1+(coef[8,5]*t_Rs/365))))/t_Rs #fraction of buried P on S_P_Rs
          T_st <- 10.5029
        }
        if(day_season[t]=="win"){ # winter
          denit_up <- coef[10,4]*(1-(1/(1+(coef[11,4]*t_up/365))))/t_up #fraction of denitrified N on S_N_up
          denit_Rs <- coef[10,4]*(1-(1/(1+(coef[11,4]*t_Rs/365))))/t_Rs #fraction of denitrified N on S_N_Rs
          denit_dn <- coef[10,4]*(1-(1/(1+(coef[11,4]*t_dn/365))))/t_dn #fraction of denitrified N on S_N_dn
          N_burial <- coef[10,3]*(1-(1/(1+(coef[11,3]*t_Rs/365))))/t_Rs #fraction of buried N on S_N_Rs
          P_burial <- coef[10,5]*(1-(1/(1+(coef[11,5]*t_Rs/365))))/t_Rs #fraction of buried P on S_P_Rs
          T_st <- 0.6051
        }

        ### Nitrogen fluxes
        ## upstream undammed river network
        # N fixation (applied on storage inflows)
        if (TP_und[t]>1e-24 & t_up>10.22) { # fixation only when P available and residence time > 10 days
          # equation for fixation from Akbarzadeh et al. (2019) with Ronny's adjustment using water temperature
          F_Nfix_up <- TN_und[t]/100*erf((t_up/365-0.028)/0.04)*37.2/(1+exp(0.5*(30.97/14.007)*TN_und[t]/TP_und[t]-6.877))*1.047**(Tw_up_[t]-T_st)
        } else {
          F_Nfix_up <- 0.0
        } #end if (TP_und[t]>1e-24 & t_up>10.22)

        # denitrification
        F_Nd_up<-S_N_up*denit_up*1.047**(Tw_up_[t]-T_st)

        # storage update based on processes of fixation, burial, denitrification
        S_N_up<-S_N_up+F_Nfix_up-F_Nd_up
        if(S_N_up < 0) S_N_up <- 0

        ## reservoir and estuaries
        # N fixation
        if ((TN_up+TN_updam)>1e-24 & t_Rs>10.22) { # fixation only when P available and residence time > 10 days
          # equation for fixation from Akbarzadeh et al. (2019) with Ronny's adjustment using water temperature
          F_Nfix_Rs <- (TN_up+TN_updam)/100*erf((t_Rs/365-0.028)/0.04)*37.2/(1+exp(0.5*(30.97/14.007)*(TN_up+TN_updam)/(TP_up+TP_updam)-6.877))*1.047**(Tw_dam_[t]-T_st)
        } else {
          F_Nfix_Rs <- 0.0
        }  #end if ((TN_up+TN_updam)>1e-24 & t_Rs>10.22)

        # N burial
        F_Nb_Rs<-S_N_Rs*N_burial

        # denitrification
        F_Nd_Rs<-S_N_Rs*denit_Rs*1.047**(Tw_dam_[t]-T_st) # includes Ronny's adjustment using water temperature

        # storage update based on processes of fixation, burial, denitrification
        S_N_Rs<-S_N_Rs+F_Nfix_Rs-F_Nb_Rs-F_Nd_Rs # storage update, RONNY only vertical exchange fluxes first, then based on updated storage, lateral fluxes
        if(S_N_Rs < 0) S_N_Rs <- 0

        ## river downstream of the dam which may flow into another dam or estuary
        # N fixation
        if (TN_Rs>1e-24 & t_dn>10.22) { # fixation only when P available and residence time > 10 days
          # equation for fixation from Akbarzadeh et al. (2019) with Ronny's adjustment using water temperature
          F_Nfix_dn <- TN_Rs/100*erf((t_dn/365-0.028)/0.04)*37.2/(1+exp(0.5*(30.97/14.007)*TN_Rs/TP_Rs-6.877))*1.047**(Tw_dn_[t]-T_st)
        } else {
          F_Nfix_dn <- 0.0
        } #end if (TN_Rs>1e-24 & t_dn>10.22)

        # denitrification
        F_Nd_dn<-S_N_dn*denit_dn*1.047**(Tw_dn_[t]-T_st) # includes Ronny's adjustment using water temperature

        # storage update based on denitrification, fixation, burial
        S_N_dn<-S_N_dn+F_Nfix_dn-F_Nd_dn
        if(S_N_dn < 0) S_N_dn <- 0

        # 2 - Phosphorus
        # P burial (only in reservoir and estuaries)
        F_Pb_Rs<-S_P_Rs*P_burial
        #F_Pb_Rs<-(0.35*t_Rs^-0.85)*S_P_Rs # when taking into account residence time (Chen et al., 2016)
        # storage update based on burial
        S_P_Rs<-S_P_Rs-F_Pb_Rs # storage update
        if(S_P_Rs < 0) S_P_Rs <- 0

        #record values

        if(dim(tab_)[1]>1){
          S_up_r[idx,t]<-S_up
          S_Rs_r[idx,t]<-S_Rs
          S_dn_r[idx,t]<-S_dn
          Q_up_r[idx,t]<-Q_up
          Q_Rs_r[idx,t]<-Q_Rs
          Q_spill_r[idx,t]<-qspill
          Q_ncons_r[idx,t]<-qnconsum
          Q_down_r[idx,t]<-qdown
          Q_cons_r[idx,t]<-qconsum_Rs
          Q_dn_r[idx,t]<-Q_dn
          Q_updam_r[idx,t]<-Q_updam

          S_N_up_r[idx,t]<-S_N_up
          F_Nfix_up_r[idx,t]<-F_Nfix_up
          F_Nb_up_r[idx,t]<-F_Nb_up
          F_Nd_up_r[idx,t]<-F_Nd_up
          #F_Nbd_up_r[idx,t]<-F_Nbd_up # taking into account residence time (Chen et al., 2016)
          TN_up_r[idx,t]<-TN_up
          TN_cons_up_r[idx,t]<-TN_cons_up

          S_N_Rs_r[idx,t]<-S_N_Rs
          F_Nfix_Rs_r[idx,t]<-F_Nfix_Rs
          F_Nb_Rs_r[idx,t]<-F_Nb_Rs
          F_Nd_Rs_r[idx,t]<-F_Nd_Rs
          #F_Nbd_Rs_r[idx,t]<-F_Nbd_Rs # taking into account residence time (Chen et al., 2016)
          TN_Rs_r[idx,t]<-TN_Rs
          TN_cons_Rs_r[idx,t]<-TN_cons_Rs

          S_N_dn_r[idx,t]<-S_N_dn
          F_Nfix_dn_r[idx,t]<-F_Nfix_dn
          F_Nb_dn_r[idx,t]<-F_Nb_dn
          F_Nd_dn_r[idx,t]<-F_Nd_dn
          #F_Nbd_dn_r[idx,t]<-F_Nbd_dn # taking into account residence time (Chen et al., 2016)
          TN_dn_r[idx,t]<-TN_dn

          TN_updam_r[idx,t]<-TN_updam

          F_Pb_up_r[idx,t]<-F_Pb_up
          S_P_up_r[idx,t]<-S_P_up
          TP_up_r[idx,t]<-TP_up
          TP_cons_up_r[idx,t]<-TP_cons_up

          F_Pb_Rs_r[idx,t]<-F_Pb_Rs
          S_P_Rs_r[idx,t]<-S_P_Rs
          TP_Rs_r[idx,t]<-TP_Rs
          TP_cons_Rs_r[idx,t]<-TP_cons_Rs

          F_Pb_dn_r[idx,t]<-F_Pb_dn
          S_P_dn_r[idx,t]<-S_P_dn
          TP_dn_r[idx,t]<-TP_dn

          TP_updam_r[idx,t]<-TP_updam
        } else{
          S_up_r[t]<-S_up
          S_Rs_r[t]<-S_Rs
          S_dn_r[t]<-S_dn
          Q_up_r[t]<-Q_up
          Q_Rs_r[t]<-Q_Rs
          Q_spill_r[t]<-qspill
          Q_ncons_r[t]<-qnconsum
          Q_down_r[t]<-qdown
          Q_cons_r[t]<-qconsum_Rs
          Q_dn_r[t]<-Q_dn
          Q_updam_r[t]<-Q_updam

          S_N_up_r[t]<-S_N_up
          F_Nfix_up_r[t]<-F_Nfix_up
          F_Nb_up_r[t]<-F_Nb_up
          F_Nd_up_r[t]<-F_Nd_up
          #F_Nbd_up_r[t]<-F_Nbd_up # taking into account residence time (Chen et al., 2016)
          TN_up_r[t]<-TN_up
          TN_cons_up_r[t]<-TN_cons_up

          S_N_Rs_r[t]<-S_N_Rs
          F_Nfix_Rs_r[t]<-F_Nfix_Rs
          F_Nb_Rs_r[t]<-F_Nb_Rs
          F_Nd_Rs_r[t]<-F_Nd_Rs
          #F_Nbd_Rs_r[t]<-F_Nbd_Rs # taking into account residence time (Chen et al., 2016)
          TN_Rs_r[t]<-TN_Rs
          TN_cons_Rs_r[t]<-TN_cons_Rs

          S_N_dn_r[t]<-S_N_dn
          F_Nfix_dn_r[t]<-F_Nfix_dn
          F_Nb_dn_r[t]<-F_Nb_dn
          F_Nd_dn_r[t]<-F_Nd_dn
          #F_Nbd_dn_r[t]<-F_Nbd_dn # taking into account residence time (Chen et al., 2016)
          TN_dn_r[t]<-TN_dn

          TN_updam_r[t]<-TN_updam

          F_Pb_up_r[t]<-F_Pb_up
          S_P_up_r[t]<-S_P_up
          TP_up_r[t]<-TP_up
          TP_cons_up_r[t]<-TP_cons_up

          F_Pb_Rs_r[t]<-F_Pb_Rs
          S_P_Rs_r[t]<-S_P_Rs
          TP_Rs_r[t]<-TP_Rs
          TP_cons_Rs_r[t]<-TP_cons_Rs

          F_Pb_dn_r[t]<-F_Pb_dn
          S_P_dn_r[t]<-S_P_dn
          TP_dn_r[t]<-TP_dn

          TP_updam_r[t]<-TP_updam
        }

      } #end for (t in 1:length(qtot_[1,]))

      unmdem_r[idx]<-unmdem
      unmdays_r[idx]<-unmdays

      k=k+1 #update counter

    } #end for (i in 1:length(tab$GRAND_ID[tab$WSL==w])
  } #end for (w in 1:max(tab$WSL))

  ##sumarize inflows
  inflow<-Q_up_r+Q_updam_r
  TN_inflow<-TN_up_r+TN_updam_r
  TP_inflow<-TP_up_r+TP_updam_r

  #return all records #to be included for other burial option: F_Nbd_up_r, F_Nbd_Rs_r, F_Nbd_dn_r
  return(list('S_up_r'=S_up_r,
              'S_Rs_r'=S_Rs_r,
              'S_dn_r'=S_dn_r,
              'Q_up_r'=Q_up_r,
              'Q_Rs_r'=Q_Rs_r,
              'Q_spill_r'=Q_spill_r,
              'Q_ncons_r'=Q_ncons_r,
              'Q_down_r'=Q_down_r,
              'Q_cons_r'=Q_cons_r,
              'Q_dn_r'=Q_dn_r,
              'Q_updam_r'=Q_updam_r,
              'S_N_up_r'=S_N_up_r,
              'F_Nfix_up_r'=F_Nfix_up_r,
              'F_Nb_up_r'=F_Nb_up_r,
              'F_Nd_up_r'=F_Nd_up_r,
              'TN_up_r'=TN_up_r,
              'TN_cons_up_r'=TN_cons_up_r,
              'S_N_Rs_r'=S_N_Rs_r,
              'F_Nfix_Rs_r'=F_Nfix_Rs_r,
              'F_Nb_Rs_r'=F_Nb_Rs_r,
              'F_Nd_Rs_r'=F_Nd_Rs_r,
              'TN_Rs_r'=TN_Rs_r,
              'TN_cons_Rs_r'=TN_cons_Rs_r,
              'S_N_dn_r'=S_N_dn_r,
              'F_Nfix_dn_r'=F_Nfix_dn_r,
              'F_Nb_dn_r'=F_Nb_dn_r,
              'F_Nd_dn_r'=F_Nd_dn_r,
              'TN_dn_r'=TN_dn_r,
              'TN_updam_r'=TN_updam_r,
              'F_Pb_up_r'=F_Pb_up_r,
              'S_P_up_r'=S_P_up_r,
              'TP_up_r'=TP_up_r,
              'TP_cons_up_r'=TP_cons_up_r,
              'F_Pb_Rs_r'=F_Pb_Rs_r,
              'S_P_Rs_r'=S_P_Rs_r,
              'TP_Rs_r'=TP_Rs_r,
              'TP_cons_Rs_r'=TP_cons_Rs_r,
              'F_Pb_dn_r'=F_Pb_dn_r,
              'S_P_dn_r'=S_P_dn_r,
              'TP_dn_r'=TP_dn_r,
              'TP_updam_r'=TP_updam_r,
              'inflow'=inflow,
              'TN_inflow'=TN_inflow,
              'TP_inflow'=TP_inflow,
              'unmdem'=unmdem_r,
              'unmdays'=unmdays_r))
}
