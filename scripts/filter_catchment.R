#### function to extract all GRAND_IDs belonging to a certain estuary
filter_catchment <- function(tab, GRAND_ID){
  idx_ <- NA
  for(k in 1:length(GRAND_ID)){
    idx<-which(tab$GRAND_ID==GRAND_ID[k])
    for(i in 1:tab$WSL[which(tab$GRAND_ID==GRAND_ID[k])]){
      if(i==1) {
        idx<-c(idx, which(tab$DOWNSTREAM_ID==GRAND_ID[k]))
        GRAND_ID_ <- tab$GRAND_ID[which(tab$DOWNSTREAM_ID==GRAND_ID[k])]
      } else{
        idx<-c(idx, which(tab$DOWNSTREAM_ID %in% GRAND_ID_))
        GRAND_ID_ <- tab$GRAND_ID[which(tab$DOWNSTREAM_ID %in% GRAND_ID_)]
      }
    }
    idx_ <- c(idx_,idx)
  }
  return(tab[idx_[-1],])
}
