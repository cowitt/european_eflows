##############################################################################
## Project: unnamed Euroflow modelling paper - scatter plots
## Capabilities: basin and dam plotting
## author: cordula.wittekind@ufz.de
##############################################################################
rm(list=ls())

library(sf)
library(leaflet)
library(webshot)

## load RDS or other file to plot that contains Dam ID
extract_damid <- function(list_results) {
  damids <- c()
  
  for (i in 1:length(list_results)) {
    damids <- c(damids, list_results[[i]]$damID)
  }
  
  return(damids)
}

damids = extract_damid(v) #v created in main_analysis.R


### only needed for Estuaries ####
NEWS_2000a <- read.csv("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/input/c00run5_NEWSInputOutput.csv", as.is=T) # GlobalNEWS database to get basin names and reference nutrient loads
basins <- NEWS_2000a$BASINID[NEWS_2000a$continent=='Europe' & substr(NEWS_2000a$basinname,1,5)!='GHAAS']+100000 #all European basins

reservoirs_basinX <- left_join(filter_catchment(tab, basins)[-1,-c(2,3)],
                               GRAND_data[,c(1,3,5)], by="GRAND_ID")

## function to backfill terminal values
backfill_terminal_values <- function(df, col1_name, col2_name) {
  result_df <- df
  
  # mapping of all connections
  connections <- setNames(df[[col2_name]], df[[col1_name]])
  
## find terminal value following the chain
  find_terminal <- function(start_id, visited = c()) {
    # convert to character to ensure consistent matching
    start_id <- as.character(start_id)
    
    # avoid infinite loops
    if (start_id %in% visited) {
      return(start_id)
    }
    
    # does this id exists in our connections
    if (!start_id %in% names(connections)) {
      return(start_id)
    }
    
    # next ID
    next_id <- connections[[start_id]]
    
    # if next_id is NA, this is terminal
    if (is.na(next_id)) {
      return(start_id)
    }
    
    # follow the chain
    return(find_terminal(as.character(next_id), c(visited, start_id)))
  }
  
  # Update each row with its terminal value
  for (i in 1:nrow(result_df)) {
    current_id <- result_df[[col1_name]][i]
    terminal_value <- find_terminal(current_id)
    result_df[[col1_name]][i] <- terminal_value
  }
  
  return(result_df)
}


df_updated <- backfill_terminal_values(reservoirs_basinX, "GRAND_ID", "DOWNSTREAM_ID")

df_updated = df_updated %>% mutate(estuary = GRAND_ID) %>% mutate(GRAND_ID = reservoirs_basinX$GRAND_ID)
##### end of estuary stuff --- dam plotting #####

setwd("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/input/Dam_Shapes")
est_catch= read_sf(dsn = ".", layer = "Dams_Areas_2000")%>%select(GRAND_ID, RES_NAME, geometry)%>%st_make_valid() %>% st_transform(crs = 4326)
# est_catch2= read_sf(dsn = ".", layer = "Dam_pointt")%>%select(GRAND_ID, RES_NAME, geometry)%>%st_make_valid() %>% st_transform(crs = 4326)

#when plotting estuaries - pull all dams that fill estuaries first
est_relevant = df_updated %>% select(estuary, GRAND_ID) %>% dplyr::rename(damID = GRAND_ID)

# yo is the dataframe made from different variables
yo = yo %>% left_join(est_relevant, by = c("basin" = "estuary"))


#subset (yo made in main analysis)
est = est_catch %>% filter(GRAND_ID %in% c$damID)

# european_euroflow_dams = est_catch2 %>% filter(GRAND_ID %in% c$damID)
# st_write(european_euroflow_dams, "only_europe_dams.shp") # pull for all est made of Dam_pointt
europe = est %>% left_join(yo, by = c("GRAND_ID" = "damID"))

europe <- europe %>%
  mutate(area = as.numeric(st_area(.))) %>%
  arrange(desc(area))


europe = europe[,c("GRAND_ID","geometry","scenario 0","basin")]


## one scenario

breaks <- c(6, 10, 16, 20, 25)
mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
# 
pal <- colorBin(palette = "viridis",
                domain = col_dom,
                bins = breaks)
legend_cols = pal(mids) #pulled for label in legend
labels <- c("N-limited (6-10)","Moderate (10-16)",
            "Transition (16-20)", "P-limited (20-25)")


 n = leaflet(europe, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
   addPolygons(data = est,
               weight = 0,
               fillColor   = "darkgray", fillOpacity = 1) %>%
   addPolygons(
      weight = 0.5,
      color = ~ pal(europe$`scenario 0`),
      fillColor = ~ pal(europe$`scenario 0`),
      fillOpacity = 0.7
    ) %>%
   addLegend(
      # pal = pal,
      values = ~ pal(europe$`scenario 0`),
      colors = legend_cols,
      labels = labels, #TURN OFF FOR PCT CHANGE
      title = "TN:TP ratio",
      position = "bottomright"
    )%>%
   htmlwidgets::onRender("
  function(el, x) {
    // target the legend specifically
    var legend = el.querySelector('.legend');
    if (legend) {
      legend.style.fontSize = '24px';
    }
    
    //  target legend text elements
    var legendLabels = el.querySelectorAll('.legend-labels');
    for (var i = 0; i < legendLabels.length; i++) {
      legendLabels[i].style.fontSize = '24px';
    }
  }
")

  
  htmlwidgets::saveWidget(n, "temp.html", selfcontained = FALSE)
  webshot::webshot("temp.html", file = "../../figures/maps/scenario0.png", cliprect = "viewport", vwidth = 980, vheight = 940)

  europe_all = rbind(europe, europe2)
length(unique(europe_all$basin))

nlim = europe_all %>% filter(`scenario 0` <10) 
length(unique(nlim$basin));length(unique(nlim$basin))/40*100

mod =europe_all %>% filter(`scenario 0` > 10) %>% filter(`scenario 0` <16)
length(unique(mod$basin));length(unique(mod$basin))/40*100

trans =europe_all %>% filter(`scenario 0` > 16) %>% filter(`scenario 0` <20)
length(unique(trans$basin));length(unique(trans$basin))/40*100

plim =europe_all %>% filter(`scenario 0` > 20) %>% filter(`scenario 0` <25)
length(unique(plim$basin));length(unique(plim$basin))/40*100

## several scenarios
# ## pct change if needed
europe$pct_change =(( europe$`scenario 0`-europe$`scenario -1`)/europe$`scenario -1`) * 100
breaks <- seq(range(europe$pct_change)[1],range(europe$pct_change)[2],(abs(range(europe$pct_change)[1])+abs(range(europe$pct_change)[2]))/8);breaks


breaks = seq(-100,20,20);breaks
pal <- colorBin(palette = "viridis",
                domain = europe$`scenario 7`,
                bins = breaks)
mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
legend_cols = pal(mids)
labels = c("-100 - -80","-80 - -60","-60 - -40","-40 - -20","-20 - 0", ">0")
# pplooott
m = list()

for (scen in names(RT)[1:7]) {
 n = leaflet(europe, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
   addPolygons(data = est,
               weight = 0,
               fillColor   = "darkgray", fillOpacity = 1) %>%
   addPolygons(
      weight = 0.5,
      color = ~ pal(europe[[scen]]),
      fillColor = ~ pal(europe[[scen]]),
      fillOpacity = 0.7
    ) %>%
    addLegend(
      pal = pal,
      values =~ pal(europe[[scen]]),

      # colors = legend_cols,
      labels = labels, #TURN OFF FOR PCT CHANGE
      title = paste0(scen, ":</br> % difference in</br>residence time"),
      position = "bottomright"
    )%>%
   htmlwidgets::onRender("
  function(el, x) {
    // Target the legend specifically
    var legend = el.querySelector('.legend');
    if (legend) {
      legend.style.fontSize = '20px';
    }
    
    // Also target legend text elements
    var legendLabels = el.querySelectorAll('.legend-labels');
    for (var i = 0; i < legendLabels.length; i++) {
      legendLabels[i].style.fontSize = '20px';
    }
  }
")
  # m[[scen]] = n
  
  htmlwidgets::saveWidget(n, "temp.html", selfcontained = FALSE)

  webshot::webshot("temp.html", file = paste0("../../figures/maps/", scen, "rt_change.png"), cliprect = "viewport", vwidth = 980, vheight = 940)
}

library(htmlwidgets)
saveWidget( m[[7]], file = "../../maps/.html")


## finding the connection to the estuary
euroflow_dams = read_sf("C:/Users/wittekin/Documents/cle2024/projects/european_eflows/input/Dam_Shapes/only_europe_dams.shp")
hydrobas= read_sf(dsn = "C:/Users/wittekin/Documents/cle2024/projects/european_eflows/hydrobasin", 
                  layer = "hybas_eu_lev04_v1c")%>%st_make_valid() %>% st_transform(crs = 4326)

leaflet(hydrobas) %>%
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons() %>%
  addCircleMarkers(data = euroflow_dams, radius = 5, color = "blue", fill = TRUE)


x_buffered <- st_buffer(euroflow_dams, dist = 100)

hydbas_subset <- st_filter(hydrobas, x_buffered, .predicate = st_intersects)
# st_write(hydbas_subset, "hydrobasin_estuary_connection.shp") # pull for all est made of Dam_pointt

