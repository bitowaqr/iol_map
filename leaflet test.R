


event_sp = shapefile(x = "./raw/event_sp")
library(leaflet)
  
  tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
  tiles_participation <- "https://bitowaqr.github.io/iol_map/tiles_participation/{z}/{x}/{y}.png"
  
  leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12), width = "100%",height = 800) %>%
  
    
    
    # provider tile
    addTiles(group="OSM") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Carto") %>% 
    
    
    # my tile
    addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group="Distances") %>%
    addTiles(tiles_participation, options = tileOptions(opacity = 0.5),group="Participation") %>%
    
    # established events
    addCircleMarkers(
      group = "Parkrun events",
      data = event_sp,
      # lng = ~lon,lat = ~lat,
      radius = 3,fillColor = "blue",
      stroke = F, fillOpacity = 1,
      popup = paste("Course:",event_sp$course,"<br>",
                    "Established:", event_sp$Estblsh,"<br>",
                    "Served pop:", round(event_sp$srvd_pop),"<br>",
                    "Served LSOA:", round(event_sp$srvd_lsoa),"<br>",
                    "Mean participants:", round(event_sp$Mn_prtc),"<br>",
                    "Mean volunteers:", round(event_sp$Mn_vlnt))
    ) %>% 
    addLegend(
      group = "Distances",
      colors =c("darkgreen","green","orange","red","darkred"),
      title = "Distance to the <br> Nearest parkrun event",
      labels = c("&nbsp < 1 km","  1-2.5 km","2.5- 5 km","&nbsp 5 -10 km","&nbsp   > 10 km"),
      labFormat = labelFormat(prefix = "", suffix = "km", between = " &ndash; ",
                              digits = 0, big.mark = ",", transform = identity)
      ) %>%
    
    
    
  
  # leaflet options
  setView(0, 52, zoom = 7) %>%
    addLayersControl(
      baseGroups = c("OSM","Carto"),
      overlayGroups = c("Distances","Participation"),
      options = layersControlOptions(collapsed = F,autoZIndex=T) 
    ) %>%
    hideGroup("Distances") %>%
    hideGroup("Participation")
  
  
