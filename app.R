
# parkrun iolmap app prototype I

# load packages
    library(shiny)
    library(leaflet)
    library(raster)
    library(sp)
    library(sf)
    print("packages loaded")
    
# alt shape fun
    pchIcons = function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...) {
        n = length(pch)
        files = character(n)
        # create a sequence of png images
        for (i in seq_len(n)) {
            f = tempfile(fileext = '.png')
            png(f, width = width, height = height, bg = bg)
            par(mar = c(0, 0, 0, 0))
            plot.new()
            points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
            dev.off()
            files[i] = f
        }
        files
    }
    

# load data
    # lsoa data
    lsoa_sf = read_sf("./raw/lsoa_sf.shp")
    # established events data
    event_sp = shapefile("./events/event_sp")
    # proposed new event locations
    runs_imd = read_sf("./events/opt_loc_runs_imd.prep.shp")
    runs_imd_shape = pchIcons(16, 20, 20, col = c("darkorange"), lwd = 3)
    
    runs_tot = read_sf("./events/opt_loc_runs.prep.shp")
    runs_tot_shape = pchIcons(17, 20, 20, col = c("darkorange"), lwd = 3)
    
    dist_imd = read_sf("./events/opt_loc_dist_imd.prep.shp")
    dist_imd_shape = pchIcons(16, 20, 20, col = c("red"), lwd = 3)
    
    dist_tot = read_sf("./events/opt_loc_dist.prep.shp")
    dist_tot_shape = pchIcons(17, 20, 20, col = c("red"), lwd = 3)
    
    # green spaces considered in the analysis
    greens_coordinates = read.csv("./raw/greens_coordinates.csv")
    print("Data loaded")
    
# User Interface
    ui <- bootstrapPage(
        tags$style(type = "text/css", 
        "#map {height: calc(100vh ) !important;
        padding-top: 10px;
        padding-left: 10px;
                   }"),
        
        
        leafletOutput('map'),
        
        absolutePanel(id = "controls",  fixed = TRUE,
                      draggable = TRUE, top = 20, right = "auto", left = 50, bottom = "auto",
                      width = "auto", height = "auto",
                      # info text
                      div(id = "button",style="border-bottom: 1px solid #CCC; background-color: rgb(250, 250,250,0);",
                      HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#demo'>Quick help</button>")
                      ),
                      div(id = "demo", class = "collapse",
                          style="border: 1px solid #CCC; background-color: rgb(250, 250,250,0.9); width: 700px;",
                          HTML("<br>This map shows the results of a research project, aimed at supporting parkrun's 
                               plan to create 200 new parkrun events across England. The full study can be found"),
                          
                          HTML("<a href='https://github.com/bitowaqr/iolmap_analysis' target='_blank'>here</a>"),
                          HTML("<hr>"),
                          
                          HTML("The map contain x laylers:"),
                          HTML("With the control panel on the right, you can show and hide different layer:
                               Area attributes show (simplified) ...<br>"),
                          HTML("With the control panel on the right, you can show and hide different layer:
                               Area attributes show (simplified) ...<hr>"),
                          HTML("With the control panel on the right, you can show and hide different layer:
                               Area attributes show (simplified) ...<br>"),
                          HTML("Contact:")
                          
                      )),
        absolutePanel(id = "info",  fixed = TRUE,
                      draggable = TRUE, bottom = 30, left = 10, right = "auto", top = "auto",
                      width = "280", height = "auto",
                      
                      div(id = "lsoainfo", 
                          style="border: 2px solid #CCC; background-color: rgb(250, 250,250,0.9); padding: 10px",
                          HTML(" <strong>LSOA summary:</strong>"),
                          htmlOutput("text1")
                          )
        )
    )
    
    
    server <- function(input, output) {
        
    
        output$map <- renderLeaflet({
            
            
            tag.map.ref <- tags$style(HTML("
                      .leaflet-control.map-title {
                        transform: translate(-50%,20%);
                        position: fixed !important;
                        left: 50%;
                        text-align: center;
                        padding-left: 10px;
                        padding-right: 10px;
                        background: rgba(255,255,255,0.5);
                        
                        font-size: 12px;
                        border: 0;
                      }
                                             "))
            ref = tags$div(id="ref",'Schneider et al. Identifying Optimal Locations to Maximise Access to parkrun events. 2019. Data & code available at',tags$a(href="https://github.com/bitowaqr/iolmap_analysis", "Github",target="_blank"))
            # # font-weight: bold;
            # title <- tags$div(
            #     tag.map.title, HTML("Identifying Optimal Locations to Maximise Access to parkrun events")
            # )
            
            # urls to get custom tiles
            tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
            tiles_participation <- "https://bitowaqr.github.io/iol_map/tiles_participation/{z}/{x}/{y}.png"
            tiles_imd <- "https://bitowaqr.github.io/iol_map/tiles_imd/{z}/{x}/{y}.png"
            tiles_pop_km2 <- "https://bitowaqr.github.io/iol_map/tiles_pop_km2/{z}/{x}/{y}.png"
            tiles_greenspaces <- "https://bitowaqr.github.io/iol_map/tiles_greenspaces/{z}/{x}/{y}.png"
            
            popkm2.lab = "Population density"
            dist_label = "Distance to nearest event"
            imd_label = "IMD Score"
            part_label = "Participation rate<hr> <u>Recommendations:</u>"
            greens_label = "Greenspaces considered"
            runs_imd_label = "max Runs * IMD<sup>2</sup>"
            runs_tot_label = "max total Runs"
            dist_imd_label  = "min Distances * IMD<sup>2</sup>"
            dist_tot_label = "min total Distances"
            parkrun_label = "Established parkrun events<hr> <u>Area attributes:</u>"
    # BUILD LEAFLET MAP
            leaflet(options = leafletOptions(minZoom = 6, maxZoom = 12), width = "100%") %>% 
    # provider tiles
                # addTiles(group="OSM") %>%
                addProviderTiles(providers$CartoDB.DarkMatter, group="CartoDB Darkmode<br> ") %>% 
                addProviderTiles(providers$CartoDB.Positron, group="CartoDB Basemap") %>%
                
    # add reference tag
            addControl(ref, position = "bottomleft", className="map-ref") %>%    
    
    # add CONTROLS           
                addLayersControl(
                    baseGroups = c("CartoDB Basemap","CartoDB Darkmode<br> "),
                    overlayGroups = c(greens_label,parkrun_label,
                                      popkm2.lab,imd_label,dist_label,part_label,
                                      runs_imd_label,runs_tot_label,dist_imd_label,dist_tot_label),
                    options = layersControlOptions(collapsed = F,autoZIndex=F,opacity=0) 
                ) %>%
        
    # add my tiles
                addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group=dist_label) %>%
                addTiles(tiles_participation, options = tileOptions(opacity = 0.5),group=part_label) %>%
                addTiles(tiles_imd, options = tileOptions(opacity = 0.5),group=imd_label) %>%
                addTiles(tiles_pop_km2, options = tileOptions(opacity = 0.5),group=popkm2.lab) %>%
                addTiles(tiles_greenspaces, options = tileOptions(opacity = 0.5),group=greens_label) %>%
                
    # established parkrun events
                addCircleMarkers(
                    group = parkrun_label,
                    data = event_sp,
                    # lng = ~lon,lat = ~lat,
                    radius = 5,fillColor = "blue",
                    stroke = F, fillOpacity = 1,
                    popup = paste("parkrun event:",event_sp$course,"<br>",
                                  "Established:", event_sp$Estblsh,"<br>",
                                  "Served pop:", round(event_sp$srvd_pop),"<br>",
                                  "Mean participants:", round(event_sp$Mn_prtc),"<br>",
                                  "Mean volunteers:", round(event_sp$Mn_vlnt))
                ) %>%
                
    # Greenspaces considered
                addCircleMarkers(data=greens_coordinates,
                                 lng = ~lng, lat = ~lat,
                                 radius = 9, 
                                 stroke = F,
                                 fillColor="white",fillOpacity=0,
                                 group = greens_label,
                                 popup = paste("Name: ",greens_coordinates$name,"<br>",
                                               "Type: ", greens_coordinates$type,"<br>",
                                               "Area (km<sub>2</sup>): ", round(greens_coordinates$area_km2,2),"<br>",
                                               "Coordinates: ", round(greens_coordinates$lng,5),"; ", round(greens_coordinates$lat,5),"<br>",
                                               "<a href='https://www.google.com/maps/@",greens_coordinates$lat,",",greens_coordinates$lng,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                                               sep=""
                                               
                                 )
                                 
                ) %>%
                
    # NEW EVENTS 
        # RUNS with IMD weights
                addMarkers(
                    data = runs_imd, 
                    group = runs_imd_label, 
                    icon = ~icons(iconUrl = runs_imd_shape, 
                                  iconAnchorX = 10, iconAnchorY = 10
                    ),
                    popup = paste("Name: ",runs_imd$distName1,"<br>",
                                  "Rank: ", runs_imd$pos,"<br>",
                                  "Objective improvement: ",runs_imd$change, "%<br>",
                                  "Coordinates: ", round(runs_imd$lat,5), ", ",round(runs_imd$lon,5),"<br>",
                                  "<a href='https://www.google.com/maps/@",runs_imd$lat,",",runs_imd$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                                  sep=""),
                    lng = ~lon, lat = ~lat,
                    label=as.character(runs_imd$pos),
                    labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
                ) %>%
        # RUNS TOTAL
            addMarkers(
                data = runs_tot, 
                group = runs_tot_label, 
                icon = ~icons(iconUrl = runs_tot_shape, 
                              iconAnchorX = 10, iconAnchorY = 10
                ),
                popup = paste("Name: ",runs_tot$distName1,"<br>",
                              "Rank: ", runs_tot$pos,"<br>",
                              "Change: ",runs_tot$change, "%<br>",
                              "Coordinates: ", round(runs_tot$lat,5),"; ", round(runs_tot$lon,5),"<br>",
                              "<a href='https://www.google.com/maps/@",runs_tot$lat,",",runs_tot$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                              sep=""),
                # radius=7,opacity = 1,color=NULL,stroke = F,fillColor="orange",fillOpacity =  0.5,
                lng = ~lon, lat = ~lat,
                label=as.character(runs_tot$pos),
                labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
            ) %>%
        # DIST with IMD weights
            addMarkers(
                data = dist_imd, 
                group = dist_imd_label, 
                icon = ~icons(iconUrl = dist_imd_shape, 
                              iconAnchorX = 10, iconAnchorY = 10
                ),
                popup = paste("Name: ",dist_imd$distName1,"<br>",
                              "Rank: ", dist_imd$pos,"<br>",
                              "Change: ",dist_imd$change, "%<br>",
                              "Coordinates: ", round(dist_imd$lat,5),"; ", round(dist_imd$lon,5),"<br>",
                              "<a href='https://www.google.com/maps/@",dist_imd$lat,",",dist_imd$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                              sep=""),
                lng = ~lon, lat = ~lat,
                label=as.character(dist_imd$pos),
                labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
            ) %>%
        # DIST TOTAL
        addMarkers(
            data = dist_tot, 
            group = dist_tot_label, 
            icon = ~icons(iconUrl = dist_tot_shape, 
                          iconAnchorX = 10, iconAnchorY = 10
            ),
            popup = paste("Name: ",dist_tot$distName1,"<br>",
                          "Rank: ", dist_tot$pos,"<br>",
                          "Change: ",dist_tot$change, "%<br>",
                          "Coordinates: ", round(dist_tot$lat,5),"; ", round(dist_tot$lon,5),"<br>",
                          "<a href='https://www.google.com/maps/@",dist_tot$lat,",",dist_tot$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                          sep=""),
            lng = ~lon, lat = ~lat,
            label=as.character(dist_tot$pos),
            labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
        ) %>%
             
            
    # LEGENDS
                addLegend(
                    group = dist_label,
                    colors =c("darkgreen","green","orange","red","darkred"),
                    title = "Distance to the <br> Nearest parkrun event",
                    labels = c("&nbsp < 1 km","  1-2.5 km","2.5- 5 km","&nbsp 5 -10 km","&nbsp   > 10 km"),
                    labFormat = labelFormat(prefix = "", suffix = "km", between = " &ndash; ",
                                            digits = 0, big.mark = ",", transform = identity)
                ) %>%
                addLegend(
                    group = imd_label,
                    colors =c("darkgreen","green","orange","red","darkred"),
                    title = "Index of Multiple <br> Deprivation Quintiles",
                    labels = c("Least deprived","Less deprived","","More deprived","Most deprived")
                ) %>%
                addLegend(
                    group = part_label,
                    colors =c("darkred","red","orange","green","darkgreen"),
                    title = "parkrun runs per <br> 1,000 per week",
                    labels = c("&nbsp 0","0-0.5","0.5-1","1 - 2",">2")
                ) %>%
                addLegend(
                    group = popkm2.lab,
                    colors =c("darkgreen","green","orange","red","darkred"),
                    title = "Population density",
                    labels = c("Least dense","Less dense","","More dense","Most dense")
                ) %>%
                
                
        # set view
                setView(-1.2, 53, zoom = 7) %>%
        
        # leaflet layer options
                hideGroup(dist_label) %>%
                hideGroup(imd_label) %>%
                hideGroup(part_label) %>%
                hideGroup(popkm2.lab) %>%
                hideGroup(runs_imd_label) %>%
                hideGroup(runs_tot_label) %>%
                hideGroup(dist_imd_label) %>%
                hideGroup(dist_tot_label) %>%
                hideGroup(greens_label) 
            
            
                    
            
        })
        
        # click to get info
        observeEvent(input$map_click,ignoreNULL = F, {
            if(!is.null(input$map_click)){
                click_location <- input$map_click
                lng = click_location$lat
                lat = click_location$lng
                p <- st_as_sf(SpatialPoints(cbind(lat,lng),proj4string = CRS(" +proj=longlat +ellps=WGS84 +no_defs ")))
                index = as.numeric(st_intersects(p,lsoa_sf))
                if(!is.na(index)){
                    hit = as.data.frame(lsoa_sf)[index,]
                lsoa_name = ifelse(nchar(hit$name)>30,
                                   paste(substr(hit$name,1,30),"...",sep=""),
                                   hit$name)
                print(lsoa_name)

                } else {
                    hit = as.data.frame(lsoa_sf)[11467,]
                    lsoa_name = "<b>Not found</b>" # quick and dirty solution to avoid NA
                }
            } else {
                hit = as.data.frame(lsoa_sf)[11467,]
                lsoa_name = ifelse(nchar(hit$name)>30,
                                   paste(substr(hit$name,1,30),"...",sep=""),
                                   hit$name)
            }
                
            output$text1 <- renderUI({
                str1 <- paste("&nbsp Name:",lsoa_name)
                str3 <- paste("&nbsp Population:",hit$pop)
                str3.5 <- paste("&nbsp Density:",hit$pop_km2,"/km<sup>2</sup>")
                str3.9 <- paste("&nbsp IMD Score:",round(hit$imd_sc,1))
                str4 <- paste("&nbsp ",round(hit$mn_dstn,2),"km to nearest parkrun event")
                str5 <- paste("&nbsp ",round(hit$rns_pm_,3),"Runs per 1,000 per week")
                HTML(paste(str1, str3,str3.5,str3.9,str4,str5, sep = '<br/>'))
            })
            
            
            
        })
        
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
