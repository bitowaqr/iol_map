
# parkrun iolmap app prototype I

# load packages
    library(shiny)
    library(leaflet)
    library(raster)
    library(sp)
    library(sf)
    print("packages loaded")

# load data
    # lsoa data
    lsoa_sf = read_sf("./raw/lsoa_sf.shp")
    # established events data
    event_sp = shapefile("./events/event_sp")
    # proposed new event locations
    runs_imd = shapefile("./events/opt_loc_runs_imd.prep.shp")
    runs_tot = shapefile("./events/opt_loc_runs.prep.shp")
    dist_imd = shapefile("./events/opt_loc_dist_imd.prep.shp")
    dist_tot = shapefile("./events/opt_loc_dist.prep.shp")
    print("Data loaded")


    
# User Interface
    ui <- bootstrapPage(
        # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        tags$style(type = "text/css", 
        "#map {height: calc(100vh ) !important;
        padding-top: 10px;
        padding-left: 10px;
                   }"),
        # titlePanel("IOLMAP"),
        
        
        leafletOutput('map'),
    #     ,
    # absolutePanel(id = "title", class = "panel panel-default", fixed = TRUE,
    #               draggable = T, bottom = "auto", left = 50, right = "auto", top = 5,
    #               width = "auto", height = "auto",
    #               h4("- Identifying Optimal Locations to Maximise Access to parkrun events -")
    # ),
        
        absolutePanel(id = "controls",  fixed = TRUE,
                      draggable = TRUE, top = 20, right = "auto", left = 50, bottom = "auto",
                      width = "auto", height = "auto",
                      div(id = "button",style="border-bottom: 1px solid #CCC; background-color: rgb(80, 184,217,0.8);",
                      HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#demo'>Quick help</button>")),
                      div(id = "demo", class = "collapse",
                          style="border-bottom: 1px solid #CCC; background-color: rgb(80, 184,217,0.8);",
                          HTML("Identifying Optimal Locations to Maximise Access to parkrun events <br>"),
                          HTML("Some explanatio bla bla <br>"),
                          
                          sliderInput(
                              inputId = "sld01_Mag",
                              label="Additional input?",
                              min=1, max=10,
                              value=c(5)
                          ),
                          selectInput("add_layers","Show",choices = c("LSOA centroids","Something else"),multiple = T)
                      )),
        absolutePanel(id = "info", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, bottom = 20, left = "auto", right = 10, top = "auto",
                      width = "280", height = "auto",
                      
                      div(id = "lsoainfo", 
                          style="padding: 8px; border: 2px solid #CCC",
                      HTML(" <strong>LSOA info:</strong>"),
                      
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
            ref = tags$div(id="ref",'Schneider et al.',tags$em('IOL MAP'), '2019. Data and code available at:',tags$a(href="https://github.com/bitowaqr/", "https://github.com/bitowaqr/",target="_blank"))
            # # font-weight: bold;
            # title <- tags$div(
            #     tag.map.title, HTML("Identifying Optimal Locations to Maximise Access to parkrun events")
            # )
            
            
            tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
            tiles_participation <- "https://bitowaqr.github.io/iol_map/tiles_participation/{z}/{x}/{y}.png"
            tiles_imd <- "https://bitowaqr.github.io/iol_map/tiles_imd/{z}/{x}/{y}.png"
            tiles_pop_km2 <- "https://bitowaqr.github.io/iol_map/tiles_pop_km2/{z}/{x}/{y}.png"
            popkm2.lab = "popkm2 <br> <br><u> Recommendations</u>: "
            
            leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12), width = "100%") %>% 
                # alternative: show unmoving title on top?
                # addControl(title, position = "topleft", className="map-title") %>%
            addControl(ref, position = "bottomleft", className="map-ref") %>%    
                
                
                
                addLayersControl(
                    baseGroups = c("OSM","Carto"),
                    overlayGroups = c("Distances","IMD","Participation",popkm2.lab,"runs_imd","runs_tot","dist_imd","dist_tot"),
                    options = layersControlOptions(collapsed = F,autoZIndex=F,opacity=0) 
                ) %>%
                # provider tile
                addTiles(group="OSM") %>%
                addProviderTiles(providers$CartoDB.Positron, group="Carto") %>% 
    
                # my tile
                addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group="Distances") %>%
                addTiles(tiles_participation, options = tileOptions(opacity = 0.5),group="Participation") %>%
                addTiles(tiles_imd, options = tileOptions(opacity = 0.5),group="IMD") %>%
                addTiles(tiles_pop_km2, options = tileOptions(opacity = 0.5),group=popkm2.lab) %>%
                
    # established parkrun events
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
    # NEW EVENTS 
        # RUNS with IMD weights
                addCircleMarkers(
                    data = runs_imd, group = "runs_imd", 
                    radius=7,opacity = 1,color=NULL,stroke = F,fillColor="orange",fillOpacity =  0.5,
                    lng = ~lon, lat = ~lat,
                    label=as.character(runs_imd$pos),
                    labelOptions = labelOptions(noHide = T, textsize = "10px",textOnly = T, direction ="center")
                ) %>%
        # RUNS TOTAL
                addCircleMarkers(
                    data = runs_tot, group = "runs_tot", 
                    radius=7,opacity = 1,color=NULL,stroke = F,fillColor="pink",fillOpacity =  0.5,
                    lng = ~lon, lat = ~lat,
                    label=as.character(runs_tot$pos),
                    labelOptions = labelOptions(noHide = T, textsize = "10px",textOnly = T, direction ="center")
                ) %>%
        # DIST with IMD weights
                addCircleMarkers(
                    data = dist_imd, group = "dist_imd", 
                    radius=7,opacity = 1,color=NULL,stroke = F,fillColor="purple",fillOpacity =  0.5,
                    lng = ~lon, lat = ~lat,
                    label=as.character(dist_imd$pos),
                    labelOptions = labelOptions(noHide = T, textsize = "10px",textOnly = T, direction ="center")
                ) %>%
        # DIST TOTAL
                addCircleMarkers(
                    data = dist_tot, group = "dist_tot", 
                    radius=7,opacity = 1,color=NULL,stroke = F,fillColor="green",fillOpacity =  0.5,
                    lng = ~lon, lat = ~lat,
                    label=as.character(dist_tot$pos),
                    labelOptions = labelOptions(noHide = T, textsize = "10px",textOnly = T, direction ="center")
                ) %>%
                
                
    # LEGENDS
                addLegend(
                    group = "Distances",
                    colors =c("darkgreen","green","orange","red","darkred"),
                    title = "Distance to the <br> Nearest parkrun event",
                    labels = c("&nbsp < 1 km","  1-2.5 km","2.5- 5 km","&nbsp 5 -10 km","&nbsp   > 10 km"),
                    labFormat = labelFormat(prefix = "", suffix = "km", between = " &ndash; ",
                                            digits = 0, big.mark = ",", transform = identity)
                ) %>%
                addLegend(
                    group = "IMD",
                    colors =c("darkgreen","green","orange","red","darkred"),
                    title = "Index of Multiple <br> Deprivation Quintiles",
                    labels = c("Least deprived","Less deprived","","More deprived","Most deprived")
                ) %>%
                addLegend(
                    group = "Participation",
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
                
                
                # leaflet options
                setView(0, 52, zoom = 7) %>%
                hideGroup("Distances") %>%
                hideGroup("IMD") %>%
                hideGroup("Participation") %>%
                hideGroup(popkm2.lab) %>%
                hideGroup("runs_imd") %>%
                hideGroup("runs_tot") %>%
                hideGroup("dist_imd") %>%
                hideGroup("dist_tot") 
                    
            
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
                                   
                
                output$text1 <- renderUI({
                    str1 <- paste("&nbsp  ",lsoa_name)
                    str2 <- paste("&nbsp &nbsp Code:", hit$code)
                    str3 <- paste("&nbsp &nbsp Population:",hit$pop)
                    str4 <- paste("&nbsp &nbsp ",round(hit$mn_dstn,2),"km to nearest event")
                    str5 <- paste("&nbsp  &nbsp ",round(hit$rns_pm_,3),"Runs per 1,000 per week")
                    HTML(paste(str1, str2,str3,str4,str5, sep = '<br/>'))
                    })
                }
            } else {
                output$text1 <- renderUI({
                    str1 <- paste("&nbsp  ")
                    str2 <- paste("&nbsp &nbsp Code:")
                    str3 <- paste("&nbsp &nbsp Population:")
                    str4 <- paste("&nbsp &nbsp ","km to nearest event")
                    str5 <- paste("&nbsp  &nbsp ","Runs per 1,000 per week")
                HTML(paste(str1, str2,str3,str4,str5, sep = '<br/>'))
                })
            }
                # # maybe also plot the area lines?
            # proxy <- leafletProxy("map")
            # proxy %>% clearPopups() %>%
              #  addPopups(click$lng, click$lat, text)
        })
        
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
