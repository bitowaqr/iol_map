
# app prototype I

library(shiny)
library(leaflet)
library(raster)
library(sp)
library(sf)


lsoa_sf = read_sf("./raw/lsoa_sf.shp")
event_sp = shapefile("./raw/event_sp")
print("Data loaded")

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    # titlePanel("IOLMAP"),
    
    
    leafletOutput('map'),
absolutePanel(id = "title", class = "panel panel-default", fixed = TRUE,
              draggable = T, bottom = "auto", left = 50, right = "auto", top = 5,
              width = "auto", height = "auto",
              h4("- Identifying Optimal Locations to Maximise Access to parkrun events -")
              
),
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 160, right = "auto", left = 20, bottom = "auto",
                  width = 330, height = "auto",
                  HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#demo'>Click me</button>"),
                  div(id = "demo", class = "collapse",
                      sliderInput(
                          inputId = "sld01_Mag",
                          label="Useless slider input",
                          min=1, max=10,
                          value=c(5)
                      ),
                      selectInput("add_layers","Show",choices = c("LSOA centroids","Something else"),multiple = T),
                      div("hello", style = "font-size:75%")
                  )),
    absolutePanel(id = "info", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, bottom = 80, left = "auto", right = 10, top = "auto",
                  width = "280", height = 160,
                  h4(" LSOA info:"),
                  
                      htmlOutput("text1")
                  
                  )
)


server <- function(input, output) {
    
    output$map <- renderLeaflet({
        
        tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
        tiles_participation <- "https://bitowaqr.github.io/iol_map/tiles_participation/{z}/{x}/{y}.png"
        tiles_imd <- "https://bitowaqr.github.io/iol_map/tiles_imd/{z}/{x}/{y}.png"
        tiles_pop_km2 <- "https://bitowaqr.github.io/iol_map/tiles_pop_km2/{z}/{x}/{y}.png"
        
        
        leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12), width = "100%") %>% 
            
            # provider tile
            addTiles(group="OSM") %>%
            addProviderTiles(providers$CartoDB.Positron, group="Carto") %>% 

            # my tile
            addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group="Distances") %>%
            addTiles(tiles_participation, options = tileOptions(opacity = 0.5),group="Participation") %>%
            addTiles(tiles_imd, options = tileOptions(opacity = 0.5),group="IMD") %>%
            addTiles(tiles_pop_km2, options = tileOptions(opacity = 0.5),group="popkm2") %>%
            
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
                group = "popkm2",
                colors =c("darkgreen","green","orange","red","darkred"),
                title = "Population density",
                labels = c("Least dense","Less dense","","More dense","Most dense")
            ) %>%
            
            
            # leaflet options
            setView(0, 52, zoom = 7) %>%
            addLayersControl(
                baseGroups = c("OSM","Carto"),
                overlayGroups = c("Distances","IMD","Participation","popkm2"),
                options = layersControlOptions(collapsed = F,autoZIndex=T) 
            ) %>%
            hideGroup("Distances") %>%
            hideGroup("IMD") %>%
            hideGroup("Participation") %>%
            hideGroup("popkm2")
        
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
