#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(raster)
library(sp)
library(sf)


lsoa_sp = shapefile("./raw/lsoa_sp")
lsoa_sf = st_as_sf(lsoa_sp)
event_sp = shapefile("./raw/event_sp")


print("Data loaded");cat("Cat ")

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    titlePanel("IOLMAP"),
    
    leafletOutput('map'),
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
                  draggable = TRUE, bottom = 30, left = "auto", right = 20, top = "auto",
                  width = "280", height = 160,
                  h4(" Info:"),
                  
                      htmlOutput("text1")
                  
                  )
    
    
    
    # sidebarLayout(
    #     position = "left",fluid=F,
    #     mainPanel(width=9,
    #               leafletOutput('map',height = 700)
    #     ),
    #     sidebarPanel(
    #         width = 3,
    #         HTML("<button type='button' class='btn btn-danger' data-toggle='collapse' data-target='#demo'>Click me</button>"),
    #         
    #         
    #         
    #         div(id = "demo", class = "collapse",
    #             sliderInput(
    #                 inputId = "sld01_Mag",
    #                 label="Useless slider input",
    #                 min=1, max=10,
    #                 value=c(5)
    #             ),
    #             selectInput("add_layers","Show",choices = c("LSOA centroids","Something else"),multiple = T),
    #             div("hello", style = "font-size:75%")
    #         )
    #     )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        
        tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
        
        
        leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11), width = "100%") %>% 
            
            
            # provider tile
            addTiles(group="OSM") %>%
            addProviderTiles(providers$CartoDB.Positron, group="Carto") %>% 
            
            
            # my tile
            addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group="Distances") %>%
            
            
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
            
            
            
            # # complex LSOA layer # just takes too long to render
            # addPolygons(data = lsoa_sp, group = "Travel distances",
            #             color = "black", smoothFactor = 2,stroke = T,opacity = 0,
            #             weight = 0, fillOpacity = 0,fillColor = "cyan",
            #             highlight = highlightOptions(
            #                 weight = 1,color = "cyan",opacity = 1,bringToFront = FALSE,sendToBack = TRUE),
            #             popup = paste(
            #                 lsoa_sp$name,"<br>",
            #                 "Nearest event:", lsoa_sp$nrst_evnt,"<br>",
            #                 "Distance: ",round(lsoa_sp$mn_dstn,1)," km <br>",
            #                 "IMD score: ", lsoa_sp$imd_sc,"<br>",
            #                 "Population: ", lsoa_sp$pop,"<br>"
            #             )
            # ) %>%
        
        # addCircleMarkers(data = lsoa_sp, group = "LSOA info",
        #                  lng = lsoa_sp$cntr_ln,lat = lsoa_sp$cntr_lt,
        #                  radius = 2, color = "black",stroke = F,
        #                  fillOpacity = 0.9,fillColor = "black",
        #             
        #             popup = paste(
        #                 lsoa_sp$name,"<br>",
        #                 "Nearest event:", lsoa_sp$nrst_evnt,"<br>",
        #                 "Distance: ",round(lsoa_sp$mn_dstn,1)," km <br>",
        #                 "IMD score: ", lsoa_sp$imd_sc,"<br>",
        #                 "Population: ", lsoa_sp$pop,"<br>"
        #             )
        # ) %>%
        #     
            
            # leaflet options
            setView(0, 52, zoom = 7) %>%
            addLayersControl(
                baseGroups = c("OSM","Carto"),
                 overlayGroups = c("Distances"),
                options = layersControlOptions(collapsed = F,autoZIndex=T) 
            ) %>%
            hideGroup("Distances")
        
        
    })
    
    # click to get info
    observeEvent(input$map_click,ignoreNULL = F, {
        if(!is.null(input$map_click)){
            click_location <- input$map_click
            lng = click_location$lat
            lat = click_location$lng
            p <- st_as_sf(SpatialPoints(cbind(lat,lng),proj4string = CRS(" +proj=longlat +ellps=WGS84 +no_defs ")))
            index = as.numeric(st_intersects(p,lsoa_sf))
            hit = lsoa_sp@data[index,]
            
            lsoa_name = ifelse(nchar(hit$name)>30,
                               paste(substr(hit$name,1,30),"...",sep=""),
                               hit$name)
                               
            
            output$text1 <- renderUI({
                str1 <- paste("&nbsp  LSOA code:", hit$code)
                str2 <- paste("&nbsp  Name:",lsoa_name)
                str3 <- paste("&nbsp  LSOA pop:",hit$pop)
                str4 <- paste("&nbsp  Distance:",hit$mn_dstn)
                str5 <- paste("&nbsp  Part. rate:",round(hit$rn_rt_w,3))
                HTML(paste(str1, str2,str3,str4,str5, sep = '<br/>'))
                })
            
        } else {
            output$text1 <- renderUI({
            str1 <- paste("&nbsp  LSOA code:")
            str2 <- paste("&nbsp  Name:")
            str3 <- paste("&nbsp  LSOA pop:")
            str4 <- paste("&nbsp  Distance:")
            str5 <- paste("&nbsp  Part. rate:")
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
