
# parkrun iolmap app prototype I

# load packages
    library(shiny)
    library(leaflet)
    library(raster)
    library(sp)
    library(sf)
    library(DT)
    library(shinyjs)
    
 # loading screen   
    appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  padding-top: 250px;
  opacity: 0.9;
  z-index: 100;
  font-size:30px;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: cyan;
}
"
    
    print("packages loaded")
    
# urls to get custom tiles
    tiles_distance <- "https://bitowaqr.github.io/iol_map/tiles_distance/{z}/{x}/{y}.png"
    tiles_participation <- "https://bitowaqr.github.io/iol_map/tiles_participation/{z}/{x}/{y}.png"
    tiles_imd <- "https://bitowaqr.github.io/iol_map/tiles_imd/{z}/{x}/{y}.png"
    tiles_pop_km2 <- "https://bitowaqr.github.io/iol_map/tiles_pop_km2/{z}/{x}/{y}.png"
    tiles_greenspaces <- "https://bitowaqr.github.io/iol_map/tiles_greenspaces/{z}/{x}/{y}.png"
    
# global tiles and marker labels
    popkm2.lab = "Population density"
    dist_label = "Distance to nearest event"
    imd_label = "IMD Score"
    part_label = "parkrun participation"
    greens_label = "Greenspaces considered"
    runs_imd_label = "max Runs * IMD<sup>2</sup>"
    runs_tot_label = "max total Runs"
    dist_imd_label  = "min Distances * IMD<sup>2</sup>"
    dist_tot_label = "min total Distances"
    parkrun_label = "Established parkrun events"
    


# load data
    # lsoa data
    lsoa_sf = read_sf("./raw/lsoa_sf.shp")
    # established events data
    event_sp = shapefile("./events/event_sp")
    
    # proposed new event locations
    runs_imd = read_sf("./events/opt_loc_runs_imd.prep.shp")
    runs_tot = read_sf("./events/opt_loc_runs.prep.shp")
    dist_imd = read_sf("./events/opt_loc_dist_imd.prep.shp")
    dist_tot = read_sf("./events/opt_loc_dist.prep.shp")
    
    # create data frame for table
    runs_imd.df <- (runs_imd %>% st_set_geometry(NULL))[,c("pos","distName1","lon","lat")]
    runs_imd.df$set = runs_imd_label
    runs_tot.df <- (runs_tot %>% st_set_geometry(NULL))[,c("pos","distName1","lon","lat")]
    runs_tot.df$set = runs_tot_label
    dist_imd.df <- (dist_imd %>% st_set_geometry(NULL))[,c("pos","distName1","lon","lat")]
    dist_imd.df$set = dist_imd_label
    dist_tot.df <- (dist_tot %>% st_set_geometry(NULL))[,c("pos","distName1","lon","lat")]
    dist_tot.df$set = dist_tot_label
    # combine
    recommendations.df = ( rbind(runs_imd.df,runs_tot.df,dist_imd.df,dist_tot.df))
    recommendations.df$greenspace_name = recommendations.df$distName1
    
    # green spaces considered in the analysis
    greens_coordinates = read.csv("./raw/greens_coordinates.csv")
    print("Data loaded")
    
# User Interface
    ui <- bootstrapPage(
        
        useShinyjs(),
        inlineCSS(appCSS),
        
        # Loading message
        div(
            id = "loading-content",
            HTML("Just one moment please <br><br> Loading...")
        ),
        
        
        tags$style(type = "text/css", 
        "#map {height: calc(100vh ) !important;
        padding-top: 10px;
        padding-left: 10px;
                   }"),
        
        # leaflet map output
        leafletOutput('map'),
        
        # RECOMMENDATION SELECTION PANEL
        absolutePanel(id = "recommendations",  fixed = TRUE,
                      draggable = TRUE, top = 230, right = 10, left = "auto", bottom = "auto",
                      width = 220, height = "auto",
                      div(id="selection_boxes",
                          style="border: 2px solid #CCC; background-color: rgb(255, 255,255,1);padding:10px; font-size:100%",
                          checkboxGroupInput(inputId = "rec_choice",
                                             label = "New parkrun event locations:",
                                             choiceNames = list(
                                                 tags$span("max. IMD-weighted runs", style = "color: orange;"),
                                                 tags$span("max. total runs", style = "color: magenta;"),
                                                 tags$span("min. IMD-weighted distances", style = "color: darkgreen;"),
                                                 tags$span("min. total distances", style = "color: slateblue;")),
                                                                
                                             choiceValues = list(runs_imd_label,
                                                                 runs_tot_label,
                                                                 dist_imd_label,
                                                                 dist_tot_label),
                                             selected = NULL)),
                      div(dataTableOutput('table01'), style = "border-bottom: 2px solid #CCC; background-color: rgb(250, 250,250,1);font-size:70%")
                      ),
        
        # QUICK HELP PANEL
        absolutePanel(id = "controls",  fixed = TRUE,
                      draggable = TRUE, top = 20, right = "auto", left = 50, bottom = "auto",
                      width = "auto", height = "auto",
                      # info text
                      div(id = "button",style="border-bottom: 1px solid #CCC; background-color: rgb(250, 250,250,0);",
                          tags$head(
                              tags$style(HTML('#show{background-color:cyan}'))
                          ),
                          actionButton("show", "Quick Help"))),
                      # HTML("<button type='button' class='btn btn-info' data-toggle='collapse' data-target='#demo'>Quick help</button>")
                      # ),
                      # div(id = "demo", class = "collapse",
                      #     style="border: 1px solid #CCC; background-color: rgb(250, 250,250,0.9); width: 700px;",
                      #     HTML("<br>This map shows the results of a research project, aimed at supporting parkrun's 
                      #          plan to create 200 new parkrun events across England. More about the study can be found "),
                      #     
                      #     HTML("<a href='https://github.com/bitowaqr/iolmap_analysis' target='_blank'>here</a>"),
                      #     HTML(".<hr>"),
                      #     
                      #     HTML("<strong>Tutorial</strong><br>
                      #          Space for a short and good explanation of the important features of this map..."),
                      #     HTML("<br><hr>Contact: ...")
                      #     
                      # )
                      # ),
        
        # LSOA INFO PANEL
        absolutePanel(id = "info",  fixed = TRUE,
                      draggable = TRUE, bottom = 30, left = 10, right = "auto", top = "auto",
                      width = "280", height = "auto",
                      
                      div(id = "lsoainfo", 
                          style="border: 2px solid #CCC; background-color: rgb(255, 255,255,1); padding: 10px; font-size:80%",
                          HTML(" <strong>LSOA summary:</strong>"),
                          htmlOutput("text1")
                          )
        )
    )
    
    
    server <- function(input, output) {
        
        
        observeEvent(input$show, {
            showModal(modalDialog(
                title = "Identifying Optimal Locations for Maximising Access to parkun",
                
                HTML("This map shows the results of a research project, aimed at supporting parkrun's plan to create 200 new parkrun events across England. More about the study can be found "),
                     HTML("<a href='https://github.com/bitowaqr/iolmap_analysis' target='_blank'>here</a>. <br><br>"),
                HTML('<img src="https://github.com/bitowaqr/iol_map/raw/master/tooltip.png" width="600" > <hr>'),
                HTML("Contact:..."),
                size="l",
                easyClose = TRUE,
                footer = modalButton("Close")
            ))
        })
            
        output$map <- renderLeaflet({
            

            ref = tags$div(id="ref",
            style="padding-left: 10px;
                    padding-right: 10px;
                    background: rgba(245,245,245,0.5);
                    font-size: 12px;",
            'Schneider et al. Identifying Optimal Locations to Maximise Access to parkrun events. 2019. Data & code available at',
            tags$a(href="https://github.com/bitowaqr/iolmap_analysis", "Github",target="_blank"))
            
            
####### ---->  BUILD LEAFLET MAP   
            
        map <- leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12), width = "100%") %>% 
    
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
                                      popkm2.lab,imd_label,dist_label,part_label
                                      #,runs_imd_label,runs_tot_label,dist_imd_label,dist_tot_label
                                      ),
                    options = layersControlOptions(collapsed = F,autoZIndex=F,opacity=0) 
                ) %>%
        
    # add my tiles
                addTiles(tiles_distance, options = tileOptions(opacity = 0.5),group=dist_label) %>%
                addTiles(tiles_participation, options = tileOptions(opacity = 0.5),group=part_label) %>%
                addTiles(tiles_imd, options = tileOptions(opacity = 0.5),group=imd_label) %>%
                addTiles(tiles_pop_km2, options = tileOptions(opacity = 0.5),group=popkm2.lab) %>%
                addTiles(tiles_greenspaces, options = tileOptions(opacity = 0.5),group=greens_label) %>%
                
    # established parkrun events
                addCircles(
                    group = parkrun_label,
                    data = event_sp,
                    # lng = ~lon,lat = ~lat,
                    radius = 100,fillColor = "blue",
                    stroke = F, fillOpacity = 1,
                    opacity = 1,color="blue"
                ) %>%
                addCircles(
                    group = parkrun_label,
                    data = event_sp,
                    # lng = ~lon,lat = ~lat,
                    radius = 2500,fillColor = "blue",
                    stroke = F, fillOpacity = 0.4,
                    opacity = 1,color="blue",weight = 1,
                    popup = paste("parkrun event:",event_sp$course,"<br>",
                                  "Established:", event_sp$Estblsh,"<br>",
                                  "Served pop:", round(event_sp$srvd_pop),"<br>",
                                  "Mean participants:", round(event_sp$Mn_prtc),"<br>",
                                  "Mean volunteers:", round(event_sp$Mn_vlnt))
                ) %>%
                
                
    # Greenspaces considered
                addCircles(data=greens_coordinates,
                                 lng = ~lng, lat = ~lat,
                                 radius = ~sqrt(greens_coordinates$area_km2*1000^2), 
                                 stroke = F,
                                 fillColor="white",fillOpacity=0,
                                 group = greens_label,
                                 popup = paste("Name: ",greens_coordinates$name,"<br>",
                                               "Type: ", greens_coordinates$type,"<br>",
                                               "Area (km<sup>2</sup>): ", round(greens_coordinates$area_km2,2),"<br>",
                                               "Coordinates: ", round(greens_coordinates$lng,5),"; ", round(greens_coordinates$lat,5),"<br>",
                                               "<a href='https://www.google.com/maps/@",greens_coordinates$lat,",",greens_coordinates$lng,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                                               sep=""
                                               
                                 )
                                 
                ) %>%
                
    # NEW EVENTS 
        # RUNS with IMD weights
            addCircles(
                data = runs_imd,
                group = runs_imd_label,
                radius = 100,fillColor = "orange",
                stroke = F, fillOpacity = 1,
                lng = ~lon, lat = ~lat
            ) %>%
                addCircles(
                    data = runs_imd,
                    group = runs_imd_label,
                    radius = 2500,fillColor = "orange",
                    stroke = F, fillOpacity = 0.4,
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
            addCircles(
                data = runs_tot,
                group = runs_tot_label,
                radius = 100,fillColor = "magenta",
                stroke = F, fillOpacity = 1,
                lng = ~lon, lat = ~lat
            ) %>%
            addCircles(
                data = runs_tot,
                group = runs_tot_label,
                radius = 2500,fillColor = "magenta",
                stroke = F, fillOpacity = 0.4,
                popup = paste("Name: ",runs_tot$distName1,"<br>",
                              "Rank: ", runs_tot$pos,"<br>",
                              "Objective improvement: ",runs_tot$change, "%<br>",
                              "Coordinates: ", round(runs_tot$lat,5), ", ",round(runs_tot$lon,5),"<br>",
                              "<a href='https://www.google.com/maps/@",runs_tot$lat,",",runs_tot$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                              sep=""),
                lng = ~lon, lat = ~lat,
                label=as.character(runs_tot$pos),
                labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
            ) %>%
        # DIST with IMD weights
            addCircles(
                data = dist_imd,
                group = dist_imd_label,
                radius = 100,fillColor = "darkgreen",
                stroke = F, fillOpacity = 1,
                lng = ~lon, lat = ~lat
            ) %>%
            addCircles(
                data = dist_imd,
                group = dist_imd_label,
                radius = 2500,fillColor = "darkgreen",
                stroke = F, fillOpacity = 0.4,
                popup = paste("Name: ",dist_imd$distName1,"<br>",
                              "Rank: ", dist_imd$pos,"<br>",
                              "Objective improvement: ",dist_imd$change, "%<br>",
                              "Coordinates: ", round(dist_imd$lat,5), ", ",round(dist_imd$lon,5),"<br>",
                              "<a href='https://www.google.com/maps/@",dist_imd$lat,",",dist_imd$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                              sep=""),
                lng = ~lon, lat = ~lat,
                label=as.character(dist_imd$pos),
                labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
            ) %>%
            
        # DIST TOTAL
        addCircles(
            data = dist_tot,
            group = dist_tot_label,
            radius = 100,fillColor = "slateblue",
            stroke = F, fillOpacity = 1,
            lng = ~lon, lat = ~lat
        ) %>%
        addCircles(
            data = dist_tot,
            group = dist_tot_label,
            radius = 2500,fillColor = "slateblue",
            stroke = F, fillOpacity = 0.4,
            popup = paste("Name: ",dist_tot$distName1,"<br>",
                          "Rank: ", dist_tot$pos,"<br>",
                          "Objective improvement: ",dist_tot$change, "%<br>",
                          "Coordinates: ", round(dist_tot$lat,5), ", ",round(dist_tot$lon,5),"<br>",
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
                    title = "Distance to the <br> nearest parkrun event",
                    labels = c("0.0 < &nbsp 1.0 km",
                               "1.0 - &nbsp 2.5 km",
                               "2.5 - &nbsp 5.0 km",
                               "5.0 - 10.0 km",
                               "&nbsp&nbsp&nbsp&nbsp&nbsp  >10.0 km"),position = "topleft",
                    labFormat = labelFormat(prefix = "", suffix = "km", between = " &ndash; ",
                                            digits = 0, big.mark = ",", transform = identity)
                ) %>%
                addLegend(
                    group = imd_label,
                    colors =c("darkgreen","green","orange","red","darkred"),
                    position = "topleft",
                    title = "Index of Multiple <br> Deprivation quintiles",
                    labels = c("&nbsp 0.5 - &nbsp 8.4 (lowest)",
                               "&nbsp 8.4 - 14.0 (low)",
                               "14.0 - 21.4 (medium)",
                               "21.4 - 33.9 (high)",
                               "33.9 - 92.6 (highest)")
                ) %>%
                addLegend(
                    group = part_label,
                    colors =c("darkred","red","orange","green","darkgreen"),
                    position = "topleft",
                    title = "parkrun participation <br> per 1,000 pop. per week",
                    labels = c("&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp0 runs",
                               "&nbsp &nbsp0 - 0.5 runs",
                               "0.5 - 1.0 runs",
                               "1.0 - 2.0 runs",
                               "&nbsp &nbsp&nbsp > 2.0 runs")
                ) %>%
                addLegend(
                    group = popkm2.lab,
                    colors =c("darkgreen","green","orange","red","darkred"),
                    position = "topleft",
                    title = "Population density <br> quintiles (pop/km<sup>2</sup>)",
                    labels = c("&nbsp &nbsp &nbsp&nbsp 2 - &nbsp&nbsp 822",
                               "&nbsp&nbsp 822 - 2,676",
                               "2,676 - 4,430",
                               "4,430 - 6,612",
                               "6,612 - 99,024")
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
    
            map
            
                    
            
        })
    
    prev_row <- reactiveVal()
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
    
    
    # INPUT OBSERVER
    observe({
        
        # print(input$rec_choice)
        
        possible_choices = c(runs_imd_label,
                             runs_tot_label,
                             dist_imd_label,
                             dist_tot_label)
        selected_recommendations = possible_choices %in% input$rec_choice
        unselected_recommendation = !selected_recommendations
        sum_selection = sum(selected_recommendations)
        # update map
        leafletProxy("map") %>%
            hideGroup(possible_choices[unselected_recommendation]) %>%
            showGroup(possible_choices[selected_recommendations]) 
        
        # update table
        # draw table raw
        if(sum_selection==1){
        output$table01 <- renderDataTable({
            
            
            
            recommendations.df_selected_rows = recommendations.df$set %in% possible_choices[selected_recommendations]
            
            DT::datatable(data=recommendations.df[recommendations.df_selected_rows,c("pos","greenspace_name")],height=200,
                          options=list(searching=F,pageLength = 10,lengthMenu=c(5,30,50), lengthChange = FALSE,rownames= FALSE,stateSave = TRUE),
                          class="compact",rownames=F,selection = "single")
        })
        }
        if(sum_selection<1){
            output$table01 <- renderDataTable({
                
                
                DT::datatable(data=data.frame(pos =NA,greenspace_name= "Select a set of location recommendation set from above."),
                              options=list(searching=F,pageLength = 10, lengthChange = FALSE,rownames= FALSE,stateSave = TRUE),
                              class="compact",rownames=F,selection = "single")
            
            })
        }
        if(sum_selection>1){
            output$table01 <- renderDataTable({
                
                
                DT::datatable(data=data.frame(pos =NA,greenspace_name= "Cannot show details for more than 1 set of location recommendations."),
                              options=list(searching=F,pageLength = 10, lengthChange = FALSE,rownames= FALSE,stateSave = TRUE),
                              class="compact",rownames=F,selection = "single")
                
            })
        }
            
            
            
        
        
        })
    
    
    
    # DATATABLE OBSERVER
    observeEvent(input$table01_rows_selected, {
        
        possible_choices = c(runs_imd_label,
                             runs_tot_label,
                             dist_imd_label,
                             dist_tot_label)
        selected_recommendations = possible_choices %in% input$rec_choice
        recommendations.df_selected_rows = recommendations.df$set %in% possible_choices[selected_recommendations]
        
        row_selected = recommendations.df[recommendations.df_selected_rows,][input$table01_rows_selected,]
        
        #print(row_selected)
        # HIGHLIGHT
        leafletProxy('map') %>%
            addCircleMarkers(layerId =  paste(row_selected$lon,row_selected$lat,row_selected$set),
                             group = row_selected$set,
                             lng=row_selected$lon,
                             lat=row_selected$lat,
                             popup = paste("Name: ",row_selected$distName1,"<br>",
                                           "Rank: ", row_selected$pos,"<br>",
                                           "Objective improvement: ",row_selected$change, "%<br>",
                                           "Coordinates: ", round(row_selected$lat,5), ", ",round(row_selected$lon,5),"<br>",
                                           "<a href='https://www.google.com/maps/@",row_selected$lat,",",row_selected$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                                           sep=""),
                             radius=10,opacity = 1,color=NULL,stroke = F,fillColor="red",fillOpacity =  1,
                             label=as.character(row_selected$pos),
                             labelOptions = labelOptions(noHide = T, textsize = "10px",textOnly = T,  direction ="center",clickable=T))
        
        
        
        if(!is.null(prev_row())){
            leafletProxy('map') %>%
                removeMarker(paste(prev_row()$lon,prev_row()$lat,prev_row()$set))
                    
                    
        }
        # set new value to reactiveVal 
        prev_row(row_selected)
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
                str3.5 <- paste("&nbsp Density: ",hit$pop_km2,"/km<sup>2</sup>",sep="")
                str3.9 <- paste("&nbsp IMD Score:",round(hit$imd_sc,1))
                str4 <- paste("&nbsp ",round(hit$mn_dstn,2),"km to nearest parkrun event")
                str5 <- paste("&nbsp ",round(hit$rns_pm_,3),"Runs per 1,000 per week")
                HTML(paste(str1, str3,str3.5,str3.9,str4,str5, sep = '<br/>'))
            })
            
            
            
        })
        
        hide(id = "loading-content", anim = TRUE, animType = "fade",time = 3)    
        show("app-content")
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
