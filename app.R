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
# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("IOLMAP"),
    sidebarLayout(
        position = "left",fluid=F,
        mainPanel(width=9,
                  leafletOutput('map',height = 700)
        ),
        sidebarPanel(
            width = 3,
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
                )
        )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        tiles <- "https://bitowaqr.github.io/iol_map/tiles/{z}/{x}/{y}.png"
        leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11), width = "100%") %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addTiles(tiles, options = tileOptions(opacity = 1)) %>%
            setView(0, 52, zoom = 5) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
