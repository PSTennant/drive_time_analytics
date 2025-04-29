library(tidyverse)
library(mapgl)
library(mapboxapi)
library(mapdeck)
library(tidycensus)
library(sf)
library(shiny)
library(shinythemes)

# Loading tract data
tx_tract_mhv <- readRDS("data/tx_tract_mhv.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Texas Drive Time Neighborhood Analysis"),
  theme = shinytheme("spacelab"),  # Add theme
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Enter Address"),
      sliderInput("minutes", "Select Drive Time (Minutes)", 
                  min = 0, max = 60, value = 15, step = 1, "Shiny"),
      actionButton("submit", "Submit")
      ),

    mainPanel(
      textOutput("output"),
      mapdeckOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  df <- eventReactive(input$submit, {
    isochrone <-  mb_isochrone(
        input$address,
        profile = "driving",
        time = input$minutes)
    
    isochrone_tracts <- sf::st_intersection(isochrone, st_transform(tx_tract_mhv, 4326))

    return(isochrone_tracts)
  })

  output$map <- renderMapdeck({
    req(df())
    mapdeck(style = mapdeck_style("light")) %>%
      add_polygon(
        data = df(), 
        fill_colour = "estimate",
        fill_opacity = .75,
        palette = grDevices::colorRamp(c("#EAEAEA", 
        "#293064"))( (1:256)/256 ))
    })
  
  output$output <- renderText({
    req(df())
    paste("You entered:", input$address, "address and", input$minutes, "minutes")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

