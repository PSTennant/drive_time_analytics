library(tidyverse)
library(mapgl)
library(mapboxapi)
library(tidycensus)
library(sf)
library(shiny)
library(shinythemes)
library(leaflet)
library(htmltools)
library(shinycssloaders)

# Loading tract data
tract_data <- readRDS("data/tract_data.rds")
tract_data <- st_as_sf(tract_data)

# Set Mapbox Access Token
if(!interactive()) {
  mb_access_token(Sys.getenv("mapbox_api_token"), install = TRUE, overwrite = TRUE)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Texas Drive Time Neighborhood Analysis"),
  theme = shinytheme("darkly"), # Add theme
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "height: 100vh;",
      textInput("address", "Enter Address"),
      sliderInput(
        "minutes", "Select Drive Time (Minutes)",
        min = 1, max = 60, value = 15, step = 1, "Shiny"
      ),
      selectInput("traffic", "Traffic Level", choices = c("Low", "Moderate", "High"), selected = "Moderate"),
      selectInput(
        "metric", "Metric", 
        choices = c(
          "Median Home Value (ACS)", 
          "Median Gross Rent (ACS)", 
          "Median Loan Amount (HMDA)", 
          "Median Age of Housing Units (HMDA)"
        ), 
        selected = "Median Age of Housing Units (HMDA)"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      width = 9, height = "100vh",
      style = "height: 100vh;",
      shinycssloaders::withSpinner(
        leafletOutput("map", height = "800px"),
        type = 6,
        color = "#293064",
        size = 1.25
      )
    )
))

# Define server logic
server <- function(input, output) {

  metric_name <- eventReactive(input$submit, {
    metric_name <- input$metric
    return(metric_name)
  })

  metric_var <- eventReactive(input$submit, {

    if (input$metric == "Median Home Value (ACS)") {
      metric_var <- "acs_median_home_value"
    }
    if (input$metric == "Median Gross Rent (ACS)") {
      metric_var <- "acs_median_gross_rent"
    }
    if (input$metric == "Median Loan Amount (HMDA)") {
      metric_var <- "hmda_median_loan_amount"
    }
    if (input$metric == "Median Age of Housing Units (HMDA)") {
      metric_var <- "hmda_tract_median_age_of_housing_units"
    }

    return(metric_var)

  })

  df <- eventReactive(input$submit, {

    if (input$traffic == "Low") {
      depart_at_var <- "2025-03-01T08:00"
    }
    if (input$traffic == "Moderate") {
      depart_at_var <- "2025-03-03T14:00"
    }
    if (input$traffic == "High") {
      depart_at_var <- "2025-03-05T17:30"
    }

    isochrone <- mb_isochrone(
      input$address,
      profile = "driving",
      time = input$minutes,
      depart_at = depart_at_var
    )

    isochrone_tracts <- sf::st_intersection(isochrone, st_transform(tract_data, 4326))

    return(isochrone_tracts)
  })

  output$map <- renderLeaflet({
    req(df())
    # req(df()) is a reactive expression that returns NULL if df() is not available,
    # and the value of df() if it is. This is used to prevent the map from rendering
    # until the user has submitted an address and a drive time.
    
    validate(need(length(df()) > 0, "Please enter a known address in Texas."))    

    pal <- colorRampPalette(c("#CCCCCC", "#293064"))
    df() %>%
      select(
        census_tract, # Using the rlang package to use the value of metric_var,
        estimate = metric_var(),
        geometry
      ) %>%
      mutate(estimate_fmtd = case_when(
        metric_name() == "Median Home Value (ACS)" ~ scales::dollar(estimate),
        metric_name() == "Median Gross Rent (ACS)" ~ scales::dollar(estimate),
        metric_name() == "Median Loan Amount (HMDA)" ~ scales::dollar(estimate),
        metric_name() == "Median Age of Housing Units (HMDA)" ~ paste(estimate, "years old"))) %>%
      drop_na() %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        fillColor = ~ pal(100)[as.numeric(cut(estimate, breaks = 100, labels = FALSE))],
        weight = .1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.75,
        highlightOptions = highlightOptions(weight = .4),
        label = ~ paste0(
          "Census Tract: ", census_tract
        ),
        popup = ~ paste0(
          "<b>Census Tract: </b>", census_tract, 
          "<br>",
          "<b>", metric_name(), ": </b>", estimate_fmtd
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
