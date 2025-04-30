library(tidyverse)
library(mapgl)
library(mapboxapi)
library(tidycensus)
library(sf)
library(shiny)
library(shinythemes)
library(leaflet)
library(htmltools)

# Loading tract data
tract_data <- readRDS("data/tract_data.rds")
tract_data <- st_as_sf(tract_data)

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
          "hmda_tract_median_age_of_housing_units",
          "acs_median_gross_rent" ,
          "acs_median_home_value",
          "hmda_median_loan_amount"
        ),
        selected = "Home Value (ACS)"
      ),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      width = 9, height = "100vh",
      style = "height: 100vh;",
      leafletOutput("map", height = "100%")
    )
  )
)

# Define server logic
server <- function(input, output) {
  df <- eventReactive(input$submit, {

    # if (input$metric == "Age of Housing Units (HMDA)") {
    #   metric_var <- "hmda_tract_median_age_of_housing_units"
    # }

    # if (input$metric == "Gross Rent (ACS)") {
    #   metric_var <- "acs_median_gross_rent" 
    # }

    # if (input$metric == "Home Value (ACS)") {
    #   metric_var <- "acs_median_home_value"
    # }

    # if (input$metric == "Median Loan Amount (HMDA)") {
    #   metric_var <- "hmda_median_loan_amount"
    # }

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
    pal <- colorRampPalette(c("#CCCCCC", "#293064"))
    df() %>%
      select(
        census_tract, # Using the rlang package to use the value of metric_var,
        estimate = input$metric,
        geometry
      ) %>%
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
          "Census Tract: ", census_tract,
          ", Median Home Value: ", estimate
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
