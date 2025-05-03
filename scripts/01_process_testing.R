#                                01 Process Testing Script
#
# Created: 2025-04-28
# Author: Patrick
# Purpose: Exploring the Isochrone API and merging with tract data
#
# =============================================================================================================
# Loading Necessary Libraries and Checking Location
library(tidyverse)
library(mapgl)
library(mapboxapi)
library(mapdeck)
library(tidycensus)
library(sf)
library(leaflet)

# ========================================== Loading Data ====================================================

# Isochrone data example
# sccc20 <- mb_isochrone(
#   "11720 Joan of Arc Dr, Houston, TX 77024",
#   profile = "driving",
#   time = 20)
# saveRDS(sccc20, "data/sccc20.rds")
sccc20 <- readRDS("archive/sccc20.rds")
# sccc20

# Tract Content Data
tract_data <- readRDS("data/tract_data.rds")
tract_data <- st_as_sf(tract_data)

# and merging them into one shape file via intersecting geometries
sccc20_tracts <- sf::st_intersection(sccc20, st_transform(tract_data, 4326))


# ========================================== EDA ====================================================

sapply(tract_data, summary)
sapply(sccc20_tracts, summary)

# ========================================== Example Maps ====================================================

# Mapdeck is okay, but I think I'll use leaflet
mapdeck() %>%
  add_polygon(
    data = sccc20,
    fill_colour = alpha("#293064", .7),
    stroke_color = "#CC303D",
    stroke_width = 50
  )
mapdeck(style = mapdeck_style("light")) %>%
  add_polygon(
    data = sccc20_tracts %>%
      select(
        estimate = hmda_tract_median_age_of_housing_units,
        geometry
      ) %>% drop_na(),
    fill_colour = "estimate",
    fill_opacity = .75
  )

# Leaflet version does more of what I want
pal <- colorRampPalette(c("#EAEAEA", "#293064"))
sccc20_tracts %>%
  select(
    census_tract,
    estimate = acs_median_home_value,
    geometry
  ) %>% 
  drop_na() %>% 
  leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    addPolygons(
      fillColor = ~pal(100)[as.numeric(cut(estimate, breaks = 100, labels = FALSE))],
      weight = .1,
      opacity = 1,
      color = "green",
      fillOpacity = 0.6,
      highlightOptions = highlightOptions(weight = .3),
      label = ~as.character(census_tract),
      popup = ~as.character(estimate)
    )
