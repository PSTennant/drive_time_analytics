library(tidyverse)
library(mapgl)
library(mapboxapi)
library(mapdeck)
library(tidycensus)
library(sf)

# tidycensus::load_variables(2023, "acs5") %>% View()

#mapboxgl()
# sccc20 <- mb_isochrone(
#   "11720 Joan of Arc Dr, Houston, TX 77024",
#   profile = "driving",
#   time = 20)
sccc20
saveRDS(sccc20, "data/sccc20.rds")
mapdeck() %>%
  add_polygon(
    data = sccc20, 
    fill_colour = alpha("#293064",.7),
    stroke_color = "#CC303D",
    stroke_width = 50
  )
# tx_tract_mhv <- get_acs(geography = "tract", state = "TX", year = 2023, geometry = TRUE, 
#         variables = c("B25077_001"))
saveRDS(tx_tract_mhv, "data/tx_tract_mhv.rds")
sccc20_tracts <- sf::st_intersection(sccc20, st_transform(tx_tract_mhv, 4326))
mapdeck(style = mapdeck_style("light")) %>%
  # add_polygon(
  #   data = sccc20, 
  #   fill_colour = alpha("#293064",.7),
  #   stroke_color = "#CC303D",
  #   stroke_width = 50) %>%
    add_polygon(
      data = sccc20_tracts, 
      fill_colour = "estimate",
      fill_opacity = .75,
      palette = grDevices::colorRamp(c("#EAEAEA", 
        "#293064", ))((1:256)/256),
      )