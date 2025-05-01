# Purpose
Automated mapping that will display Census tract-level information about areas within a certain drive time from a focal address in Texas. 

# Details
The map displays all of the Census tracts (or partial Census tracts) located within the selected drive time on a map (i.e., an isochrone). Each Census tract within the isochrone is shaded by its relative performance on the selected metric. 

# References
I'm grateful to the developers and maintainers of many useful software products, open source packages, and data sets that made this work possible. A few to highlight include:

- Kyle Walker for the mapboxapi (https://walker-data.com/mapboxapi/index.html) and tidycensus packages (https://walker-data.com/tidycensus/)
- The entire Posit and Tidyverse teams (https://www.tidyverse.org/)
- The Leaflet folks for mapping software (https://rstudio.github.io/leaflet/)
- The American Community Survey (ACS; https://www.census.gov/programs-surveys/acs/data.html) and Home Mortgage Disclosure Act (HDMA; https://ffiec.cfpb.gov/data-browser/) teams for data collection. 
