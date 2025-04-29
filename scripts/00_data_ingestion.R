#                                00 Data Ingestion Script
#
# Created:
# Author: Patrick
# Purpose: Loading External Texas Census Tract Data for Use in Drive Time Analysis App
#
# =============================================================================================================
# Loading Necessary Libraries and Checking Location
library(tidyverse)
library(tidycensus)

# ========================================== HMDA ====================================================

# Loading Home Mortgage Disclosure Act Data
hmda <- readxl::read_excel("data/dwelling_categories_Single Family (1-4 Units)_Site-Built_state_TX.xlsx")
# Prepping HMDA
hmda <- hmda %>%
  filter(activity_year == 2023) %>%
  select(census_tract, loan_amount, tract_median_age_of_housing_units) %>%
  mutate(census_tract = as.character(census_tract)) %>% 
  group_by(census_tract) %>%
  summarise(
    hmda_median_loan_amount = median(loan_amount),
    hmda_tract_median_age_of_housing_units = mean(tract_median_age_of_housing_units)
  )

# ========================================== ACS ====================================================

# Loading ACS from tidycensus
acs <- get_acs(
  geography = "tract", state = "TX", year = 2023, geometry = TRUE, output = "wide",
  variables = c(
    acs_median_home_value = "B25077_001",
    acs_median_gross_rent = "B25064_001"
  )
)
# Prepping ACS
acs <- acs %>%
  select(
    GEOID,
    acs_median_home_value = acs_median_home_valueE,
    acs_median_gross_rent = acs_median_gross_rentE,
    geometry
  )

# ========================================== Merging to Final DF =================================================

# Merging the two data sets
tract_data <- full_join(hmda, acs, by = c("census_tract" = "GEOID"))

# and saving the results
saveRDS(tract_data, "data/tract_data.rds")
