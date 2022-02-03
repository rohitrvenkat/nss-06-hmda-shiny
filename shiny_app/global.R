library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(shinyDataFilter)
library(plotly)
library(patchwork)
library(shinycssloaders)

# Read in census data
census_data <- as.tibble(readRDS("data/census_data.rds"))

# Read in HMDA data
hmda_data <- readRDS("data/hmda_data_filtered.rds")

# Include column of each lender's annual amount lent for the given activity year
hmda_data <- hmda_data %>%
  inner_join(hmda_data %>% 
               group_by(`Institution Name`, Year) %>% 
               summarize(`Annual Money Lent` = sum(`Loan Amount`))
  )

hmda_groupby_state <- hmda_data %>%
  group_by(GEOID = State) %>%
  mutate(GEOID = "53") %>%
  summarize('Number of Loan Applications' = n(), 'Median Interest Rate' = median(`Interest Rate`, na.rm = T),
            'Median Loan Amount' = median(`Loan Amount`, na.rm = T),
            'Median Loan Cost' = median(`Total Loan Costs`, na.rm = T),
            'Median Total Points and Fees' = median(`Total Points and Fees`, na.rm = T),
            'Median Origination Charges' = median(`Origination Charges`, na.rm = T),
            'Median Loan Term' = median(`Loan Term`, na.rm = T),
            'Median Property Value' = median(`Property Value`, na.rm = T),
            'Median Debt-to-Income Ratio' = median(`Debt-to-Income Ratio`, na.rm = T))
hmda_groupby_county <- hmda_data %>%
  group_by(GEOID = County) %>%
  summarize('Number of Loan Applications' = n(), 'Median Interest Rate' = median(`Interest Rate`, na.rm = T),
            'Median Loan Amount' = median(`Loan Amount`, na.rm = T),
            'Median Loan Cost' = median(`Total Loan Costs`, na.rm = T),
            'Median Total Points and Fees' = median(`Total Points and Fees`, na.rm = T),
            'Median Origination Charges' = median(`Origination Charges`, na.rm = T),
            'Median Loan Term' = median(`Loan Term`, na.rm = T),
            'Median Property Value' = median(`Property Value`, na.rm = T),
            'Median Debt-to-Income Ratio' = median(`Debt-to-Income Ratio`, na.rm = T))
hmda_groupby_tract <- hmda_data %>%
  group_by(GEOID = `Census Tract`) %>%
  summarize('Number of Loan Applications' = n(), 'Median Interest Rate' = median(`Interest Rate`, na.rm = T),
            'Median Loan Amount' = median(`Loan Amount`, na.rm = T),
            'Median Loan Cost' = median(`Total Loan Costs`, na.rm = T),
            'Median Total Points and Fees' = median(`Total Points and Fees`, na.rm = T),
            'Median Origination Charges' = median(`Origination Charges`, na.rm = T),
            'Median Loan Term' = median(`Loan Term`, na.rm = T),
            'Median Property Value' = median(`Property Value`, na.rm = T),
            'Median Debt-to-Income Ratio' = median(`Debt-to-Income Ratio`, na.rm = T))
hmda_census_data <- bind_rows(hmda_groupby_state, hmda_groupby_county, hmda_groupby_tract)
census_data <- merge(census_data, hmda_census_data, by = "GEOID")
choro_variables <- variable.names(subset(census_data, select=c("Population",
                                                               "Number of Loan Applications",
                                                               "Median Income",
                                                               "Median Interest Rate",
                                                               "Median Loan Amount",
                                                               "Median Loan Cost",
                                                               "Median Total Points and Fees",
                                                               "Median Origination Charges",
                                                               "Median Loan Term",
                                                               "Median Property Value",
                                                               "Median Debt-to-Income Ratio")))

census_geoid_name <- census_data %>%
  select(GEOID, NAME)

hmda_geoid_state<- hmda_data %>%
  mutate(GEOID = "53") %>%
  inner_join(census_geoid_name)

hmda_geoid_county <- hmda_data %>%
  mutate(GEOID = County) %>%
  inner_join(census_geoid_name)

hmda_geoid_tract <- hmda_data %>%
  mutate(GEOID = `Census Tract`) %>%
  inner_join(census_geoid_name)

hmda_geoid_name <- bind_rows(hmda_geoid_state, hmda_geoid_county, hmda_geoid_tract)

hmda_race_data <- hmda_geoid_name %>%
  group_by(NAME) %>%
  count(Race, name = "Population") %>%
  mutate(Group = "HMDA")

levels(hmda_race_data$Race) <- c("Two Or More Races", 
                                 "American Indian", 
                                 "Asian", "Black", 
                                 "Free Form Text Only", 
                                 "Joint", 
                                 "Pacific Islander", 
                                 "Race Not Available", 
                                 "White")

census_race_data <- census_data %>%
  gather(key = "Race", value = "Population", White, Black, `American Indian`, Asian, `Pacific Islander`, `Other Race`, `Two Or More Races`) %>%
  mutate(Group = "Census") %>%
  select(NAME, Race, Population, Group)

hmda_census_race_data <- bind_rows(hmda_race_data, census_race_data)

# Initialize leaflet map function
draw_base_map <- function() {
  leaflet(
    options = leafletOptions(minZoom = 5, maxZoom = 14)
  ) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -120.7401, lat = 47.7511, zoom = 6) %>%
    addResetMapButton() %>%
    addEasyButton(
      easyButton(icon = htmltools::HTML("<i class='fas fa-filter'></i>"),
                 title = "Filter",
                 onClick = JS("function(btn, map) {
                              Shiny.onInputChange('filterButton', '1');
                              Shiny.onInputChange('filterButton', '2');
                              }")
      )
    )
}

pal <- colorNumeric(palette = "viridis", domain = NULL)

# Update choropleth function
update_choropleth <- function(mymap, census_data, chor_vars) {
  
  census_data$label <-
    paste0("<b>", census_data$NAME, "</b><br>",
           str_to_title(chor_vars), ": ", census_data[[chor_vars]]) %>%
    lapply(htmltools::HTML)
  
  leafletProxy(mymap, data = census_data) %>% 
    clearShapes() %>% 
    addPolygons(
      data = census_data$geometry,
      layerId = census_data$NAME,
      group = "name",
      stroke = TRUE,
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      fillColor = pal(census_data[[chor_vars]]),
      label = census_data$label,
      highlight = highlightOptions(
        weight = 3,
        fillOpacity = 0.9,
        color = "#666",
        bringToFront = FALSE) 
    )
}

# Draw map legend function
draw_map_legend <- function(mymap, census_data, chor_vars) {
  leafletProxy(mymap, data = census_data) %>%
    clearControls() %>%
    addLegend(
      "bottomleft",
      pal = pal, 
      values = census_data[[chor_vars]], # need to change with input
      title = ~ str_to_title(chor_vars), # needs to change with input
      opacity = 1
    )
}

# Check zoom level
check_zoom <- function(zoom) {
  case_when(
    zoom <= 6 ~ "state",
    zoom <= 7 ~ "county",
    TRUE ~ "tract"
  )
}
