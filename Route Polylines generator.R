# Load necessary libraries
library(sf)
library(leaflet)
library(dplyr)

# Read Run 1 and Run 6 data
run1 <- read.csv("/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run1_0.339.csv", stringsAsFactors = FALSE)
run6 <- read.csv("/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run6_0.339.csv", stringsAsFactors = FALSE)

# Convert coordinates to decimal degrees for Run 1
run1$latitude <- as.numeric(substr(run1$latitude, 1, 2)) + (as.numeric(substr(run1$latitude, 3, nchar(run1$latitude))) / 60)
run1$longitude <- -(as.numeric(substr(run1$longitude, 1, 3)) + (as.numeric(substr(run1$longitude, 4, nchar(run1$longitude))) / 60))

# Convert coordinates to decimal degrees for Run 6
run6$latitude <- as.numeric(substr(run6$latitude, 1, 2)) + (as.numeric(substr(run6$latitude, 3, nchar(run6$latitude))) / 60)
run6$longitude <- -(as.numeric(substr(run6$longitude, 1, 3)) + (as.numeric(substr(run6$longitude, 4, nchar(run6$longitude))) / 60))

# Create the map
leaflet() %>%
  addTiles() %>%
  addProviderTiles("Thunderforest.MobileAtlas") %>%
  setView(lng = mean(c(run1$longitude, run6$longitude)), 
          lat = mean(c(run1$latitude, run6$latitude)), 
          zoom = 13) %>%
  addPolylines(data = run1, lng = ~longitude, lat = ~latitude, color = "blue", weight = 2.5, opacity = 0.8 ) %>%
  addPolylines(data = run6, lng = ~longitude, lat = ~latitude, color = "red", weight = 2.5, opacity = 0.8) %>%
  addLayersControl(
    
    #overlayGroups = c("Original Route (Run 1)", "Newly Designed Route (Run 6)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("blue", "red"),
    labels = c("Original Route (Run 1, 2, 3 and 4)", "Newly Designed Route (Run 5 and 6)"),
    title = "Sampling Routes",
    opacity = 1
  )
