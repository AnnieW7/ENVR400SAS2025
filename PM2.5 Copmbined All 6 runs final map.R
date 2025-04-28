# Load necessary libraries
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(gstat)
library(leaflet)
library(sp)
library(zoo)  # For rolling averages

# List of run files
data_files <- c(
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/PM2.5Quad/run1_quad.csv"
)

# Initialize dataset_all as NULL to handle first assignment properly
dataset_all <- NULL

# Read and preprocess all runs
for (file in data_files) {
  ds <- read.csv(file, stringsAsFactors = FALSE)
  ds$datetime <- as.POSIXct(ds$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # Convert coordinates to decimal degrees
  ds$latitude <- as.numeric(substr(ds$latitude, 1, 2)) + (as.numeric(substr(ds$latitude, 3, nchar(ds$latitude))) / 60)
  ds$longitude <- -(as.numeric(substr(ds$longitude, 1, 3)) + (as.numeric(substr(ds$longitude, 4, nchar(ds$longitude))) / 60))
  
  # Compute 30-second rolling average for PM2.5
  ds <- ds %>%
    arrange(datetime) %>%
    mutate(PM2.5_avg = rollmean(PM2.5, k = 30, fill = NA, align = "right")) %>%
    na.omit()
  
  # Combine all datasets
  dataset_all <- if (is.null(dataset_all)) ds else bind_rows(dataset_all, ds)
}

# Compute percentiles for color scaling
quantiles <- quantile(dataset_all$PM2.5_avg, probs = c(0, 0.6, 0.7, 0.8,0.9,0.925, 0.95, 0.975, 0.99, 1.0), na.rm = TRUE)

# Define color palette
color_palette <- colorBin(
  palette = c("#D0EFFF", "lightyellow", "yellow", "#FFD700", "#FF8C00", "#FF6600", "red", "#B22222", "darkred", "#550000", "#8000FF"),
  domain = dataset_all$PM2.5_avg,
  bins = quantiles,
  na.color = "#000000"
)

# Function to generate heatmap
generate_heatmap <- function(ds) {
  coordinates(ds) <- ~longitude + latitude
  proj4string(ds) <- CRS("+proj=longlat +datum=WGS84")
  
  # Create interpolation grid
  x.range <- range(ds$longitude)
  y.range <- range(ds$latitude)
  extent_raster <- raster(extent(x.range[1], x.range[2], y.range[1], y.range[2]), res = 0.0002)
  grid_points <- rasterToPoints(extent_raster)
  grid_points_df <- data.frame(grid_points)
  coordinates(grid_points_df) <- ~x + y
  proj4string(grid_points_df) <- CRS("+proj=longlat +datum=WGS84")
  
  # IDW interpolation
  idw_output <- idw(formula = PM2.5_avg ~ 1, locations = ds, newdata = grid_points_df, idp = 2.0)
  gridded(idw_output) <- TRUE
  raster_interp <- raster(idw_output)
  
  return(raster_interp)
}

# Generate combined heatmap
raster_interp <- generate_heatmap(dataset_all)

# Interactive Leaflet map for combined data
map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Thunderforest.MobileAtlas") %>%
  setView(lng = mean(dataset_all$longitude), lat = mean(dataset_all$latitude), zoom = 13) %>%
  addRasterImage(raster_interp, opacity = 0.6, colors = color_palette) %>%
  #addPolylines(data = dataset_all, lng = ~longitude, lat = ~latitude, color = "red", weight = 2, opacity = 0.7) %>%
  addLegend(pal = color_palette, 
            values = dataset_all$PM2.5_avg,
            title = "PM2.5 (μm/m³)",
            opacity = 0.7,
            labFormat = function(type, cuts, p) {
              labels <- c("0-60%", "60-70%", "70-80%", "80-90%", "90-92.5%", "92.5-95%", "95-97.5%", "97.5-99%", "99-100%") 
              paste0(round(cuts[-length(cuts)], 2), " - ", round(cuts[-1], 2), " (", labels, ")")
            })

# Display the map
print(map)
