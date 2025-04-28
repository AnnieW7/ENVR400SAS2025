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
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run1_0.339.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run2_0.339.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run3_0.339.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run4_0.339.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run5_0.339.csv",
  "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/ProcessedSecheltData/run6_0.339.csv"
)

# Initialize an empty dataset
dataset_all <- data.frame()

# Read and preprocess all runs
for (file in data_files) {
  ds <- read.csv(file, stringsAsFactors = FALSE)
  ds$datetime <- as.POSIXct(ds$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # Extract run number from file name
  ds$run_number <- as.numeric(gsub(".*run([0-9]+).*", "\\1", file))
  
  # Convert coordinates to decimal degrees
  ds$latitude <- as.numeric(substr(ds$latitude, 1, 2)) + (as.numeric(substr(ds$latitude, 3, nchar(ds$latitude))) / 60)
  ds$longitude <- -(as.numeric(substr(ds$longitude, 1, 3)) + (as.numeric(substr(ds$longitude, 4, nchar(ds$longitude))) / 60))
  
  # Compute 30-second rolling average for BC6
  ds <- ds %>%
    arrange(datetime) %>%
    mutate(BC6_avg = rollmean(BC6, k = 30, fill = NA, align = "right")) %>%
    na.omit()
  
  dataset_all <- rbind(dataset_all, ds)
}

# Compute percentiles with the updated probabilities
quantiles <- quantile(dataset_all$BC6_avg, probs = c(0, 0.6, 0.7, 0.8,0.9,0.925, 0.95, 0.975, 0.99, 1.0), na.rm = TRUE)

# Updated color gradient ensuring red/orange transition starts at 80%
color_palette <- colorBin(
  #palette = colorRampPalette(c("#D0EFFF","lightyellow","yellow","#FFD700", "#FF8C00" ,"#FF6600", "red","#B22222", "darkred","#550000","#6400E4"))(12),
  palette <- colorRampPalette(c("#D0EFFF", "lightyellow", "yellow", "#FFD700", "#FF8C00", "#FF6600", "red", "#B22222", "darkred", "#550000", "#8000FF"))(10),  
 #"#D0EFFF", "lightgreen", "lightyellow", "yellow", "orange", "#FF6600", "red", "darkred", "#550000"
 # "#D0EFFF","lightyellow","yellow", "orange", "#FF6600", "red", "darkred","#550000"))(n)
 #c("yellow", "#FFD700", "#FFA500", "#FF8C00", "#FF4500", "red", "#B22222", "darkred", "#550000")
 #palette = colorRampPalette(c("#D0EFFF","lightyellow","yellow","#FFD700", "#FF8C00" ,"#FF6600", "red","#B22222", "darkred","#550000","#6400E4"))(12),
  domain = dataset_all$BC6_avg,
  bins = quantiles,
  na.color = "#000000"
)



# Heatmap generation function
generate_heatmap <- function(ds, run_number) {
  coordinates(ds) <- ~longitude + latitude
  proj4string(ds) <- CRS("+proj=longlat +datum=WGS84")
  
  # Create an interpolation grid
  x.range <- range(ds$longitude)
  y.range <- range(ds$latitude)
  extent_raster <- raster(extent(x.range[1], x.range[2], y.range[1], y.range[2]), res = 0.0001)
  grid_points <- rasterToPoints(extent_raster)
  grid_points_df <- data.frame(grid_points)
  coordinates(grid_points_df) <- ~x + y
  proj4string(grid_points_df) <- CRS("+proj=longlat +datum=WGS84")
  
  # IDW interpolation
  idw_output <- idw(formula = BC6_avg ~ 1, locations = ds, newdata = grid_points_df, idp = 2.0)
  gridded(idw_output) <- TRUE
  raster_interp <- raster(idw_output)
  
  return(raster_interp)
}

# Generate heatmaps for each run
rasters <- list()
for (i in 1:6) {
  ds_run <- dataset_all %>% filter(run_number == i)
  cat("Generating heatmap for run", i, "...\n")
  rasters[[i]] <- generate_heatmap(ds_run, i)
}

# Interactive Leaflet maps for each run
for (i in 1:6) {
  ds_run <- dataset_all %>% filter(run_number == i)
  raster_interp <- rasters[[i]]
  
  map <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Thunderforest.MobileAtlas") %>%
    setView(lng = mean(ds_run$longitude), lat = mean(ds_run$latitude), zoom = 13) %>%
    addRasterImage(raster_interp, opacity = 0.6, colors = color_palette) %>%
    addPolylines(data = ds_run, lng = ~longitude, lat = ~latitude, color = "red", weight = 2, opacity = 0.7) %>%
    addLegend(pal = color_palette, 
              values = dataset_all$BC6_avg,
              title = paste("Run", i, "BlCa (ng/m³)"),
              opacity = 0.7,
              labFormat = function(type, cuts, p) {
                # Adjust the labels to match the new probs
                labels <- c("0-60%", "60-70%", "70-80%", "80-90%", "90-92.5%", "92.5-95%", "95-97.5%", "97.5-99%", "99-100%") 
                paste0(round(cuts[-length(cuts)], 2), " - ", round(cuts[-1], 2), " (", labels, ")")
              }) %>%
    #addMarkers(lng = ds_run$longitude[1], lat = ds_run$latitude[1], popup = "Start", label = "Start", icon = makeIcon(iconUrl = "https://maps.google.com/mapfiles/kml/paddle/grn-circle.png")) %>%
    #addMarkers(lng = tail(ds_run$longitude, 1), lat = tail(ds_run$latitude, 1), popup = "End", label = "End", icon = makeIcon(iconUrl = "https://maps.google.com/mapfiles/kml/paddle/red-stars.png"))
    
    print(map)
}
