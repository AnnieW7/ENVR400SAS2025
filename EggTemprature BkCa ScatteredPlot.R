# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)  # For handling date-time data

# Define file paths
folder_path <- "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/CombinedTempnBCData/"
file_names <- paste0(folder_path, "CombinedTempnBC_Run", 1:6, ".csv")

# Read and combine all CSV files
data_list <- lapply(file_names, read_csv)
data <- bind_rows(data_list)

# Convert "Time Point" to datetime format and extract the date
data$Time_Point <- ymd_hms(data$`Time Point`)  # Convert to POSIXct datetime
data$Date <- as.Date(data$Time_Point)  # Extract only the date

# Remove zeros from BC6 data
data <- data %>% filter(BC6 > 0)

# Remove the smallest 5% of BC6 values
lower_percentile <- quantile(data$BC6, 0.05, na.rm = TRUE)
data <- data %>% filter(BC6 > lower_percentile)

# Remove the upper 5% of temperature values
upper_temp_threshold <- quantile(data$`temp (C)`, 0.95, na.rm = TRUE)
data <- data %>% filter(`temp (C)` <= upper_temp_threshold)

# Remove outliers using the IQR method
Q1 <- quantile(data$BC6, 0.25, na.rm = TRUE)
Q3 <- quantile(data$BC6, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter data within bounds
data <- data %>% filter(BC6 >= lower_bound & BC6 <= upper_bound)

# Fit linear model
lm_model <- lm(BC6 ~ `temp (C)`, data = data)
lm_summary <- summary(lm_model)

# Extract coefficients
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

# Extract R² and p-value
r_squared <- lm_summary$r.squared
p_value <- lm_summary$coefficients[2, 4]

# Create equation text
eq_text <- paste0(
  "y = ", round(slope, 3), "x + ", round(intercept, 3), "\n",
  "R² = ", round(r_squared, 3), ", p = ", formatC(p_value, format = "e", digits = 2)
)

# Create scatter plot with points colored by date
plot <- ggplot(data, aes(x = `temp (C)`, y = BC6)) +
  geom_point(alpha = 0.6, size = 2, color = "red") +  # Set point color directly here
  annotate("text", x = min(data$`temp (C)`, na.rm = TRUE), 
           y = max(data$BC6, na.rm = TRUE), 
           label = eq_text, hjust = 0, vjust = 1, 
           size = 5, fontface = "bold", color = "black") +
  labs(
    title = "Scatter Plot of Air Temperature vs Black Carbon Concentration",
    x = "Air Temperature (°C)",
    y = "BlCa Concentration (ng/m³)",
    caption = "Data Source: CombinedTempnBC_Run1-6.csv"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


# Display the plot
print(plot)
