# Load necessary libraries
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(gridExtra)  # For table plotting

# Define file path
file_path <- "/Users/annieinnature/Documents/2025 Spring/ENVR400 Sechelt project/Data Analysis/TheEggData/TheEggAllData.csv"

# Read the dataset
data <- read_csv(file_path)

# Convert "Time Point" to datetime format
data <- data %>%
  mutate(Time_Point = ymd_hms(`Time Point`),
         Date = as.Date(Time_Point))

# Define specific time filters for each date
filtered_data <- data %>%
  filter(
    (Date == as.Date("2025-01-11") & 
       hour(Time_Point) >= 22 & (hour(Time_Point) < 24 | (hour(Time_Point) == 0 & minute(Time_Point) <= 7))) |
      (Date == as.Date("2025-01-12") & hour(Time_Point) >= 21) |
      (Date == as.Date("2025-01-13") & 
         (hour(Time_Point) > 21 | (hour(Time_Point) == 21 & minute(Time_Point) >= 50)) &
         hour(Time_Point) < 25)  # Including up to 24:59:47
  )

# Remove the upper 5% temperature values as outliers
upper_threshold <- quantile(filtered_data$`temp (C)`, 0.95, na.rm = TRUE)
filtered_data <- filtered_data %>% filter(`temp (C)` <= upper_threshold)

# Compute summary statistics for each day
summary_stats <- filtered_data %>%
  group_by(Date) %>%
  summarize(
    Avg_Temp = round(mean(`temp (C)`, na.rm = TRUE), 2),
    Min_Temp = round(min(`temp (C)`, na.rm = TRUE), 2),
    Max_Temp = round(max(`temp (C)`, na.rm = TRUE), 2),
    Temp_Diff = round(Max_Temp - Min_Temp, 2)  # Temperature range
  )

# Assign specific colors for average temperature lines
date_colors <- c("2025-01-11" = "red", "2025-01-12" = "green", "2025-01-13" = "blue")

# Create the time series plot with points and solid color-coded average lines
plot <- ggplot(filtered_data, aes(x = Time_Point, y = `temp (C)`, color = as.factor(Date))) +
  geom_point(size = 1, alpha = 0.6) +  # Small points for visibility
  geom_hline(data = summary_stats, aes(yintercept = Avg_Temp, color = as.factor(Date)), 
             linetype = "solid", size = 1.2, alpha = 0.9) +  # Darker color-coded average line
  scale_color_manual(values = date_colors) +  # Apply custom colors for lines
  labs(
    title = "Time Series of Air Temperature During Sampling Periods",
    x = "Time (HH:MM)",
    y = "Temperature (°C)",
    color = "Date",
    caption = "Data Source: TheEggAllData.csv\nSolid lines represent the average daily temperature."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top"
  ) +
  facet_wrap(~Date, scales = "free_x")

# Create the summary table for display
summary_table <- summary_stats %>%
  rename(`Avg Temp (°C)` = Avg_Temp,
         `Min Temp (°C)` = Min_Temp,
         `Max Temp (°C)` = Max_Temp,
         `Temp Range (°C)` = Temp_Diff)

# Convert to tableGrob for clear presentation
table_grob <- tableGrob(summary_table, rows = NULL, 
                        theme = ttheme_minimal(core=list(fg_params=list(fontface="bold")),
                                               colhead=list(fg_params=list(fontface="bold"))))

# Arrange plot and table
grid.arrange(plot, table_grob, ncol = 1, heights = c(3, 1))
