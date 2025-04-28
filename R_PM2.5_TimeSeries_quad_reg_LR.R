library(tidyr)
library(ggplot2)
library(dplyr)
library(zoo)


### Run1 

Run1 <- read.csv("run1_quad.csv", header = TRUE)

Run1 <- mutate(Run1, BC6_ug = BC6/1000)
Run1$BC6_ug_30 <- zoo::rollmean(Run1$BC6_ug, k = 30, fill = NA, align = 'center')
Run1$PM2.5_30 <- zoo::rollmean(Run1$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run1$PM2.5_30)

Run1_long <- pivot_longer(Run1, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run1_long$datetime <- as.POSIXct(Run1_long$datetime)

Run1_ts <- ggplot(Run1_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-11 21:30:00"), as.POSIXct("2025-01-11 23:00:00")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 30) +
  ggtitle("A) Run 1") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), #angle = 45, hjust = 1)
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run1_ts
#ggsave("Run1_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)


### Run2 

Run <- read.csv("run2_quad.csv", header = TRUE)

Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run$PM2.5_30)

Run_long <- pivot_longer(Run, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run_long$datetime <- as.POSIXct(Run_long$datetime)

Run_ts <- ggplot(Run_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-11 23:29:19"), as.POSIXct("2025-01-12 01:00:07")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 30) +
  ggtitle("B) Run 2") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run_ts
#ggsave("Run2_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)


### Run3 
Run <- read.csv("run3_quad.csv", header = TRUE)

Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run$PM2.5_30)

Run_long <- pivot_longer(Run, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run_long$datetime <- as.POSIXct(Run_long$datetime)

Run_ts <- ggplot(Run_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-12 19:59:20"), as.POSIXct("2025-01-12 21:30:36")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 25) +
  ggtitle("C) Run 3") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run_ts
#ggsave("Run3_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)


### Run4 

Run <- read.csv("run4_quad.csv", header = TRUE)

Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run$PM2.5_30)

Run_long <- pivot_longer(Run, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run_long$datetime <- as.POSIXct(Run_long$datetime)

Run_ts <- ggplot(Run_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-12 22:02:00"), as.POSIXct("2025-01-12 23:34:28")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 50) +
  ggtitle("D) Run 4") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run_ts
#ggsave("Run4_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 5, units = "in", dpi = 300)



### Run5 

Run <- read.csv("run5_quad.csv", header = TRUE)

Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run$PM2.5_30)

Run_long <- pivot_longer(Run, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run_long$datetime <- as.POSIXct(Run_long$datetime)

Run_ts <- ggplot(Run_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-13 20:29:38"), as.POSIXct("2025-01-13 22:33:20")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 50) +
  ggtitle("E) Run 5") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run_ts
#ggsave("Run5_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 5, units = "in", dpi = 300)



### Run6 

Run <- read.csv("run6_quad.csv", header = TRUE)

Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
summary(Run$PM2.5_30)

Run_long <- pivot_longer(Run, cols = c(BC6_ug_30, PM2.5_30), names_to = "Particle", values_to = "Concentration")
Run_long$datetime <- as.POSIXct(Run_long$datetime)

Run_ts <- ggplot(Run_long, aes(x = datetime, y = Concentration, color = Particle, group = Particle)) +
  geom_line() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
  scale_x_datetime(
    limits = c(as.POSIXct("2025-01-13 22:58:40"), as.POSIXct("2025-01-14 00:59:47")),
    date_labels = "%H:%M",
    date_breaks = "10 min"
  ) +
  labs(x = "Time", y = "Concentration (µg/m³)", colour = " ") +
  scale_color_discrete(labels = c("BC6_ug_30" = "Black carbon", "PM2.5_30" = "PM2.5")) +
  ylim(0, 25) +
  ggtitle("F) Run 6") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 14),
    axis.line = element_line(linewidth = 0.25, color = "black"),
    legend.text = element_text(size = 14)
  )
Run_ts
#ggsave("Run6_BC6_PM2.5_ts_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)
