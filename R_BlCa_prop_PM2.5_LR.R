library(dplyr)
library(tidyverse)

# convert BC6 from ng/m3 to ug/m3, then divide by PM2.5 (in ug/m3) to get BC6 proportion of PM2.5
# then, multiply the proportion by 100 to get the percent composition of BlCa in PM2.5
# the data sets have been already been processed; all BC6 values under the LOD (5 ng/m3) were replaced with LOD/sqrt2

### Run 1
Run <- read.csv("run1_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)

### Run 2
Run <- read.csv("run2_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)

### Run 3
Run <- read.csv("run3_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)

### Run 4
Run <- read.csv("run4_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)

### Run 5
Run <- read.csv("run5_0.339_2029_start.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)

### Run 6
Run <- read.csv("run6_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_prop = BC6/1000/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)
sd(Run$BC6_percent)



### Using 30 second rolling mean ###

library(zoo)

### Run 1
Run <- read.csv("run1_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)

### Run 2
Run <- read.csv("run2_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)


### Run 3
Run <- read.csv("run3_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)


### Run 4
Run <- read.csv("run4_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)


### Run 5
Run <- read.csv("run5_0.339_2029_start.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)


### Run 6
Run <- read.csv("run6_0.339.csv", header = TRUE)
Run <- mutate(Run, BC6_ug = BC6/1000)
Run$BC6_ug_30 <- zoo::rollmean(Run$BC6_ug, k = 30, fill = NA, align = 'center')
Run$PM2.5_30 <- zoo::rollmean(Run$PM2.5, k = 30, fill = NA, align = 'center')
Run <- mutate(Run, BC6_prop = BC6_ug_30/PM2.5) 
summary(Run$BC6_prop)
Run <- mutate(Run, BC6_percent = BC6_prop*100)
summary(Run$BC6_percent)



### more statistics 

# average PM2.5 across all runs 
mean_PM2.5 <- mean(4.2492, 3.1693, 4.3163, 3.3685, 6.715, 2.688)
mean_PM2.5

# average BlCa percentage composition of PM2.5 across all runs (no rollmean) 
mean_BlCa_percent <- mean(10.1, 9.72, 10.7, 10.8, 7.99, 9.42)
mean_BlCa_percent

# average BlCa across all runs 
mean_BlCa_ng <- mean(265.50, 189.70, 319.50, 249.02, 447.098, 210.98)
mean_BlCa_ng
