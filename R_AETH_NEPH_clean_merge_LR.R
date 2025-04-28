# BC1 and BC6 raw data concentrations are in ng/m3

# cleaning the text file - manual steps:
# delete empty rows and text on top of the file
# delete semicolons between column names
# add column names 68 to 82 after the pre-existing column names
# separate each new column name with a space (" ")
  # the number of column names should now match the number of columns (82) in the data set
# columns 72-75 are the latitude, latitude_direction, longitude, and longitude_direction 
  # rename them accordingly in the text file (or later, in R)
# manually remove symbols like "%" from column names

# if one run spans two dates/files, combine all the run data into one file 
# refer to the run start and end times, and data time stamps

#####   In R: 

# install.packages("tidyverse")
# install.packages("dplyr")
library(tidyverse)
library(dplyr)

###   Run 1 AETH    ###

run1aeth_raw <- read.table("20250111/2025_01_11_Sechelt_Trip1_AETH_Copy.dat", 
                             header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)
# select the desired columns 
aeth <- select(run1aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
                            latitude_direction, longitude, longitude_direction)

# format datetime to ensure correct filtering 
aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

# filter times so that only the run data remains
# lower bound is the run start time, upper bound is the run end time
aeth <- filter(aeth, datetime > "2025-01-11 21:29:44", 
                            datetime < "2025-01-11 22:59:46")

threshold <- 5 # values under threshold (LOD = 5 nm/m3) will be replaced with new_value
new_value <- 5/sqrt(2) # as per Hornung & Reed, 1990
aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 1 NEPH    ###

neph <- read.csv("20250111/2025_01_11_Sechelt_Trip1_NEPH.txt", 
                           header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                              "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
                          enclosure_temperature) # select the desired columns 

neph <- filter(neph, datetime > "2025-01-11 21:29:44", 
                     datetime < "2025-01-11 22:59:46")

###   Run 1 MERGE   ###

#Merge aeth and neph data by datetime stamp 
run1 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run1)


###   Run 2 AETH   ###

run2aeth_raw <- read.table("20250111/2025_01_11_Sechelt_Trip2_AETH.dat", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run2aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-11 23:29:18", 
                   datetime < "2025-01-12 01:00:08")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 2 NEPH   ###

neph <- read.csv("20250111/2025_01_11_Sechelt_Trip1_NEPH_5.txt", 
                         header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-11 23:29:18", 
               datetime < "2025-01-12 01:00:08")

###   Run 2 MERGE   ###

run2 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run2)


###   Run 3 AETH   ###

run3aeth_raw <- read.table("20250112/2025_01_12_Sechelt_Trip3and4_AETH.dat", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run3aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-12 19:59:19", 
               datetime < "2025-01-12 21:30:37")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 3 NEPH   ###

neph <- read.csv("20250112/2025_01_12_Sechelt_Trip3_NEPH.txt", 
                 header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-12 19:59:19", 
               datetime < "2025-01-12 21:30:37")

###   Run 3 MERGE   ###

run3 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run3)


###   Run 4 AETH   ###

run4aeth_raw <- read.table("20250112/2025_01_12_Sechelt_Trip3and4_AETH.dat", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run4aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-12 22:01:59", 
               datetime < "2025-01-12 23:34:29")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 4 NEPH   ###

neph <- read.csv("20250112/2025_01_12_Sechelt_Trip4_NEPH.txt", 
                 header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-12 22:01:59", 
               datetime < "2025-01-12 23:34:29")

###   Run 4 MERGE   ###

run4 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run4)


###   Run 5 AETH   ###

run5aeth_raw <- read.table("20250113/2025_01_13_Sechelt_Trip5and6_AETH.dat", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run5aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-13 20:39:37", 
               datetime < "2025-01-13 22:33:21")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 5 NEPH   ###

neph <- read.csv("20250113/2025_01_13_Sechelt_Trip5_NEPH.txt", 
                 header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-13 20:39:37", 
               datetime < "2025-01-13 22:33:21")

###   Run 5 MERGE   ###

run5 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run5)


###   Run 6 AETH   ###

run6aeth_raw <- read.table("20250113/2025_01_13_Sechelt_Trip5and6_AETH.dat", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run6aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-13 22:58:39", 
               datetime < "2025-01-14 00:59:48")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 6 NEPH   ###

neph <- read.csv("20250113/2025_01_13_Sechelt_Trip6_NEPH.txt", 
                 header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-13 22:58:39", 
               datetime < "2025-01-14 00:59:48")

###   Run 6 MERGE   ###

run6 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run6)


# write.csv(run1, "run1LR.csv", row.names = FALSE)
# write.csv(run2, "run2LR.csv", row.names = FALSE)
# write.csv(run3, "run3LR.csv", row.names = FALSE)
# write.csv(run4, "run4LR.csv", row.names = FALSE)
# write.csv(run5, "run5LR.csv", row.names = FALSE)
# write.csv(run6, "run6LR.csv", row.names = FALSE)



###   Run 5 AETH AMENDED START TIME  ###

run5aeth_raw <- read.table("2025_01_13_Sechelt_Trip5and6_AETH.txt", 
                           header = TRUE, sep = " ", stringsAsFactors = FALSE, fill = TRUE)

aeth <- select(run5aeth_raw, Date, BB, Time, BC1, BC6, latitude, 
               latitude_direction, longitude, longitude_direction)

aeth$datetime <- as.POSIXct(paste(aeth$Date, aeth$Time, format="%Y-%m-%d %H:%M:%S"))
aeth$datetime <- format(aeth$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(aeth$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"
summary(aeth)

aeth <- filter(aeth, datetime > "2025-01-13 20:29:37", 
               datetime < "2025-01-13 22:33:21")

aeth <- aeth %>%
  mutate(BC6 = ifelse(BC6 < threshold, new_value, BC6)) %>%
  mutate(BC1 = ifelse(BC1 < threshold, new_value, BC1))

###   Run 5 NEPH AMENDED START TIME   ###

neph <- read.csv("2025_01_13_Sechelt_Trip5_NEPH.txt", 
                 header = FALSE, stringsAsFactors = FALSE, fill = TRUE)

colnames(neph) <- c("datetime", "beta_scat", "air_temperature", 
                    "enclosure_temperature", "relative_humidity", "pressure", "pressure2", "pressure3")

neph$datetime <- as.POSIXct(neph$datetime, format="%Y-%m-%d %H:%M:%S")
neph$datetime <- format(neph$datetime, "%Y-%m-%d %H:%M:%S") #reformat to remove "PST"
head(neph$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "PST"

neph$beta_scat <- as.numeric(neph$beta_scat)
neph$air_temperature <- as.numeric(neph$air_temperature)
neph$enclosure_temperature <- as.numeric(neph$enclosure_temperature)
neph$relative_humidity <- as.numeric(neph$relative_humidity)
neph$pressure <- as.numeric(gsub(",", ".", neph$pressure))

neph <- select(neph, datetime, beta_scat, air_temperature, 
               enclosure_temperature) # select the desired columns

neph <- filter(neph, datetime > "2025-01-13 20:29:37", 
               datetime < "2025-01-13 22:33:21")

###   Run 5 MERGE   ###

run5 <- merge(aeth, neph, by = "datetime", all = TRUE) 
summary(run5)
nrow(run5)
sd(run5$BC6)
sd(run5$beta_scat, na.rm = TRUE)

# write.csv(run5, "run5LR_2029_start.csv", row.names = FALSE)
