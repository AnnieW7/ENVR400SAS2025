library(tidyverse)
library(dplyr)


### Station 39301 below ###

PA39301 <- read.csv("39301_UTC.csv", header = TRUE)

PA39301$datetime <- as.POSIXct(PA39301$time_stamp, format="%Y-%m-%dT%H:%M:%SZ") #create and format the datetime column
PA39301$datetime <- format(PA39301$datetime, "%Y-%m-%d %H:%M:%S") #reformat datetime to remove "UTC"
head(PA39301$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "UTC"

PA39301$datetime <- as.POSIXct(PA39301$datetime, tz = "UTC") #define initial timezone as UTC
PA39301$datetime <- format(PA39301$datetime, tz = "America/Los_Angeles", usetz = TRUE) #change the datetime time stamps to correspond to PST
head(PA39301$datetime) # preview the time, should be 16:XX:XX PST on the previous day
tail(PA39301$datetime)

nrow(PA39301)
summary(PA39301$scattering_coefficient)
sd(PA39301$scattering_coefficient)
summary(PA39301$pm2.5_alt)
sd(PA39301$pm2.5_alt)

# write.csv(PA39301, "PA39301_PST.csv", row.names = FALSE)


### Station 175685 below ###

PAdata <- read.csv("175685_UTC.csv", header = TRUE)

PAdata$datetime <- as.POSIXct(PAdata$time_stamp, format="%Y-%m-%dT%H:%M:%SZ") #create and format the datetime column
PAdata$datetime <- format(PAdata$datetime, "%Y-%m-%d %H:%M:%S") #reformat datetime to remove "UTC"
head(PAdata$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "UTC"

PAdata$datetime <- as.POSIXct(PAdata$datetime, tz = "UTC") #define initial timezone as UTC
PAdata$datetime <- format(PAdata$datetime, tz = "America/Los_Angeles", usetz = TRUE) #change the datetime time stamps to correspond to PST
head(PAdata$datetime) # preview the time, should be 16:XX:XX PST on the previous day

nrow(PAdata)
summary(PAdata$scattering_coefficient)
sd(PAdata$scattering_coefficient)
summary(PAdata$pm2.5_alt)
sd(PAdata$pm2.5_alt)

# write.csv(PAdata, "PA175685_PST.csv", row.names = FALSE)


### Station 147725 ###

PAdata <- read.csv("147725_UTC.csv", header = TRUE)

PAdata$datetime <- as.POSIXct(PAdata$time_stamp, format="%Y-%m-%dT%H:%M:%SZ") #create and format the datetime column
PAdata$datetime <- format(PAdata$datetime, "%Y-%m-%d %H:%M:%S") #reformat datetime to remove "UTC"
head(PAdata$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "UTC"

PAdata$datetime <- as.POSIXct(PAdata$datetime, tz = "UTC") #define initial timezone as UTC
PAdata$datetime <- format(PAdata$datetime, tz = "America/Los_Angeles", usetz = TRUE) #change the datetime time stamps to correspond to PST
head(PAdata$datetime) # preview the time, should be 16:XX:XX PST on the previous day

nrow(PAdata)
summary(PAdata$scattering_coefficient)
sd(PAdata$scattering_coefficient)
summary(PAdata$pm2.5_alt)
sd(PAdata$pm2.5_alt)

# write.csv(PAdata, "PA147725_PST.csv", row.names = FALSE)


### Station 175713 ###

PAdata <- read.csv("175713_UTC.csv", header = TRUE)

PAdata$datetime <- as.POSIXct(PAdata$time_stamp, format="%Y-%m-%dT%H:%M:%SZ") #create and format the datetime column
PAdata$datetime <- format(PAdata$datetime, "%Y-%m-%d %H:%M:%S") #reformat datetime to remove "UTC"
head(PAdata$datetime) #datetime should print as "YYYY-mm-dd HH:MM:SS", without "UTC"

PAdata$datetime <- as.POSIXct(PAdata$datetime, tz = "UTC") #define initial timezone as UTC
PAdata$datetime <- format(PAdata$datetime, tz = "America/Los_Angeles", usetz = TRUE) #change the datetime time stamps to correspond to PST
head(PAdata$datetime) # preview the time, should be 16:XX:XX PST on the previous day

nrow(PAdata)
summary(PAdata$scattering_coefficient)
sd(PAdata$scattering_coefficient)
summary(PAdata$pm2.5_alt)
sd(PAdata$pm2.5_alt)

# write.csv(PAdata, "PA175713_PST.csv", row.names = FALSE)
