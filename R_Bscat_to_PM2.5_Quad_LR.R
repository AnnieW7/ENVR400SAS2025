library(tidyverse)
library(dplyr)

### define the PM2.5 conversion factors, from the quadratic regression equation
a <- 0.00083
b <- 0.332

### Run 1
run <- read.csv("run1LR.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run1_quad.csv", row.names = FALSE)


### Run 2
run <- read.csv("run2LR.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run2_quad.csv", row.names = FALSE)


### Run 3
run <- read.csv("run3LR.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run3_quad.csv", row.names = FALSE)


### Run 4
run <- read.csv("run4LR.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run4_quad.csv", row.names = FALSE)


### Run 5
run <- read.csv("run5LR_2029_start.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run5_quad.csv", row.names = FALSE)


### Run 6
run <- read.csv("run6LR.csv", header = TRUE, stringsAsFactors = FALSE)

run <- run %>%
  mutate(PM2.5 = a*(beta_scat^2) + b*beta_scat) %>%
  filter(!is.na(PM2.5))

nrow(run)
summary(run$PM2.5)
sd(run$PM2.5)
#write.csv(run, "run6_quad.csv", row.names = FALSE)

means <- c(4.4052, 3.2613, 4.4805, 3.4956, 7.122, 2.711)
mean(means)
