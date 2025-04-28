library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpmisc)
library(lubridate)


### Station 39301 ###

PA <- read.csv("PA39301_PST.csv", header = TRUE, stringsAsFactors = FALSE)

PA <- PA %>%
  mutate(date = as.Date(datetime))%>%
  mutate(date = format(date, "%d/%m/%Y"))
PA <- filter(PA, date > "10/01/2025", 
                  !is.na(pm2.5_alt), !is.na(scattering_coefficient))

PA_quad <- ggplot(
  data = PA,
  aes(x = scattering_coefficient, y = pm2.5_alt)) +
  geom_point(mapping = aes(color = date)) +
  ggtitle("Station 39301 (Kunut Ave)") +
  geom_smooth(formula = y ~ 0 + poly(x,2, raw = TRUE), method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), "*','~~", after_stat(rr.label))),
               formula = y ~ 0 + poly(x,2, raw = TRUE), parse = TRUE)
PA_quad # Y = 0.363x + 0.000499x2
#ggsave("39301_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)



### Station 175685 ###

PA <- read.csv("PA175685_PST.csv", header = TRUE, stringsAsFactors = FALSE)

PA <- PA %>%
  mutate(date = as.Date(datetime))%>%
  mutate(date = format(date, "%d/%m/%Y"))
PA <- filter(PA, date > "10/01/2025", 
             !is.na(pm2.5_alt), !is.na(scattering_coefficient))

PA_quad <- ggplot(
  data = PA,
  aes(x = scattering_coefficient, y = pm2.5_alt)) +
  geom_point(mapping = aes(color = date)) +
  ggtitle("Station 175685 (5757 Kwatamus Dr)") +
  geom_smooth(formula = y ~ 0 + poly(x,2, raw = TRUE), method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), "*','~~", after_stat(rr.label))),
               formula = y ~ 0 + poly(x,2, raw = TRUE), parse = TRUE)
PA_quad # Y = 0.321x + 0.00134x2
#ggsave("175685_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)



### Station 147725 ###

PA <- read.csv("PA147725_PST.csv", header = TRUE, stringsAsFactors = FALSE)

PA <- PA %>%
  mutate(date = as.Date(datetime))%>%
  mutate(date = format(date, "%d/%m/%Y"))
PA <- filter(PA, date > "10/01/2025", 
             !is.na(pm2.5_alt), !is.na(scattering_coefficient))

PA_quad <- ggplot(
  data = PA,
  aes(x = scattering_coefficient, y = pm2.5_alt)) +
  geom_point(mapping = aes(color = date)) +
  ggtitle("Station 147725 (5690 Kwatamus Ave)") +
  geom_smooth(formula = y ~ 0 + poly(x,2, raw = TRUE), method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), "*','~~", after_stat(rr.label))),
               formula = y ~ 0 + poly(x,2, raw = TRUE), parse = TRUE)
PA_quad # Y = 0.333x + 0.000496x2
#ggsave("147725_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)



### Station 175713 ###

PA <- read.csv("PA175713_PST.csv", header = TRUE, stringsAsFactors = FALSE)

PA <- PA %>%
  mutate(date = as.Date(datetime))%>%
  mutate(date = format(date, "%d/%m/%Y"))
PA <- filter(PA, date > "10/01/2025", 
             !is.na(pm2.5_alt), !is.na(scattering_coefficient))

PA_quad <- ggplot(
  data = PA,
  aes(x = scattering_coefficient, y = pm2.5_alt)) +
  geom_point(mapping = aes(color = date)) +
  ggtitle("Station 175713 (5586 Sinku Dr)") +
  geom_smooth(formula = y ~ 0 + poly(x,2, raw = TRUE), method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), "*','~~", after_stat(rr.label))),
               formula = y ~ 0 + poly(x,2, raw = TRUE), parse = TRUE)
PA_quad #Y = 0.31x + 0.000983x2
#ggsave("175713_quad.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)


x <- c(0.363,0.321,0.333,0.31)
mean(x)

x2 <- c(0.000499,0.00134,0.000496,0.000983)
mean(x2)
