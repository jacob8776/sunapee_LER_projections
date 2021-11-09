library(tidyverse)
library(dplyr)
library(plyr)

setwd("~/Dropbox/sunapee_LER_projections/LER_calibration/Data/")

manbuoy <- read.csv("manual_buoy_temp_hrz.csv")

manbuoy$floordepth <- round_any(manbuoy$Depth_meter, 1)

str(manbuoy)


manbuoy$newdepth <- replace(manbuoy$floordepth, manbuoy$floordepth == 0, 0.1)


manbuoy <- select(manbuoy, datetime, newdepth, Water_Temperature_celsius)
colnames(manbuoy) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")
head(manbuoy)
str(manbuoy)

manbuoy <- manbuoy %>% 
  group_by(datetime, Depth_meter) %>% 
  mutate(mean_temp = mean(Water_Temperature_celsius,na.rm = TRUE))

manbuoy <- select(manbuoy, datetime, Depth_meter, mean_temp)
colnames(manbuoy) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")

str(manbuoy)
head(manbuoy)

write.csv(manbuoy, "manual_buoy_temp_dpth.csv", row.names = FALSE)
