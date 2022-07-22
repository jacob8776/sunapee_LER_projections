library(tidyverse)
library(dplyr)
library(plyr)

setwd("~/Dropbox/sunapee_LER_projections/LER_calibration/Data/")

manbuoy <- read.csv("manual_buoy_temp_hrz.csv")
unique(manbuoy$Depth_meter)

manbuoy$floordepth <- round_any(manbuoy$Depth_meter, 1)
unique(manbuoy$floordepth)

str(manbuoy)


manbuoy$newdepth <- replace(manbuoy$floordepth, manbuoy$floordepth == 0, 0.1)


manbuoy <- select(manbuoy, datetime, newdepth, Water_Temperature_celsius)
colnames(manbuoy) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")
head(manbuoy)
str(manbuoy)

manbuoy <- plyr::ddply(manbuoy, c("datetime", "Depth_meter"), function(x) {
  data.frame(mean_temp = mean(x[, 3]))
})

# manbuoy <- manbuoy %>% 
#   group_by(datetime, Depth_meter) %>% 
#   mutate(mean_temp = mean(Water_Temperature_celsius,na.rm = TRUE))

manbuoy <- select(manbuoy, datetime, Depth_meter, mean_temp)
colnames(manbuoy) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")

str(manbuoy)
head(manbuoy)

# manbuoy$datetime <- as.POSIXct(manbuoy$datetime)

tst <- paste0(dat$datetime, dat$Depth_meter)

idx <- duplicated(tst)
sum(idx)

ggplot(manbuoy) +
  # geom_line(aes(datetime, Water_Temperature_celsius, color = as.factor(Depth_meter))) +
  geom_point(aes(datetime, Water_Temperature_celsius, color = as.factor(Depth_meter))) +
  coord_cartesian(xlim = as.POSIXct(c("2005-01-01", "2014-01-01")))

write.csv(manbuoy, "manual_buoy_temp_dpth.csv", row.names = FALSE)
