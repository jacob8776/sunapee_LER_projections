library(tidyverse)
library(lubridate)


setwd("~/Dropbox/sun_ens_ltta/Data")
mantemp <- read.csv("manual_temperature_210.csv")
mantemp <- mantemp %>% filter(!is.na(Temp)) %>% 
  filter(Depth != 85)

str(mantemp)
mantemp$DateTime <- as.POSIXct((mantemp$DateTime))
str(mantemp)
colnames(mantemp) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")

write.csv(mantemp, "mantemp.csv", row.names = FALSE)



meteo <- read.csv("met_daily.csv")
str(meteo)
meteo$datetime <- as.POSIXct(meteo$datetime)

meteo <- meteo %>%
    mutate(datetime = floor_date(datetime)) %>%
    group_by(datetime) %>% 
    summarize_all(mean)
str(meteo)

meteo$Surface_Level_Barometric_Pressure_pascal <- 10000

#write.csv(meteo, "met_d.csv", row.names = FALSE)
write.csv(meteo, "met_d.csv", row.names = FALSE)


write.csv(meteo, "met_d.csv", row.names = FALSE)


inflow <- read.csv("oneInflow_14Jun19.csv")
inflow <- select(inflow, time, FLOW, TEMP, SALT)
str(inflow)
colnames(inflow) <- c("datetime", "Flow_metersCubedPerSecond", 
                      "Water_Temperature_celsius", "Salinity_practicalSalinityUnits")
inflow$datetime <- ymd_hms(inflow$datetime)
str(inflow)
inflow$Salinity_practicalSalinityUnits <- as.numeric(inflow$Salinity_practicalSalinityUnits)
str(inflow)

write.csv(inflow, "infens.csv", row.names = FALSE)




str(inflow)















