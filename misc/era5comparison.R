library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(ncdf4)
library(LakeMetabolizer)


ewembimet <- read.csv("~/Dropbox/sunapee_LER_projections/LER_calibration/Data/ewembimet.csv")
era5 <- read.csv("~/Dropbox/sunapee_LER_projections/LER_validation/Data/ewembimeti3.csv")

era5f <- era5 %>% filter(datetime >= "1979-01-01" & datetime <= "2016-12-31")


relhum <- lm(ewembimet$Relative_Humidity_percent~era5f$Relative_Humidity_percent)
summary(relhum)

plot(ewembimet$Relative_Humidity_percent, era5f$Relative_Humidity_percent)

rainfall <- lm(ewembimet$Rainfall_millimeterPerDay~era5f$Rainfall_millimeterPerDay)
summary(rainfall)

plot(ewembimet$Rainfall_millimeterPerDay, era5f$Rainfall_millimeterPerDay)


pressure <- lm(ewembimet$Surface_Level_Barometric_Pressure_pascal~era5f$Surface_Level_Barometric_Pressure_pascal)
summary(pressure)

plot(ewembimet$Surface_Level_Barometric_Pressure_pascal, era5f$Surface_Level_Barometric_Pressure_pascal)


lwradiation <- lm(ewembimet$Longwave_Radiation_Downwelling_wattPerMeterSquared~era5f$Longwave_Radiation_Downwelling_wattPerMeterSquared)
summary(lwradiation)

plot(ewembimet$Longwave_Radiation_Downwelling_wattPerMeterSquared, era5f$Longwave_Radiation_Downwelling_wattPerMeterSquared)


swradiation <- lm(ewembimet$Shortwave_Radiation_Downwelling_wattPerMeterSquared~era5f$Shortwave_Radiation_Downwelling_wattPerMeterSquared)
summary(swradiation)

plot(ewembimet$Shortwave_Radiation_Downwelling_wattPerMeterSquared, era5f$Shortwave_Radiation_Downwelling_wattPerMeterSquared)


windspeed <- lm(ewembimet$Ten_Meter_Elevation_Wind_Speed_meterPerSecond~era5f$Ten_Meter_Elevation_Wind_Speed_meterPerSecond)
summary(windspeed)

plot(ewembimet$Ten_Meter_Elevation_Wind_Speed_meterPerSecond, era5f$Ten_Meter_Elevation_Wind_Speed_meterPerSecond)


airtemp <- lm(ewembimet$Air_Temperature_celsius~era5f$Air_Temperature_celsius)
summary(airtemp)

plot(ewembimet$Air_Temperature_celsius, era5f$Air_Temperature_celsius)










