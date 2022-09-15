# library(tidyverse)
# library(lubridate)
# library(ggplot2)
# library(ggpubr)
# library(reshape2)
# library(ncdf4)
# library(LakeMetabolizer)
# library(here)

library(rLakeAnalyzer)
library(GLM3r)
library(glmtools)
library(FLakeR)
library(GOTMr)
library(gotmtools)
library(SimstratR)
library(MyLakeR)
library(LakeEnsemblR)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(Metrics)
library(plotrix)
library(here)
library(pacman)
library(ncdf4)

setwd(paste0(here(), '/LER_inputs/EWEMBI'))

both <- as.data.frame(seq.Date(from = as.Date("1979-01-01"), by = "day", length.out = 13880))


for(value in seq(14)){
flist <- list.files(pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist

# vars = c('hurs', #Near surface relative humidity
#          'huss', #Near surface specific humidity
#          'pr', #Precipitation
#          'prsn', #Snowfall flux
#          'ps', #Surface Pressure 
#          'psl', #Sea Level Pressure
#          'rlds', #Surface downwelling longwave radiation
#          'rsds', #Surface Downwelling Shortwave Radiation
#          'sfcWind', #Surface wind speed
#          'tas', #Near surface Air temperature
#          'tasmax', #Daily maximum near surface air temperature
#          'tasmin', #Daily near surface air temperature minimum
#          'uas', #Eastward near-surface wind
#          'vas' #Northward near-surface wind
# )

nc <- nc_open(paste0(flist[value]))

{
  sink(paste0( flist[1], ".txt"))
  print(nc)
  sink()
}

attributes(nc)$names

print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))
name <- attributes(nc$var)$names

ncatt_get(nc, attributes(nc$var)$names[1])
varoi <- ncvar_get(nc, attributes(nc$var)$names[1])
dim(varoi)
variableofi <- as.data.frame(varoi)
variableofi <- t(variableofi)
variableofi <- as.data.frame(varoi)
colnames(variableofi) <- c(name)

varoi
time <- as.data.frame(nc$dim$time$vals)
both <- cbind(both, variableofi)
}

colnames(both) <- c("datetime", "Relative_Humidity_percent", "huss", 
                    "precipitation_millimeterPerDay", "Snowfall_millimeterPerDay", 
                    "Surface_Level_Barometric_Pressure_pascal", 
                    "Sea_Level_Barometric_Pressure_pascal", "Longwave_Radiation_Downwelling_wattPerMeterSquared",
                    "Shortwave_Radiation_Downwelling_wattPerMeterSquared", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond",
                    "Air_Temperature_celsius", "tasmax", "tasmin", "Ten_Meter_Uwind_vector_meterPerSecond",
                    "Ten_Meter_Vwind_vector_meterPerSecond")

both <- select(both, -huss, -tasmax, -tasmin)

both$Rainfall_millimeterPerDay <- both$precipitation_millimeterPerDay-both$Snowfall_millimeterPerDay

str(both)

write.csv(both, "ewembimet.csv", row.names = FALSE)


new <- read.csv("ewembimet.csv")

str(new)
new$datetime <- as.POSIXct(new$datetime)
str(new)
write.csv(new, "ewembimet.csv", row.names = FALSE)


# ewembimet <- read.csv("ewembimet.csv")
# # 
# # colnames(ewembimet)
# # 
# ewembimet$Air_Temperature_celsius <- ewembimet$Air_Temperature_celsius - 274.15
# # 
# write.csv(ewembimet, "ewembimet.csv", row.names = FALSE)
# 
# metd <- read.csv("met_d.csv")
# 
# str(ewembimet)
# str(metd)

