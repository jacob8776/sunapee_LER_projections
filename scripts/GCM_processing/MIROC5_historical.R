library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(ncdf4)
library(LakeMetabolizer)

setwd("~/Dropbox/JHW_thesis/Sunapee/")

fils <- list.files("MIROC5", full.names = TRUE)


both <- as.data.frame(seq.Date(from = as.Date("1861-01-01"), by = "day", length.out = 52960))


for(value in seq(14)){
  flist <- list.files("MIROC5", full.names = TRUE, pattern = "historical", )
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
  nc[["dim"]][["time"]][["units"]]
  
  
  # {
  #   sink(paste0( "hurs", ".txt"))
  #   print(nc)
  #   sink()
  # }
  
  attributes(nc)$names
  
  nc$nvars
  
  
  print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))
  if(nc$nvars > 1){
    name <- attributes(nc$var)$names[2]
    ncatt_get(nc, attributes(nc$var)$names[2])
    varoi <- ncvar_get(nc, attributes(nc$var)$names[2])
  }
  else{
    name <- attributes(nc$var)$names[1]
    ncatt_get(nc, attributes(nc$var)$names[1])
    varoi <- ncvar_get(nc, attributes(nc$var)$names[1])
  }
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

str(both)

both$datetime <-  ymd(both$datetime)
both$Air_Temperature_celsius <- both$Air_Temperature_celsius -274.15

write.csv(both, "MIROC5_historical.csv", row.names = FALSE)
