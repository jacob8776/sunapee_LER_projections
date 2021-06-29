library(LakeEnsemblR)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)

gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("historical", "rcp60")
vars <- c("Relative_Humidity_percent", "Precipitation_millimeterPerDay", "Snowfall_millimeterPerDay",
          "Surface_Level_Barometric_Pressure_pascal", "Sea_Level_Barometric_Pressure_pascal", 
          "Longwave_Radiation_Downwelling_wattPerMeterSquared", "Shortwave_Radiation_Downwelling_wattPerMeterSquared",
          "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Air_Temperature_celsius", 
          "Ten_Meter_Uwind_vector_meterPerSecond", "Ten_Meter_Vwind_vector_meterPerSecond")


setwd("~/Dropbox/sunapee_LER_projections/met_files_processed")

anomalies <- data.frame("year" = numeric(0), "ann_mean" = numeric(0), 
                        "anom" = numeric(0), "model" = character(0), 
                        "variable" = character(0))

for(q in 1:length(vars)){

for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path("~/Dropbox/sunapee_LER_projections/met_files_processed/", gcm[[i]]))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    fname <- paste0(gcm[[i]], "_", rcp[[l]], ".csv")


    
    met <- read.csv(fname)
    if(rcp[[l]] == "historical"){
      hist_mean <- function(met){
        met$year <- year(met$datetime)
        met$month <- month(met$datetime)
        sub <- met[met$year >= 1975 & met$year <= 2005 & met$month >= 6 & met$month <= 9, ]
        subselect <- select(sub, vars[[q]])
        mean(subselect[,1], na.rm = TRUE)
      }
      hmean <- hist_mean(met)

    }else{
      anom <- function(met) {
        met$year <- year(met$datetime)
        met$month <- month <- month(met$datetime)
        subst <- met[met$year >= 2006 & met$year <= 2099 & met$month >= 6 & met$month <= 9, ]
      }
      
      subset <- anom(met)
      
      mn <- ddply(subset, 
                  .variables = "year", 
                  .fun = function(subset){
                    ann_mean_selected <- select(subset, vars[[q]])
                    ann_mean <- mean(ann_mean_selected[,1], na.rm = TRUE)
                    return(data.frame(ann_mean))
                  })
      mn$anom <- mn$ann_mean - hmean
      
      mn$model <- gcm[[i]]
      mn$variable <- vars[[q]]
      
      anomalies <- rbind(anomalies, mn)
      
     assign(paste0("mn_", gcm[[i]], "_", rcp[[l]], "_", vars[[q]]), mn)
      

   }
  }
 }
}


ggplot(data = subset(anomalies), 
       mapping = aes(x = year, y = anom, color = model)) + 
  geom_line() + 
  facet_wrap(vars(variable), scales = 'free') + 
  ggtitle("Anomalies of met variables in summer months")





