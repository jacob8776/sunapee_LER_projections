library(LakeEnsemblR)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)

gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("rcp26", "rcp60", "rcp85")
vars <- c("Relative_Humidity_percent", "Precipitation_millimeterPerDay", "Snowfall_millimeterPerDay",
          "Surface_Level_Barometric_Pressure_pascal", "Sea_Level_Barometric_Pressure_pascal", 
          "Longwave_Radiation_Downwelling_wattPerMeterSquared", "Shortwave_Radiation_Downwelling_wattPerMeterSquared",
          "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Air_Temperature_celsius", 
          "Ten_Meter_Uwind_vector_meterPerSecond", "Ten_Meter_Vwind_vector_meterPerSecond")


setwd("~/Dropbox/sunapee_LER_projections/met_files_processed/cat_files/")

anomalies <- data.frame("year" = numeric(0), "ann_mean" = numeric(0), 
                        "anom" = numeric(0), "model" = character(0), 
                        "variable" = character(0), "rcp" = character(0))


for(q in 1:length(vars)){
  
  for(i in 1:length(gcm)){
    # Sets working directory to each gcm 
    setwd(file.path("~/Dropbox/sunapee_LER_projections/met_files_processed/cat_files/", gcm[[i]]))
    # Nested for loop goes into RCP scenarios for GCMs 
    for(l in 1:length(rcp)){
      fname <- paste0(gcm[[i]], "_", rcp[[l]], ".csv")
      
      
      
      met <- read.csv(fname)
      met$year <- year(met$datetime)
      met$month <- month(met$datetime)
      sub <- met[met$year >= 1975 & met$year <= 2005 & met$month >= 6 & met$month <= 9, ]
      subselect <- select(sub, vars[[q]])
      hmean <- mean(subselect[,1], na.rm = TRUE)
      
      
      subset <- met[met$year >= 2006 & met$year <= 2099 & met$month >= 6 & met$month <= 9, ]
      
      mn <- ddply(subset, 
                  .variables = "year", 
                  .fun = function(subset){
                    ann_mean_selected <- select(subset, vars[[q]])
                    ann_mean <- mean(ann_mean_selected[,1], na.rm = TRUE)
                    return(data.frame(ann_mean))
                  })
      
      mn$anom <- mn$ann_mean - hmean
      
      mn$model <- gcm[[i]]
      mn$rcp <- rcp[[l]]
      mn$variable <- vars[[q]]
      
      anomalies <- rbind(anomalies, mn)
      
      assign(paste0("mn_", gcm[[i]], "_", rcp[[l]], "_", vars[[q]]), mn)
      
    }
  }
}



anomalies_master <- anomalies %>% 
  group_by(year, model, rcp, variable) %>% 
  mutate(gcm_mean = mean(anom, na.rm = TRUE)) %>% 
  mutate(gcm_sd = sd(anom, na.rm = TRUE)) %>% 
  mutate(gcm_var = var(anom, na.rm = TRUE))






mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


ggplot(data = subset(anomalies_master), 
       mapping = aes(x = year, y = gcm_mean, color = rcp)) + 
  geom_line() + 
  facet_wrap(vars(variable), scales = 'free') + 
  ggtitle("Anomalies of met variables in summer months") +
  mytheme


anomalies$variable <- as.factor(anomalies$variable)
anomalies <- anomalies %>% 
  dplyr::group_by(year, variable) %>% 
  dplyr::mutate(mean = mean(anom)) %>% 
  dplyr::mutate(sd = sd(anom))


ggplot(data = subset(anomalies), 
       mapping = aes(x = year, y = anom, color = model)) + 
  geom_line() + 
  facet_wrap(vars(variable), scales = 'free') + 
  ggtitle("Anomalies of met variables in summer months")



ggplot(data=anomalies, aes(y=mean, x=year), color = "black") + 
  geom_line() + 
  geom_ribbon(data = anomalies, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.4, 
              linetype = 0.1, 
              color = "grey") + 
  facet_wrap(vars(variable), scales = 'free') +
  ggtitle("Mean anomaly of met variables in summer months")

library(tidyr)

long_met <- gather(met, measurement, value, Relative_Humidity_percent:Ten_Meter_Vwind_vector_meterPerSecond)
long_met_f <- filter(long_met, year >= 2004 & year <= 2007)
long_met_f$datetime <- as.Date(long_met_f$datetime)

ggplot(data = long_met_f, 
       mapping = aes(x = datetime, y = value)) + 
  geom_line() + 
  # facet_wrap(vars(measurement), scales = 'free') + 
  facet_wrap(~measurement, scales = 'free') + 
  geom_vline(xintercept = as.Date("2006-01-01")) +
  ggtitle("Met Data from 2004-2006")



