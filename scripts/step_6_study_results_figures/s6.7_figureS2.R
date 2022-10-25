library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(Metrics)
library(zoo) # moving averages        
library(tidyverse) # all tidyverse packages
library(dplyr)
library(here)

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=14, colour='black'), axis.text.y=element_text(size=14, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(20), strip.text.y = element_text(size=20),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=25),
                 legend.title = element_text(size = 25), 
                 plot.title = element_text(size=25))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


setwd(paste0(here::here(), "/met_data_comparison/"))

era5 <- read.csv("era5.csv")
ewembi <- read.csv("ewembimet.csv")

era5 <- filter(era5, datetime >= "1979-01-01" & datetime <= "2016-12-31")
era5$Air_Temperature_celsius <- era5$Air_Temperature_celsius - 273.15

relhum_lm <- lm(era5$Relative_Humidity_percent~ewembi$Relative_Humidity_percent)
summary(relhum_lm)


rainfall_lm <- lm(era5$Rainfall_millimeterPerDay~ewembi$Precipitation_millimeterPerDay)
summary(rainfall_lm)


sl_barometric <- lm(era5$Surface_Level_Barometric_Pressure_pascal~ewembi$Surface_Level_Barometric_Pressure_pascal)
summary(sl_barometric)


lw_radiation <- lm(era5$Longwave_Radiation_Downwelling_wattPerMeterSquared~ewembi$Longwave_Radiation_Downwelling_wattPerMeterSquared)
summary(lw_radiation)


sw_radiation <- lm(era5$Shortwave_Radiation_Downwelling_wattPerMeterSquared~ewembi$Shortwave_Radiation_Downwelling_wattPerMeterSquared)
summary(sw_radiation)


windspeed <- lm(era5$Ten_Meter_Elevation_Wind_Speed_meterPerSecond~ewembi$Ten_Meter_Elevation_Wind_Speed_meterPerSecond)
summary(windspeed)


airtemp <- lm(era5$Air_Temperature_celsius~ewembi$Air_Temperature_celsius)
summary(airtemp)



era5_cols <- colnames(era5)
colnames(era5) <- paste("era5", era5_cols, sep = "_")

ewembi_cols <- colnames(ewembi)
colnames(ewembi) <- paste("ewembi", ewembi_cols, sep = "_")


boundmets <- bind_cols(era5, ewembi)

colnames(boundmets)



relhum_plot <- ggplot(boundmets, aes(y = era5_Relative_Humidity_percent, x = ewembi_Relative_Humidity_percent))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = 40, label.y = 85)+
  stat_regline_equation(label.x = 40, label.y = 80, aes(label = ..rr.label..)) + mytheme


rainfall_plot <- ggplot(boundmets, aes(y = era5_Rainfall_millimeterPerDay, x = ewembi_Rainfall_millimeterPerDay))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue")+  
  stat_regline_equation(label.x = .0003, label.y = 0.0009)+
  stat_regline_equation(label.x = 0.0003, label.y = 0.00099, aes(label = ..rr.label..)) + mytheme



pressure_plot <- ggplot(boundmets, aes(y = era5_Surface_Level_Barometric_Pressure_pascal, x = ewembi_Surface_Level_Barometric_Pressure_pascal))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = 94000, label.y = 99000)+
  stat_regline_equation(label.x = 94000, label.y = 100000, aes(label = ..rr.label..)) + mytheme

lwradiation_plot <- ggplot(boundmets, aes(y = era5_Longwave_Radiation_Downwelling_wattPerMeterSquared, x = ewembi_Longwave_Radiation_Downwelling_wattPerMeterSquared))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = 70, label.y = 300)+
  stat_regline_equation(label.x = 70, label.y = 350, aes(label = ..rr.label..)) + mytheme


swradiation_plot <- ggplot(boundmets, aes(y = era5_Shortwave_Radiation_Downwelling_wattPerMeterSquared, x = ewembi_Shortwave_Radiation_Downwelling_wattPerMeterSquared))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = 70, label.y = 300)+
  stat_regline_equation(label.x = 70, label.y = 350, aes(label = ..rr.label..)) + mytheme


wind_plot <- ggplot(boundmets, aes(y = era5_Ten_Meter_Elevation_Wind_Speed_meterPerSecond, x = ewembi_Ten_Meter_Elevation_Wind_Speed_meterPerSecond))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = 2, label.y = 7.5)+
  stat_regline_equation(label.x = 2, label.y = 8.5, aes(label = ..rr.label..)) + mytheme


airtemp_plot <- ggplot(boundmets, aes(y = era5_Air_Temperature_celsius, x = ewembi_Air_Temperature_celsius))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
  stat_regline_equation(label.x = -10, label.y = 20)+
  stat_regline_equation(label.x = -10, label.y = 25, aes(label = ..rr.label..)) + mytheme




ggarrange(relhum_plot, rainfall_plot, pressure_plot, lwradiation_plot, swradiation_plot, wind_plot, airtemp_plot, 
          labels = c("A", "B", "C", "D", "E", "F", "G"), 
          ncol = 2, nrow = 4, common.legend = TRUE, legend = "bottom")
ggsave('../figures/figureS2.png', dpi = 300,width = 384,height = 500, units = 'mm') 




