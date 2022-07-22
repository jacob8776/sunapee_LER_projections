library(LakeEnsemblR)
library(lubridate)
library(plyr)
library(gotmtools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)
library(scales)

anomalies <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")

## Maximum surface temperature

anomalies_master <- filter(anomalies, variable == "TotStratDur")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

mean(anom_midcentury$mean_model, na.rm = TRUE)
max(anom_midcentury$mean_model, na.rm = TRUE)
median(anom_midcentury$mean_model, na.rm = TRUE)
sum(anom_midcentury$var_model)
mean(anom_endcentury$mean_model, na.rm = TRUE)
max(anom_endcentury$mean_model, na.rm = TRUE)
median(anom_endcentury$mean_model, na.rm = TRUE)
sum(anom_endcentury$var_model, na.rm = TRUE)



mean(anom_midcentury$mean_gcm, na.rm = TRUE)
max(anom_midcentury$mean_gcm, na.rm = TRUE)
median(anom_midcentury$mean_gcm, na.rm = TRUE)
sum(anom_midcentury$var_gcm)
mean(anom_endcentury$mean_gcm, na.rm = TRUE)
max(anom_endcentury$mean_gcm, na.rm = TRUE)
median(anom_endcentury$mean_gcm, na.rm = TRUE)
sum(anom_endcentury$var_gcm, na.rm = TRUE)



boxplot(anom_midcentury$mean_model ~ anom_midcentury$rcp)



## total ice duration

anomalies_master <- filter(anomalies, variable == "TotIceDur")

anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")

mean(anom_midcentury$mean_model, na.rm = TRUE)
max(anom_midcentury$mean_model, na.rm = TRUE)
median(anom_midcentury$mean_model, na.rm = TRUE)
sum(anom_midcentury$var_model)
mean(anom_endcentury$mean_model, na.rm = TRUE)
max(anom_endcentury$mean_model, na.rm = TRUE)
median(anom_endcentury$mean_model, na.rm = TRUE)
sum(anom_endcentury$var_model, na.rm = TRUE)



mean(anom_midcentury$mean_gcm, na.rm = TRUE)
max(anom_midcentury$mean_gcm, na.rm = TRUE)
median(anom_midcentury$mean_gcm, na.rm = TRUE)
sum(anom_midcentury$var_gcm, na.rm = TRUE)
mean(anom_endcentury$mean_gcm, na.rm = TRUE)
max(anom_endcentury$mean_gcm, na.rm = TRUE)
median(anom_endcentury$mean_gcm, na.rm = TRUE)
sum(anom_endcentury$var_gcm, na.rm = TRUE)

