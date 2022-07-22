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

## total strat duration

anomalies_master <- filter(anomalies, variable == "TotStratDur")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

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


par(mfrow=c(2,2))
boxplot(anom_midcentury$mean_model ~ anom_midcentury$rcp)
boxplot(anom_midcentury$mean_gcm ~ anom_midcentury$rcp)
boxplot(anom_endcentury$mean_model ~ anom_endcentury$rcp)
boxplot(anom_endcentury$mean_gcm ~ anom_endcentury$rcp)



library("vioplot")

par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_2.6,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_2.6,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot

legend("topleft",
       legend = c("gcm", "model"),
       fill = c("#5773CC", "#FFB900"))

#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_6.0,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_6.0,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot

legend("topleft",
       legend = c("gcm", "model"),
       fill = c("#5773CC", "#FFB900"))


#### rcp 8.5

anomaly_model_8.5 <- anom_midcentury_8.5$mean_model
anomaly_gcm_8.5 <- anom_midcentury_8.5$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_8.5,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_8.5,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot

legend("topleft",
       legend = c("gcm", "model"),
       fill = c("#5773CC", "#FFB900"))







## total ice duration

anomalies_master <- filter(anomalies, variable == "TotIceDur")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

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



par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_2.6,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_2.6,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot



#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_6.0,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_6.0,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot




#### rcp 8.5

anomaly_model_8.5 <- anom_midcentury_8.5$mean_model
anomaly_gcm_8.5 <- anom_midcentury_8.5$mean_gcm

# Plot each side and join them
vioplot(anomaly_model_8.5,
        plotCentre = "line", # Median with a line
        side = "right",   # Right side
        col = "#5773CC")  # Color for the right side
vioplot(anomaly_gcm_8.5,
        plotCentre = "line", # Median with a line
        side = "left",     # Left side
        col = "#FFB900",   # Color for the left side
        add = TRUE)        # Add it to the previous plot








