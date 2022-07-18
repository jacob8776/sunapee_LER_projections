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


# 
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# # # List of RCP's to run 
rcp <- c("rcp26", "rcp60", "rcp85") 




setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")


anomalies_master <- data.frame("year" = numeric(0), 
                               "rcp" = character(0), "gcm" = character(0), "model" = character(0), "variable" = character(0),
                               "value" = numeric(0), "mean" = numeric(0), "anom" = numeric(0))



for(i in 1:length(gcm)){
  # i = 1
  # Sets working directory to each gcm 
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # l = 1
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    print(ncdf)
    
    # out <- load_var(ncdf = ncdf, var = "temp")
    # out <- out[1:5]
    
    temp <- load_var(ncdf, "temp")

    
    df <- melt(temp[1:5], id.vars = 1)
    colnames(df)[4] <- "model"
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    df$year <- year(df$datetime)
    
    hmeans <- df %>%  
      filter(year >= 1975 & year <= 2005) %>% 
      dplyr::group_by(model, gcm, rcp, variable) %>% 
      dplyr::mutate(mean = mean(value, na.rm = TRUE))
    
     hmeans_historical <- df %>% 
      filter(year >= 1975 & year <= 2005) %>% 
      dplyr::group_by(variable) %>% 
      dplyr::mutate(mean = mean(value, na.rm = TRUE))
     
     hmeans_historical$rcp <- "historical"
     
     mean_all_historical <- hmeans_historical %>% 
       dplyr::filter(model != "Obs") %>% 
       distinct(mean, .keep_all = TRUE) %>% 
       # mutate(gcm = gcm[[i]],
       #        rcp = rcp[[l]]) %>% 
       select(variable, model, mean, gcm)
    
    mean_all <- hmeans %>% 
      dplyr::filter(model != "Obs") %>% 
      distinct(mean, .keep_all = TRUE) %>% 
      # mutate(gcm = gcm[[i]],
      #        rcp = rcp[[l]]) %>% 
      select(variable, model, mean, gcm)
    
    
    anomalies_historical <- merge(mean_all_historical, df, by = c("variable", "model", "gcm"
    )) %>% 
      filter(year <= 2005) %>% 
      mutate(anom = value - mean) %>%
      dplyr::filter(model != "Obs") %>%  
      select(year, rcp, gcm, model, 
             variable, value, mean, anom)
    anomalies_historical$rcp <- "historical"
    
    
    
    
    anomalies <- merge(mean_all, df, by = c("variable", "model", "gcm"
    )) %>% 
      filter(year >= 2006) %>% 
      mutate(anom = value - mean) %>%
      dplyr::filter(model != "Obs") %>% 
      select(year, rcp.y, gcm, model, 
             variable, value, mean, anom)
    colnames(anomalies)[2] <- c("rcp")
    
    
    anomalies <- rbind(anomalies, anomalies_historical)
    
    anomalies$variable <- as.character(anomalies$variable)
    
    anomalies_master <- rbind(anomalies_master, anomalies)
    
    
    
    
    
    
    
    
  }
}




anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

anomalies_master <- anomalies_by_year %>% 
  group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_model = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_model = var(anom, na.rm = TRUE)) %>% 
  group_by(rcp, model, variable, year) %>%
  dplyr::mutate(mean_gcm = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_gcm = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_gcm = var(anom, na.rm = TRUE)) %>% 
  group_by(gcm, model, variable, year) %>% 
  dplyr::mutate(mean_rcp = mean(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(sd_rcp = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_rcp = var(anom, na.rm = TRUE))


write.csv(anomalies_master, "../../anomaly_calculations/surf_bot_anomalies.csv", row.names = F)

anomalies_master <- read.csv("../../anomaly_calculations/surf_bot_anomalies.csv")