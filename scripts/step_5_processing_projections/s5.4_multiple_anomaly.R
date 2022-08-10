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
library(here)


# 
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# # # List of RCP's to run 
rcp <- c("rcp26", "rcp60", "rcp85") 




setwd(paste0(here(), "/LER_projections/output/"))


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
    ice <- load_var(ncdf, "ice_height")
    # ice <- lapply(1:length(ice), function(x){
    #   ice[[x]] %>%
    #     mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))    })
    # names(ice) <- c("FLake", "MyLake", "GLM", "Simstrat", "GOTM", "Obs")
    
    
    out <- lapply(1:length(temp), function(x) {
      # x = 1 # for debugging
      mlt <- reshape::melt(temp[[x]], id.vars = 1)
      mlt[, 2] <- as.numeric(gsub("wtr_", "", mlt[, 2]))
      analyze_strat(data = mlt, H_ice = ice[[x]][, 2], dates = 6:8)
    })
    names(out) <- names(temp)
    

    

    df <- melt(out[1:5], id.vars = 1)
    colnames(df)[4] <- "model"
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    
    hmeans <- df %>%  
    filter(year >= 1975 & year <= 2005) %>% 
    dplyr::group_by(model, gcm, rcp, variable) %>% 
    dplyr::mutate(mean = mean(value, na.rm = TRUE))
      
    mean_all <- hmeans %>% 
      dplyr::filter(model != "Obs") %>% 
      distinct(mean, .keep_all = TRUE) %>% 
      # mutate(gcm = gcm[[i]],
      #        rcp = rcp[[l]]) %>% 
      select(variable, model, mean, gcm)
    
    
    
    

      anomalies <- merge(mean_all, df, by = c("variable", "model", "gcm"
      )) %>% 
        filter(year >= 1975) %>% 
        mutate(anom = value - mean) %>%
        dplyr::filter(model != "Obs") %>% 
        select(year, rcp.y, gcm, model, 
               variable, value, mean, anom)
      colnames(anomalies)[2] <- c("rcp")
      
      
      anomalies$variable <- as.character(anomalies$variable)
      
      anomalies_master <- rbind(anomalies_master, anomalies)
      
      
      
      
      
    
    
    
  }
}




anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


########################### COULD AFFECT VARIANCE DIFFERENCES ###############################################


# Model uncertainty grouped by all relevant columns except for lake model. Calculations are then carried out across lake models 
# for mean, standard deviation and variance. Same method for gcm uncertainty and rcp uncertainty. 

anomalies_master <- anomalies_master %>% 
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

##################################################################################################################


write.csv(anomalies_master, "../../anomaly_calculations/multiple_annual_anomalies.csv", row.names = F)
