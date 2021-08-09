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



gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")

# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("historical", "rcp26", "rcp85")




setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")


anomalies_master <- data.frame("year" = numeric(0), "month" = numeric(0), "yday" = numeric(0), 
                               "rcp" = character(0), "gcm" = character(0), "model" = character(0), "variable" = character(0),
                               "value" = numeric(0), "mean" = numeric(0), "anom" = numeric(0))


for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    print(ncdf)
    
    out <- load_var(ncdf = ncdf, var = "temp")
    #out <- as.data.frame(out[[1]])
    bathy <- read.csv('~/Dropbox/sunapee_LER_projections/LER_inputs/sunapee_hypso.csv')
    colnames(bathy) <- c("depths", "areas")
    ts.td <- lapply(out, function(x) {
      ts.thermo.depth(x, Smin = 0.1, na.rm = TRUE)
    })
    
    df <- melt(ts.td, id.vars = 1)
    colnames(df)[4] <- "model"
    df$yday <- yday(df$datetime)
    df$year <- year(df$datetime)
    df$month <- month(df$datetime)
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    df <- df %>% filter(month >= 6 & month <= 9)
    
    if(rcp[[l]] == "historical"){
      hmeans <- df %>%  dplyr::group_by(model) %>% 
        dplyr::mutate(mean = mean(value, na.rm = TRUE))
      
      mean_all <- hmeans %>% 
        distinct(mean, .keep_all = TRUE) %>% 
        # mutate(gcm = gcm[[i]],
        #        rcp = rcp[[l]]) %>% 
        select(variable, model, mean, gcm)
    }
    
    
    
    else{
      
      anomalies <- merge(mean_all, df, by = c("variable", "model", "gcm"
      )) %>% 
        mutate(anom = value - mean) %>%
        dplyr::filter(model != "Obs") %>% 
        select(year, month, yday, rcp, gcm, model, 
               variable, value, mean, anom)
      
      anomalies$variable <- as.character(anomalies$variable)
      
      anomalies_master <- rbind(anomalies_master, anomalies)
      
      
      
      
      
    }
    
    
  }
}




anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

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

write.csv(anomalies_master, "../../anomaly_calculations/thermodepth_annual_anomalies.csv", row.names = F)




ggplot(subset(anomalies_master, gcm == "GFDL-ESM2M"), aes(yday, anom, colour = model)) +
  facet_wrap(~year) +
  geom_line() +
  labs(y = "Thermocline Depth (m)") +
  theme_classic() 



anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050) 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099)
mean(anomalies_midcentury$anom, na.rm = TRUE)
mean(anom_endcentury$anom, na.rm = TRUE)
