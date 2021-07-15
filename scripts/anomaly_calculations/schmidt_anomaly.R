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
rcp <- c("historical", "rcp85")




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
      ts.sch <- lapply(out, function(x) {
        ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
      })
      ## Reshape to data.frame
      df <- melt(ts.sch, id.vars = 1)
      colnames(df)[4] <- "model"
      df$yday <- yday(df$datetime)
      df$year <- year(df$datetime)
      df$month <- month(df$datetime)
      df$gcm <- gcm[[i]]
      df$rcp <- rcp[[l]]

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
  

  anomalies_master <- anomalies_master %>% 
  group_by(rcp, gcm, variable, year, month, yday) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_model = sd(anom, na.rm = TRUE)) %>% 
  group_by(rcp, model, variable, year, month, yday) %>%
  dplyr::mutate(mean_gcm = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_gcm = sd(anom, na.rm = TRUE))
  
  
  



ggplot(subset(anomalies_master, gcm = "GFDL-ESM2M"), aes(yday, anom, colour = model)) +
  facet_wrap(~year) +
  geom_line() +
  labs(y = "Schmidt stability (J/m2)") +
  theme_classic() + ylim(-50, 1000)



ggplot(subset(anomalies_master, year <= 2010), aes(yday, mean_model, colour = gcm)) + 
  facet_wrap(~year) + 
  geom_line() + 
  geom_ribbon(data = subset(anomalies_master, year <= 2010), aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model), alpha = 0.4,
              linetype = 0.1,
              color = "grey") 


ggplot(subset(anomalies_master), aes(yday, mean_gcm, colour = model)) + 
  facet_wrap(~year) + 
  geom_line() + 
  geom_ribbon(data = subset(anomalies_master), aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm), alpha = 0.4,
              linetype = 0.1,
              color = "grey") 

anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050) 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099)

mean_midcentury <- mean(anom_midcentury$anom, na.rm = TRUE)
mean_endcentury <- mean(anom_endcentury$anom, na.rm = TRUE)
max(anom_midcentury$anom, na.rm = TRUE)
max(anom_endcentury$anom, na.rm = TRUE)
median(anom_midcentury$anom, na.rm = TRUE)
median(anom_endcentury$anom, na.rm = TRUE)

as.data.frame(x = c(mean_midcentury, mean_endcentury))

barplot(mean)

boxplot(anom_midcentury$anom, anom_endcentury$anom)






