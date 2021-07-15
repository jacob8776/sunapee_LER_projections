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






gcm <- c("GFDL-ESM2M")
# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("historical")



# anomalies <- data.frame("year" = numeric(0), "ann_mean" = numeric(0), 
#                         "anom" = numeric(0), "model" = character(0), 
#                         "variable" = character(0))

setwd("LER_projections/output/")


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
      df$month <- year(df$datetime)
      print(df)

      if(rcp[[l]] == "historical"){
            hmeans <- df %>%  dplyr::group_by(model) %>% 
            dplyr::mutate(mean = mean(value, na.rm = TRUE))
            
            models <- c("GLM", "FLake", "Simstrat", "GOTM", "MyLake")
            
            for(z in length(models)){
            glmmean <- hmeans %>% filter(model == models[[z]]) 
            glmmean <- glmmean[1,]
            }
        }
      

      }
    }

      unique(hmeans$mean)
      