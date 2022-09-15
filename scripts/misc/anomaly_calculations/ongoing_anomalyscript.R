library(LakeEnsemblR)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)




setwd("LER_projections/output/")


gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("historical", "rcp60")

for(i in length(gcm)) {
  for(l in length(rcp)) { 
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    
    temp <- load_var(ncdf, "temp")
    temp$obs <- NULL
    
    if (rcp[[l]] == "historical") {
      hist_mean <- lapply(1:length(temp), function(t) {
        t$year <- year(t$datetime)
        t$month <- month(t$datetime)
        sub <- tt[temp[[x]]$year >= 1975 & temp[[x]]$year <= 2005 & sub$month >=6 & sub$month < 9, ]
        mean(sub$wtr_0, na.rm = TRUE)
      })
      
    }
    

    
    
    
    }
}
























