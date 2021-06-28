library(LakeEnsemblR)
library(lubridate)
library(plyr)

setwd("~/Dropbox/sunapee_LER_projections/LER_projections")

gcm <- c("GFDL-ESM2M", "IPSL")
rcp <- c("historical", "rcp85")

for(g in gcm) {
  for(r in rcp) {
    fname <- paste0(g, "_", r, "_output.nc")
    
    temp <- load_var(fname, "temp")
    temp$Obs <- NULL
    
    if( r == "historical") {
      # Calculating historical mean 1975- 2005
      hist_mean <- lapply(1:length(temp), function(temp[[x]]) {
        temp[[x]]$year <- year(temp[[x]]$datetime)
        temp[[x]]$month <- month(temp[[x]]$datetime)
        sub <- temp[[x]][temp[[x]]$year >= 1975 & temp[[x]]$year <= 2005 & sub$month >=6 & sub$month < 9, ]
        mean(sub$wtr_0, na.rm = TRUE)
      })
    } else {
      anom <- lapply(1:length(temp), function(x) {
        temp[[x]]$year <- year(temp[[x]]$datetime)
        temp[[x]]$month <- month(temp[[x]]$datetime)
        
        # Subset data by month/year etc.
        sub <- temp[[x]][temp[[x]]$year >= 1975 & temp[[x]]$year <= 2025 & temp[[x]]$month >=6 & temp[[x]]$month < 9, ]
        
        # Future annual mean 2006-2099
        mn <- ddply(sub[, -1], "year", function(y) {
          ann_mean <- mean(y$wtr_0, na.rm  = TRUE)
          return(data.frame(ann_mean))
        })
        # Calculate anomaly
        mn$anom <- mn$ann_mean - hist_mean[[x]]
        
      })
    }
    
    
  }
}


