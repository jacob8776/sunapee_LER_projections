library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(ggpubr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)
library(rstudioapi) 
# Load LakeEnsemblR
library(LakeEnsemblR)


# Set working directory to the parent folder of all GCM/RCP combinations
setwd("~/Dropbox/sunapee_LER_projections/LER_projections/")

# List of GCM's to run 
gcm <- c("GFDL", "HADGEM", "IPSL", "MIROC5")

# List of RCP's to run 
rcp <- c("piControl", "rcp85")








pdf("test.pdf")
for(i in 1:length(gcm)){
  setwd(file.path("~/Dropbox/JHW_thesis/Draft1/LER/", gcm[[i]]))
  for(l in 1:length(rcp)){
    setwd(file.path("~/Dropbox/JHW_thesis/Draft1/LER/", gcm[[i]], rcp[[l]]))
    ncdf <- 'output/ensemble_output.nc'
    print(plot_heatmap(ncdf))
    
  }
}
dev.off()

