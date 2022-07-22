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

gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR",
         "MIROC5")

# List of RCP's to run 
rcp <- c("historical", "rcp26", "rcp85")



for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]]))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # Sets working directory specifying GCM and RCP scenario
    setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]], rcp[[l]]))
    # Ensure working directory is switching
    print(getwd())
    
    
  }
}

