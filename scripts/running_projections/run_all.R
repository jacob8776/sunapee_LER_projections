#


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
setwd("~/Dropbox/JHW_thesis/Draft1/LER/")

# List of GCM's to run 
gcm <- c("GFDL", "HADGEM", "IPSL", "MIROC5")

# List of RCP's to run 
rcp <- c("piControl", "rcp85")

# set config file for all simulations
config_file <- 'LakeEnsemblRsun.yaml'

# Set which models for all simulations
model <- c("GLM", "FLake", "Simstrat", "GOTM")

# Looping through each GCM with specified RCP scenarios
for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path("~/Dropbox/JHW_thesis/Draft1/LER/", gcm[[i]]))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # Sets working directory specifying GCM and RCP scenario
    setwd(file.path("~/Dropbox/JHW_thesis/Draft1/LER/", gcm[[i]], rcp[[l]]))
    # Ensure working directory is switching
    print(getwd())
    
    ## yaml inputs are changed for all simulations
    yaml <- read_yaml(config_file)
    yaml$time$start <- "2006-05-30 00:00:00"
    yaml$time$stop <- "2007-01-01 00:00:00"
    yaml$output$time_step <- "hour"
    yaml$output$time_step <- 24
    # meteo file is changed to the GCM/RCP combination placed in the file structure
    yaml$input$meteo$file <- c(list.files("Data", full.names = TRUE, pattern = "piControl|rcp85"))
    # Double checking working directory and meteo file match up
    print(yaml$input$meteo$file)
    write_yaml(yaml, config_file)
    # 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file for all simulations
    export_config(config_file = config_file, model = model)

    # 2. Run ensemble lake models for all simulations
    run_ensemble(config_file = config_file, model = model)

  }
}


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



#file.rename



