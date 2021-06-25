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

gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")

# List of RCP's to run 
rcp <- c("historical", "rcp85")



# set config file for all simulations
config_file <- 'LakeEnsemblR.yaml'

# Set which models for all simulations
model <- c("GLM", "FLake", "Simstrat", "GOTM")


yaml <- read_yaml(config_file)
yaml$output$time_step <- "hour"
yaml$output$time_step <- 24
yaml$location$hypsograph <- c(file.path("../../../LER_inputs/sunapee_hypso.csv"))
# meteo file is changed to the GCM/RCP combination placed in the file structure


for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]]))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # Sets working directory specifying GCM and RCP scenario
      setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]], rcp[[l]]))
    # Ensure working directory is switching
    print(getwd())
    if(rcp[[l]] == "historical"){
      yaml$time$start <- "1975-01-01 12:00:00"
      yaml$time$stop <- "2005-12-31 00:00:00"
      yaml$observations$temperature$file <- c(file.path("../../../LER_inputs/ic_historical.csv"))
    }
    else{
      yaml$time$start <- "2006-01-01 12:00:00"
      yaml$time$stop <- "2099-12-31 00:00:00"
      yaml$observations$temperature$file <- c(file.path("../../../LER_inputs/ic_projections.csv"))
    }
    yaml$input$meteo$file <- c(list.files(file.path("../../../met_files_processed/", gcm[[i]]), full.names = TRUE, pattern = rcp[[l]]))
    # yaml$output$file <- c(file.path("../../output", paste0(gcm[[i]], "_", rcp[[l]], "_", "output.nc")))
    yaml$output$file <- c(paste0(gcm[[i]], "_", rcp[[l]], "_", "output"))
    write_yaml(yaml, config_file)
    print(yaml$observations$temperature$file)
    print(yaml$location$hypsograph)
    print(yaml$output$file)
    
    # 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file for all simulations
    export_config(config_file = config_file, model = model)

    # 2. Run ensemble lake models for all simulations
    run_ensemble(config_file = config_file, model = model)
    
  }
}









