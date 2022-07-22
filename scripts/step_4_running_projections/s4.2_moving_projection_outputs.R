## Script for organizing outputs of projections

setwd(paste0(here::here(), "/LER_projections/"))

# List of GCM's to run 
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")

# List of RCP's to run 
rcp <- c("rcp26", "rcp60", "rcp85")




do.call(file.remove, list(list.files(paste0(here::here(), "/LER_projections/output/"), full.names = TRUE)))


for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path(paste0(here::here(), "/LER_projections/", gcm[[i]])))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # Sets working directory specifying GCM and RCP scenario
    setwd(file.path(paste0(here::here(), "/LER_projections/"), gcm[[i]], rcp[[l]]))
    print(getwd())
    fil <- list.files(path = "output")
    # Ensure working directory is switching
    file.rename(paste0(getwd(), "/output/" ,fil), paste0(here::here(), "/LER_projections/output/", fil))
  }
}

