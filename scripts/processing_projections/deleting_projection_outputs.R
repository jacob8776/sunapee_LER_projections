## Script for clearing outputs of projections

setwd("~/Dropbox/sunapee_LER_projections/LER_projections/")

# List of GCM's to run 
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
    current_folder <- (file.path(paste0(getwd(), "/output/")))
    new_folder <- file.path("../../output")
    list_of_files <- list.files(current_folder)
    file.copy(list_of_files, new_folder)
    do.call(file.remove, list(list.files("output/", full.names = TRUE)))
  }
}




