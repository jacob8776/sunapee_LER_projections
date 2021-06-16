## Script for clearing outputs of projections

setwd("~/Dropbox/sunapee_LER_projections/LER_projections/")

# List of GCM's to run 
gcm <- c("GFDL-ESM2M")

# List of RCP's to run 
rcp <- c("rcp26", "rcp85")


do.call(file.remove, list(list.files("~/Dropbox/sunapee_LER_projections/LER_projections/output/", full.names = TRUE)))


for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]]))
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # Sets working directory specifying GCM and RCP scenario
    setwd(file.path("~/Dropbox/sunapee_LER_projections/LER_projections/", gcm[[i]], rcp[[l]]))
    print(getwd())
    fil <- list.files(path = "output")
    # Ensure working directory is switching
    file.rename(paste0(getwd(), "/output/" ,fil), paste0("~/Dropbox/sunapee_LER_projections/LER_projections/output/", fil))
    
    
    
    # current_folder <- (file.path(paste0(getwd(), "/output/")))
    # new_folder <- file.path("~/Dropbox/sunapee_LER_projections/LER_projections/output/")
    # list_of_files <- list.files(current_folder)
    # file.copy(list_of_files, new_folder)
    # do.call(file.remove, list(list.files("output/", full.names = TRUE)))
  }
}




