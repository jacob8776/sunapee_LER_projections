# List of GCM's to run 
gcm <- c("GFDL-ESM2M")

# List of RCP's to run 
rcp <- c("historical", "rcp85")



setwd("~/Dropbox/sunapee_LER_projections/scripts/")

source("running_projections/run_all.R")

setwd("~/Dropbox/sunapee_LER_projections/scripts/")

source("processing_projections/moving_projection_outputs.R")

setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")

ncdf <- 'GFDL-ESM2M_historical_output.nc'


wtemp <- load_var(ncdf = ncdf, var = 'temp', return = 'list')


















