# Test GLM ice model ----

library(gotmtools)
library(LakeEnsemblR)

setwd("LER_calibration/")

# Set config file & models
config_file <- 'LakeEnsemblRsun.yaml'
ncdf <- "output/ensemble_output.nc"

yaml <- read_yaml(config_file)
yaml$time$start <- "2005-06-27 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
yaml$input$ice$use <- TRUE
yaml$output$time_step <- 24
yaml$output$time_unit <- "hour"
write_yaml(yaml, config_file)
model <- c("GLM")


# Run LER and inspect default output
export_config(config_file, model)

# Add snowice chunk to nml file
nml <- glmtools::read_nml("GLM/glm3.nml")
nml$snowice <- list(snow_albedo_factor = 1,
                    snow_rho_max = 500,
                    snow_rho_min = 100,
                    dt_iceon_avg = 0.042) # Adjust for ice (0.01 - 1)
glmtools::write_nml(nml, "GLM/glm3.nml")

run_ensemble(config_file = config_file, model = model)

# Read in data for plotting
met <- readr::read_csv("GLM/meteo_file.csv")
temp <- load_var(ncdf, "temp")
ice <- load_var(ncdf, "ice_height")

library(ggplot2)
library(patchwork)
p <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = met, aes(Date, AirTemp, color = "AirT")) +
  geom_line(data = temp$GLM, aes(datetime, wtr_0, color = "wtemp")) +
  coord_cartesian(xlim = range(temp$GLM$datetime))
p /
ggplot() +
  geom_line(data = ice$GLM, aes(time, ice_height))

# end
