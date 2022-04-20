Sys.setenv(TZ = "UTC")

# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("tadhg-moore/gotmtools", ref = "yaml")
# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("aemon-j/gotmtools", ref = "yaml", force = TRUE)
# devtools::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")

# Load libraries
library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
# Set working directory
getwd()
setwd("~/Dropbox/sunapee_LER_projections/LER_validation/")

# Set config file & models
config_file <- 'LakeEnsemblRsun_val.yaml'
model <- c("Simstrat")
ncdf <- "output/ensemble_output.nc"

config_file

# mantemp <- read.csv("Data/manual_buoy_temp_hrz.csv")
# str(mantemp)
# mantemp$datetime <- as.POSIXct(mantemp$datetime, format = "%Y-%m-%d %H:%M:%S")
# write.csv(mantemp, "Data/manual_buoy_temp_hrz_psx.csv", row.names = FALSE)

# LHC - Calibration ----

yaml <- read_yaml(config_file)
configr::read.config(config_file)
yaml$time$start <- "2015-06-11 00:00:00"
yaml$time$stop <- "2020-01-01 00:00:00"
# yaml$time$start <- "2007-06-11 00:00:00"
# yaml$time$stop <- "2012-01-01 00:00:00"
yaml$input$ice$use <- TRUE
yaml$output$time_step <- 24
yaml$output$time_unit <- "hour"
write_yaml(yaml, config_file)
num <- 500
spin_up <- 180
out_f <- "change"

cmethod <- "LHC"
model <- c("FLake", "Simstrat", "GOTM", "GLM", "MyLake")

folder <- "."
dir.create(out_f, showWarnings = FALSE)


# Run LER and inspect default output
export_config(config_file, model)

run_ensemble(config_file = config_file, model = model, verbose = TRUE)

# file.rename("output/ensemble_output.nc", "output/ensemble_output_all_models_15Nov21.nc")
# ncdf <- "output/ensemble_output_all_models_15Nov21.nc"

lst <- load_var(ncdf, "temp")
summary(lst$Obs)
checkdf <- lst$Obs

# plot heatmap
plot_heatmap(ncdf, model = model) +
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()

plot_ensemble(ncdf, model = model, var = "ice_height")

fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
fit

# out <- analyze_ncdf(ncdf, model, spin_up = 190)
# out$stats 

## Plot residuals
plist <- plot_resid(ncdf = ncdf, var = "temp")
ggarrange(plotlist = plist)



