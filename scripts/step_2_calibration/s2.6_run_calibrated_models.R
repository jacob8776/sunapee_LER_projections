Sys.setenv(TZ = "UTC")

# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "sunp-isimip")
# remotes::install_github("tadhg-moore/gotmtools", ref = "yaml")
# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("aemon-j/gotmtools", ref = "yaml", force = TRUE)
# devtools::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# install.packages("here")


# Load libraries
library(gotmtools)
library(GLMr)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(here)
# Set working directory
getwd()
setwd(paste0(here::here(), '/LER_calibration'))

# Set config file & models
config_file <- 'LakeEnsemblRsun.yaml'
model <- c("Simstrat")
ncdf <- "output/ensemble_output.nc"


# LHC - Calibration ----

yaml <- read_yaml(config_file)
configr::read.config(config_file)
yaml$time$start <- "2005-06-27 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
yaml$input$ice$use <- TRUE
yaml$output$time_step <- 24
yaml$output$time_unit <- "hour"
write_yaml(yaml, config_file)
num <- 500
spin_up <- 180
out_f <- "change"

cmethod <- "LHC"
model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")


folder <- "."
dir.create(out_f, showWarnings = FALSE)


# Run LER and inspect default output
export_config(config_file, model)

run_ensemble(config_file = config_file, model = model, verbose = TRUE)

Sys.Date()

file.rename("output/ensemble_output.nc", paste0("output/ensemble_output_all_models_", as.character(Sys.Date()), ".nc"))
ncdf <- paste0("output/ensemble_output_all_models_", as.character(Sys.Date()), ".nc")

lst <- load_var(ncdf, "temp")
summary(lst$Obs)


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

