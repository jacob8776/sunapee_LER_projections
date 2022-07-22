library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(here)
# Set working directory
getwd()
setwd(paste0(here(), '/LER_calibration'))



model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")


ncdf <- "./output/ensemble_output_all_models_15Nov21.nc"

lst <- load_var(ncdf, "temp")
summary(lst$Obs)

png("../figures/figure3.png", width = 5,height = 7, units = 'in', res = 200)

plot_heatmap(ncdf, model = model) +
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()


dev.off()

