Sys.setenv(TZ = "UTC")

# Load libraries
library(gotmtools)
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
ncdf <- "output/ensemble_output.nc"

config_file

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

# UNCOMMENT BELOW LINE AND TITLE IT FOR CALIBRATION
#out_f <- ""
#folder <- "."
#dir.create(out_f, showWarnings = FALSE)

last_cal <- "calibration_results_MyLake_14Nov21"

cmethod <- "LHC"
model <- c("MyLake")






cal_files <- list.files(last_cal, full.names = TRUE)
cal_files <- cal_files[c(1,2)]

res <- load_LHC_results(config_file = config_file, model = model, res_files = cal_files)
dim(res[[model]])


df <- plyr::ldply(res, function(x) {
  df <- x[, -c(3:7)]
  reshape2::melt(df, id.vars = c("par_id", "rmse"))
}, .id = "model")
df$id_no <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$par_id)) 

bst_par <- df$id_no[which.min(df$rmse)]
sub <- df[df$id_no == bst_par, ]
sub

p1 <- ggplot(df) +
  geom_point(aes(value, rmse)) +
  facet_wrap(model~variable, scales = "free_x") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  ylab("RMSE (\u00B0C)") +
  geom_vline(data = sub, aes(xintercept = value)) +
  geom_hline(yintercept = 3.5, color = "red", linetype = "dashed") +
  # coord_cartesian(ylim = c(1, 4)) +
  # scale_x_log10() +
  theme_classic(base_size = 16)
p1





##############################################################################################
##############################################################################################
##############################################################################################
###### RUNNING CALIBRATION WILL CHANGE THE YAML FILE AND AFFECT PROJECTIONS ##################
##############################################################################################
##############################################################################################
##############################################################################################

## Run LER and inspect default output
#export_config(config_file, model)
#
#run_ensemble(config_file = config_file, model = model)
#
#lst <- load_var(ncdf, "temp")
#summary(lst$Obs)
#
#
## plot heatmap
#plot_heatmap(ncdf, model = model) +
#  scale_colour_gradientn(limits = c(0, 32),
#                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()
#
#plot_ensemble(ncdf, model = model, var = "ice_height")
#
#fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
#fit
#
#out <- analyze_ncdf(ncdf, model, spin_up = 190)
#out$stats 
#
### Plot residuals
#plist <- plot_resid(ncdf = ncdf, var = "temp")
#ggarrange(plotlist = plist)
#
#
#cali_ensemble(config_file, num = num, cmethod = cmethod, parallel = TRUE, model = model, folder = ".", 
#              spin_up = spin_up, job_name = model, out_f = out_f)
#
#cal_files <- list.files(out_f, full.names = TRUE)
#cal_files <- cal_files[c(1,2)]
#
#res <- load_LHC_results(config_file = config_file, model = model, res_files = cal_files)
#dim(res[[model]])
#
#
#df <- plyr::ldply(res, function(x) {
#  df <- x[, -c(3:7)]
#  reshape2::melt(df, id.vars = c("par_id", "rmse"))
#}, .id = "model")
#df$id_no <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$par_id)) 
#
#bst_par <- df$id_no[which.min(df$rmse)]
#sub <- df[df$id_no == bst_par, ]
#sub
#
#p1 <- ggplot(df) +
#  geom_point(aes(value, rmse)) +
#  facet_wrap(model~variable, scales = "free_x") +
#  geom_hline(yintercept = 2, linetype = "dashed") +
#  ylab("RMSE (\u00B0C)") +
#  geom_vline(data = sub, aes(xintercept = value)) +
#  geom_hline(yintercept = 3.5, color = "red", linetype = "dashed") +
#  # coord_cartesian(ylim = c(1, 4)) +
#  # scale_x_log10() +
#  theme_classic(base_size = 16)
#p1
#
##
#sub <- df[df$id_no == bst_par, ]
## sub <- df[df$id_no == 1, ] # Use this to try other parameter combinations
#sub
#
#
# yaml$scaling_factors$GLM$wind_speed <- sub$value[1]
# yaml$scaling_factors$GLM$swr <- sub$value[2]
# yaml$model_parameters$GLM$`sediment/sed_temp_mean` <- c(sub$value[3], sub$value[4])
# yaml$model_parameters$GLM$`glm_setup/max_layer_thick` <-  sub$value[5]
#
#
#
#write_yaml(yaml, config_file)
#
#export_config(config_file, model)
#
#run_ensemble(config_file, model)
#
#fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
#fit # Results from running model with calib output as input
#sub # Calibration results
#
#plist <- plot_resid(ncdf = "output/ensemble_output.nc", var = "temp")
#ggarrange(plotlist = plist)
##ggsave(file.path(out_f, "calib_results.png"), p1,  dpi = 300,width = 384, height = 280, units = 'mm')
#
#