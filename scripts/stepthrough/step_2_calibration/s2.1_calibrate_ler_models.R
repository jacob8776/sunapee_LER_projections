Sys.setenv(TZ = "UTC")

# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("tadhg-moore/gotmtools", ref = "yaml")
# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("aemon-j/gotmtools", ref = "yaml", force = TRUE)
# devtools::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# install.packages("here")


# Load libraries
library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(here)
# Set working directory
getwd()
setwd(paste0(here(), '/LER_calibration'))

# Set config file & models
config_file <- 'LakeEnsemblRsun.yaml'
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
yaml$time$start <- "2005-06-27 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
# yaml$time$start <- "2007-06-11 12:00:00"
# yaml$time$stop <- "2012-01-01 00:00:00"
yaml$input$ice$use <- TRUE
yaml$output$time_step <- 24
yaml$output$time_unit <- "hour"
write_yaml(yaml, config_file)
num <- 500
spin_up <- 190
out_f <- "change"

cmethod <- "LHC"
model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")

folder <- "."
dir.create(out_f, showWarnings = FALSE)


# Run LER and inspect default output
export_config(config_file, model)

run_ensemble(config_file = config_file, model = model)

# file.rename("output/ensemble_output.nc", "output/ensemble_output_all_models_15Nov21.nc")
# ncdf <- "output/ensemble_output_all_models_15Nov21.nc"

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


# param_file <- "calibration_results_MyLake_081321/MyLake_LHC_202108131525"

cali_ensemble(config_file, num = num, cmethod = cmethod, parallel = TRUE, model = model, folder = ".", 
              spin_up = spin_up, job_name = model, out_f = out_f)

cal_files <- list.files(out_f, full.names = TRUE)
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

#
sub <- df[df$id_no == bst_par, ]
# sub <- df[df$id_no == 1, ] # Use this to try other parameter combinations
sub

#yaml$model_parameters$MyLake$`Phys.par/C_shelter` <- sub$value[3]
#yaml$scaling_factors$MyLake$wind_speed <- sub$value[1]
#yaml$scaling_factors$MyLake$swr <- sub$value[2]

# yaml$model_parameters$FLake$`LAKE_PARAMS/c_relax_C` <- sub$value[3]
# yaml$scaling_factors$FLake$wind_speed <- sub$value[1]
# yaml$scaling_factors$FLake$swr <- sub$value[2]
# yaml$model_parameters$FLake$`LAKE_PARAMS/depth_bs_lk` <- sub$value[4]
# yaml$model_parameters$FLake$`LAKE_PARAMS/T_bs_lk` <- sub$value[5]

# yaml$scaling_factors$GLM$wind_speed <- sub$value[1]
# yaml$scaling_factors$GLM$swr <- sub$value[2]
# yaml$model_parameters$GLM$`sediment/sed_temp_mean` <- c(sub$value[3], sub$value[4])
# yaml$model_parameters$GLM$`glm_setup/max_layer_thick` <-  sub$value[5]

# yaml$scaling_factors$GOTM$wind_speed <- sub$value[1]
# yaml$scaling_factors$GOTM$swr <- sub$value[2]
# yaml$model_parameters$GOTM$`turbulence/turb_param/k_min` <- sub$value[3]

# yaml$scaling_factors$Simstrat$wind_speed <- sub$value[1]
# yaml$scaling_factors$Simstrat$swr <- sub$value[2]
# yaml$model_parameters$Simstrat$`ModelParameters/a_seiche` <-  sub$value[3]




write_yaml(yaml, config_file)

export_config(config_file, model)

run_ensemble(config_file, model)

fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
fit # Results from running model with calib output as input
sub # Calibration results

plist <- plot_resid(ncdf = "output/ensemble_output.nc", var = "temp")
ggarrange(plotlist = plist)
#ggsave(file.path(out_f, "calib_results.png"), p1,  dpi = 300,width = 384, height = 280, units = 'mm')

