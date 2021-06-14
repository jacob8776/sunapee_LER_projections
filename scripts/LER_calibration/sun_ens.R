# Install packages - Ensure all packages are up to date - parallel development ongoing
#install.packages("devtools")
#devtools::install_github("aemon-j/GLM3r", ref = "v3.1.1")
#devtools::install_github("USGS-R/glmtools", ref = "ggplot_overhaul")
#devtools::install_github("aemon-j/FLakeR", ref = "inflow")
#devtools::install_github("aemon-j/GOTMr")
#devtools::install_github("aemon-j/gotmtools")
#devtools::install_github("aemon-j/SimstratR")
#devtools::install_github("aemon-j/MyLakeR")
devtools::install_github("FLARE-forecast/GLM3r")
#remotes::install_github("aemon-j/FLakeR", ref = "inflow", force = TRUE)

# Load libraries
library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(ggpubr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)

# Load LakeEnsemblR
library(LakeEnsemblR)
setwd("~/Dropbox/JHW_thesis/sunapee_ensemble_v2/")


# Set config file & models
config_file <- 'LakeEnsemblRsun.yaml'
model <- c("GLM")

yml <- read_yaml(config_file)
yml$time$start <- "2005-06-26 00:00:00"

write_yaml(yml, config_file)

get_template("LakeEnsemblR_config")




# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
export_config(config_file = config_file, model = model)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model)



# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare", force = T)
# remotes::install_github("AEMON-j/gotmtools", ref = "yaml", force = TRUE)






################## Ignore for now ###################################


ncdf <- 'output/ensemble_output.nc'
vars <- gotmtools::list_vars(ncdf)
vars # Print variables

p1 <- plot_heatmap(ncdf)

out <- load_var(ncdf, "temp")
names(out)
out$MyLake

plot(p1)

calc_fit(ncdf = "output/ensemble_output.nc", model = model)

cl <- parallel::makeCluster(7, setup_strategy = "sequential")

if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

cali_res <- cali_ensemble(config_file = config_file, num = 50, cmethod = "LHC",
                          parallel = TRUE, model = model)


plot_LHC(config_file = config_file, model = "GOTM", res_files = unlist(cali_res),
         qual_met = "nse", best = "high")


plot_LHC(config_file = config_file, model = model, res_files = unlist(cali_res),
         qual_met = "nse", best = "high")

res_LHC <- load_LHC_results(config_file = config_file, model = model, res_files = unlist(cali_res))

best_p <- setNames(lapply(model, function(m)res_LHC[[m]][which.min(res_LHC[[m]]$rmse), ]), model)
print(best_p)

best_par <- setNames(lapply(model, function(m)cali_res[[m]]$bestpar), model)
print(best_par)

plot(p1)
ens_out <- "output/ensemble_output.nc"

p <- plot_ensemble(ncdf = ens_out, model = c( 'GLM',  'GOTM', 'Simstrat'), depth = 2,
                   var = 'temp',  boxwhisker = TRUE, residuals = TRUE)
plot(p)

