library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)
library(lubridate)
library(reshape2)
library(scales)
library(plotrix)
library(Metrics)
library(here)

setwd(paste0(here::here(), "/LER_calibration/cali_calcs/"))

####################### Calibration RMSE #############################################


model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")
spin_up <- 180

ncdf <- "../output/ensemble_output_all_models_2022-08-16.nc"

fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
fit # Results from running model with calib output as input


wideform <- read.csv("surface_1m_wideform.csv")

wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)

paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("bottom_33m_wideform.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))

paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("schmidt_cali_wideform.csv")

wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("thermodepth_cali_wideform.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)

paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("totstratdur_cali_wideform.csv")


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("toticedur_cali_wideform.csv")


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))



####################### Validation RMSE #############################################


setwd(paste0(here::here(), "/LER_validation/vali_calcs/"))


model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")
spin_up <- 180

ncdf <- "../output/ensemble_output.nc"

fit <- calc_fit(ncdf, model = model, spin_up = spin_up)
fit # Results from running model with calib output as input



wideform <- read.csv("surface_1m_wideform_vali.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("bottom_33m_wideform_vali.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)

paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("schmidt_vali_wideform.csv")

wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("thermodepth_vali_wideform.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))



wideform <- read.csv("totstratdur_vali_wideform.csv")


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))


wideform <- read.csv("toticedur_vali_wideform.csv")


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))















