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


####################### Calibration bias #############################################


setwd(paste0(here::here(), "/LER_calibration/cali_calcs/"))


wideform <- read.csv("surface_1m_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)




paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("bottom_33m_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))

wideform <- read.csv("schmidt_cali_wideform.csv")


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("thermodepth_cali_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("totstratdur_cali_wideform.csv")


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))

wideform <- read.csv("toticedur_cali_wideform.csv")


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



####################### Validation bias #############################################


setwd(paste0(here::here(), "/LER_validation/vali_calcs/"))


#wideform <- read.csv("surface_1m_wideform_vali.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("bottom_33m_wideform_vali.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)


paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))


wideform <- read.csv("schmidt_vali_wideform.csv")


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("thermodepth_vali_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("totstratdur_vali_wideform.csv")

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))

wideform <- read.csv("toticedur_vali_wideform.csv")


paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))


