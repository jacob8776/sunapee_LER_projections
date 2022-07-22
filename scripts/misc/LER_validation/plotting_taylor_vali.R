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


setwd("~/Dropbox/sunapee_LER_projections/LER_validation/vali_calcs/")

# 
png("~/Dropbox/jhwthesis/figures/taylor_diagram_vali.png", width = 5,height = 7, units = 'in', res = 200)
par(mfrow=c(3,2))
wideform <- read.csv("surface_1m_wideform_vali.csv")

# red simstrat 
# green glm 
# bluegreen flake 
# yellow mylake 
# black gotm


par(mar=c(5, 4, 4, 8), xpd=TRUE)
taylor.diagram(wideform$Obs,wideform$Simstrat,  main = "Summer Surface Temperature", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)

# legend("topright", inset = c(-0.1, -0.1),
#        legend = c("Simstrat", "GLM", "FLake", "MyLake",
#                   "GOTM", "Ensemble Mean"),
#        fill = 1:6)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("bottom_33m_wideform_vali.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Bottom Temperature", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)


paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))


wideform <- read.csv("schmidt_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Schmidt Stability", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("thermodepth_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Thermocline Depth", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))



wideform <- read.csv("totstratdur_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Stratification Duration", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))

wideform <- read.csv("toticedur_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Ice Off", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)


paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))


wideform <- read.csv("mixper_vali_wideform.csv")

# taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Total Mixing Period", ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
# taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 3)
# taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 5)
# taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 7)
# taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 9)
# taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 10)

paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake bias: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM bias: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat bias: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake bias: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM bias: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble bias: ", bias(wideform$Obs, wideform$mean))


dev.off()


