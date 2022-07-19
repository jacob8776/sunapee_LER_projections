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


setwd(paste0(here(), "/LER_calibration/cali_calcs/"))

# 
png("../../figures/figure4.png", width = 5,height = 7, units = 'in', res = 200)
par(mfrow=c(3,2))
wideform <- read.csv("surface_1m_wideform.csv")

# red simstrat 
# green glm 
# bluegreen flake 
# yellow mylake 
# black gotm


par(mar=c(5, 4, 4, 8), xpd=TRUE)
taylor.diagram(wideform$Obs,wideform$Simstrat,  main = "A) Summer Surface Temperature", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)

# legend("topright", inset = c(-0.1, -0.1),
#        legend = c("Simstrat", "GLM", "FLake", "MyLake",
#                   "GOTM", "Ensemble Mean"),
#        fill = c(1,2,3,4,5,6))



wideform <- read.csv("bottom_33m_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "B) Summer Bottom Temperature", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



wideform <- read.csv("schmidt_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "C) Schmidt Stability", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



wideform <- read.csv("thermodepth_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$MyLake, main = "D) Summer Thermocline Depth", ref.sd = TRUE, col = 4, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$Simstrat, add = TRUE, col = 1)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



wideform <- read.csv("totstratdur_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "E) Summer Stratification Duration", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



wideform <- read.csv("toticedur_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "F) Ice Off", ref.sd = TRUE, col = 1, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
#taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



dev.off()


