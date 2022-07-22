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


setwd(paste0(here(), "/LER_validation/vali_calcs/"))

# 
png("../../figures/figureS3.png", width = 5,height = 7, units = 'in', res = 200)
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


wideform <- read.csv("bottom_33m_wideform_vali.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Bottom Temperature", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)




wideform <- read.csv("schmidt_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Schmidt Stability", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)



wideform <- read.csv("thermodepth_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Thermocline Depth", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)




wideform <- read.csv("totstratdur_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Summer Stratification Duration", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)




wideform <- read.csv("toticedur_vali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "Ice Off", col = 1, ref.sd = TRUE, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 2)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 3)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 4)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 5)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 6)




dev.off()


