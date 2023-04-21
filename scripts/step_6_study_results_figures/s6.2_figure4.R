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
png("../../figures/figure4_newcolors.png", width = 5,height = 7, units = 'in', res = 200)
par(mfrow=c(3,2))
wideform <- read.csv("surface_1m_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)

# red simstrat 
# green glm 
# bluegreen flake 
# yellow mylake 
# black gotm

cols <- RColorBrewer::brewer.pal(5, "Dark2")

par(mar=c(5, 4, 4, 8), xpd=TRUE)
taylor.diagram(wideform$Obs,wideform$Simstrat,  main = "A) Surface Temperature", ref.sd = TRUE, col = cols[1], xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)", pch = 15)
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')

taylor.diagram(wideform$Obs,wideform$Simstrat)
 legend("topright", inset = c(-0.06, -0.1),
        legend = c("Simstrat", "GLM", "FLake", "MyLake",
                   "GOTM", "Ensemble Mean"),
        col = c(cols, 'black'),
        pch = c(15, 16, 17, 18, 8, 16))



wideform <- read.csv("bottom_33m_wideform.csv")
#wideform$datetime <- as.Date(wideform$datetime)
#str(wideform)
#wideform$month <- month(wideform$datetime)
#wideform <- filter(wideform, month >= 6 & month <= 8)

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "B) Bottom Temperature", ref.sd = TRUE, col = cols[1], pch = 15, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
#taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')


wideform <- read.csv("schmidt_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "C) Schmidt Stability", ref.sd = TRUE, col = cols[1], pch = 15, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')


wideform <- read.csv("thermodepth_cali_wideform.csv")
wideform$datetime <- as.Date(wideform$datetime)
str(wideform)
wideform$month <- month(wideform$datetime)
wideform <- filter(wideform, month >= 6 & month <= 8)

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "D) Summer Thermocline Depth", ref.sd = TRUE, col = cols[1], pch = 15, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')



wideform <- read.csv("totstratdur_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "E) Summer Stratification Duration", ref.sd = TRUE, col = cols[1], pch = 15, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')


wideform <- read.csv("toticedur_cali_wideform.csv")

taylor.diagram(wideform$Obs, wideform$Simstrat, main = "F) Ice Off", ref.sd = TRUE, col = cols[1], pch = 15, xlab = "Standard Deviation (Obs)", ylab = "Standard Deviation (Model)")
#taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = cols[2], pch = 16)
taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = cols[3], pch = 17)
taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = cols[4], pch = 18)
taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = cols[5], pch = 8)
taylor.diagram(wideform$Obs, wideform$mean, add = TRUE, col = 'black')


dev.off()


