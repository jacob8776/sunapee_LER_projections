library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(Metrics)
library(plotrix)


ncdf <- "~/Dropbox/sunapee_LER_projections/LER_calibration/output/ensemble_output_all_models_31Aug21.nc"
out <- load_var(ncdf = ncdf, var = "temp")

df <- melt(out, id.vars = 1)
colnames(df)[4] <- "model"
df$yday <- yday(df$datetime)
df$year <- year(df$datetime)


df <- filter(df, variable == "wtr_1")

wideform <- dcast(df, datetime~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean


write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/surface_1m_wideform.csv", row.names = FALSE)



df <- melt(out, id.vars = 1)
colnames(df)[4] <- "model"
df$yday <- yday(df$datetime)
df$year <- year(df$datetime)


df <- filter(df, variable == "wtr_30", model != "FLake")

wideform <- dcast(df, datetime~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE  & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
wideformmean <- (wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/4

wideform$mean <- wideformmean


write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/bottom_33m_wideform.csv", row.names = FALSE)


