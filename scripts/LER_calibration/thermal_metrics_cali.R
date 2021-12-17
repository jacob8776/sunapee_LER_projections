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


ncdf <- "~/Dropbox/sunapee_LER_projections/LER_calibration/output/ensemble_output_all_models_15Nov21.nc"
out <- load_var(ncdf = ncdf, var = "temp")


######################## Calculating Schmidt Stability ################################


# out <- as.data.frame(out[[1]])
bathy <- read.csv('~/Dropbox/sunapee_LER_projections/LER_inputs/sunapee_hypso.csv')
colnames(bathy) <- c("depths", "areas")
ts.sch <- lapply(out, function(x) {
  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})


## Reshape to data.frame
df <- melt(ts.sch, id.vars = 1)
colnames(df)[4] <- "model"
df$yday <- yday(df$datetime)
df$year <- year(df$datetime)
df <- filter(df, model != "FLake")

listflake <- list(out[["FLake"]])
bathy <- filter(bathy, depths == 0.0)
ts.sch <- lapply(listflake, function(x) {
  ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
})

dfflake <- melt(ts.sch, id.vars = 1)
colnames(dfflake)[4] <- "model"
dfflake$model <- "FLake"
dfflake$yday <- yday(dfflake$datetime)
dfflake$year <- year(dfflake$datetime)

colnames(df)
colnames(dfflake)

df <- rbind(df, dfflake)



wideform <- dcast(df, datetime~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/schmidt_cali_wideform.csv", row.names = FALSE)

# 
# taylor.diagram(wideform$Obs, wideform$Simstrat)
# taylor.diagram(wideform$Obs, wideform$GLM, add = TRUE, col = 3)
# taylor.diagram(wideform$Obs, wideform$FLake, add = TRUE, col = 5)
# taylor.diagram(wideform$Obs, wideform$MyLake, add = TRUE, col = 7)
# taylor.diagram(wideform$Obs, wideform$GOTM, add = TRUE, col = 9)


rmse <- c()
models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM", "mean")
rmse <- c(rmse, (rmse(wideform$Obs, wideform$FLake)))
rmse <- c(rmse, (rmse(wideform$Obs, wideform$GOTM)))
rmse <- c(rmse, (rmse(wideform$Obs, wideform$Simstrat)))
rmse <- c(rmse, (rmse(wideform$Obs, wideform$MyLake)))
rmse <- c(rmse, (rmse(wideform$Obs, wideform$GLM)))
rmse <- c(rmse, (rmse(wideform$Obs, wideform$mean)))


models <- as.data.frame(models)
rmse <- as.data.frame(rmse)

model_rmse <- cbind(models, rmse)


df <- df %>% 
  dplyr :: group_by(yday, year) %>% 
  dplyr :: mutate(mean = mean(value, na.rm = TRUE)) %>% 
  dplyr :: mutate(sd = sd(value, na.rm = TRUE))





## plot results
ggplot(df, aes(yday, value, colour = model)) +
  # geom_line(data=df, aes(y=mean, x=yday), color = "black", size = 1) +
  geom_line() +
  # geom_ribbon(data = df, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, 
  #             linetype = 0.1, 
  #             color = "grey") +
  facet_wrap(~year) +
  labs(y = "Schmidt stability (J/m2)") +
  theme_classic() + ylim(-50, 1000)

ggplot(df, aes(yday, mean)) + 
  geom_line() + 
  geom_ribbon(data = df, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6,
              linetype = 0.1,
              color = "grey") + 
  facet_wrap(~year) + 
  labs(y = "Schmidt stability (J/m2)") +
  theme_classic() + ylim(-50, 1000) + 
  geom_line(data = subset(df, model == "Obs"), aes(yday, value, col = "Obs"))





#################################### Calculating thermocline depth ######################################


## Same for thermocline depth
ts.td <- lapply(out, function(x) {
  ts.thermo.depth(x, Smin = 0.1, na.rm = TRUE)
})

df <- melt(ts.td, id.vars = 1)
colnames(df)[4] <- "model"
df$yday <- yday(df$datetime)
df$month <- month(df$datetime)
df$year <- year(df$datetime)

wideform <- dcast(df, datetime~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/thermodepth_cali_wideform.csv", row.names = FALSE)

paste0("FLake RMSE: ", rmse(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", rmse(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", rmse(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", rmse(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", rmse(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", rmse(wideform$Obs, wideform$mean))

paste0("FLake RMSE: ", bias(wideform$Obs, wideform$FLake))
paste0("GOTM RMSE: ", bias(wideform$Obs, wideform$GOTM))
paste0("Simstrat RMSE: ", bias(wideform$Obs, wideform$Simstrat))
paste0("MyLake RMSE: ", bias(wideform$Obs, wideform$MyLake))
paste0("GLM RMSE: ", bias(wideform$Obs, wideform$GLM))
paste0("Ensemble RMSE: ", bias(wideform$Obs, wideform$mean))



df <- df %>% 
  dplyr :: group_by(yday, year) %>% 
  dplyr :: mutate(mean = mean(value, na.rm = TRUE)) %>% 
  dplyr :: mutate(sd = sd(value, na.rm = TRUE))



ggplot(subset(df, month >= 6 & month <= 8), aes(yday, value, colour = model)) +
  facet_wrap(~year) +
  geom_line() +
  labs(y = "Thermocline depth (m)") +
  scale_y_continuous(trans = "reverse") +
  theme_classic() 

ggplot(subset(df, month >= 6 & month <= 8 & model != "Obs"), aes(yday, mean)) +
  facet_wrap(~year) +
  geom_line() +
  geom_ribbon(data = subset(df, month >= 6 & month <= 8), aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6,
              linetype = 0.1,
              color = "grey") + 
  labs(y = "Thermocline depth (m)") +
  scale_y_continuous(trans = "reverse") +
  theme_classic() + 
  geom_line(data = subset(df, month >= 6 & month <= 8 & model == "Obs"), aes(yday, value, col = "Obs"))


dfsummer <- filter(df, month >=6 & month <=8)


###################################### Calculating stratification & ice metrics #####################

temp <- load_var(ncdf, "temp")
ice <- load_var(ncdf, "ice_height")
out <- lapply(1:length(temp), function(x) {
  x = 1 # for debugging
  mlt <- reshape::melt(temp[[x]], id.vars = 1)
  mlt[, 2] <- as.numeric(gsub("wtr_", "", mlt[, 2]))
  if(names(temp)[x] == "Obs") {
    analyse_strat(data = mlt)
  }
  analyze_strat(data = mlt, H_ice = ice[[x]][, 2], month = 6:8)
})
names(out) <- names(temp)
out


df <- melt(out[1:6], id.vars = 1)
colnames(df)[4] <- "model"


df <- df %>% 
  dplyr :: group_by(year, variable) %>% 
  dplyr :: mutate(mean = mean(value, na.rm = TRUE)) %>% 
  dplyr :: mutate(sd = sd(value, na.rm = TRUE))



ggplot(df, aes(x = year, y = value, colour = model)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y")

ggplot(df, aes(x = year, y = mean)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y") + 
  geom_ribbon(data = df, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6,
                                                         linetype = 0.1,
                                                         color = "grey") + 
  geom_line(data = subset(df, model == "Obs"), aes(year, value, col = "Obs"))








