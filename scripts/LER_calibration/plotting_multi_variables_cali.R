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
library(Metrics)

ncdf <- "~/Dropbox/sunapee_LER_projections/LER_calibration/output/ensemble_output_all_models_15Nov21.nc"

out <- load_var(ncdf = ncdf, var = "temp")

temp <- load_var(ncdf, "temp")
# depths <- get.offsets(temp$FLake)
# idx <- which(depths < 11)
# temp <- lapply(temp, function(x) x[, c(1,idx+1)])
# head(temp$FLake)
ice <- load_var(ncdf, "ice_height")
out <- lapply(1:length(temp), function(x) {
  # x = 1 # for debugging
  mlt <- reshape::melt(temp[[x]], id.vars = 1)
  mlt[, 2] <- as.numeric(gsub("wtr_", "", mlt[, 2]))
  if(names(out)[x] == "Obs") {
    analyse_strat(data = mlt)
  }
  analyse_strat(data = mlt, H_ice = ice[[x]][, 2], month = 6:8)
})
names(out) <- names(temp)


df <- melt(out[1:6], id.vars = 1)
colnames(df)[4] <- "model"


dfobs <- filter(df, model == "Obs")
df <- filter(df, model != "Obs")

df <- df %>% 
  dplyr :: group_by(year, variable) %>% 
  dplyr :: mutate(mean = mean(value, na.rm = TRUE)) %>% 
  dplyr :: mutate(sd = sd(value, na.rm = TRUE))


dfobs$mean <- NA
dfobs$sd <- NA

df <- rbind(df, dfobs)

test <- subset(df, variable == c("TsMax","TbMax"))

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

ggplot(subset(df, variable == "TsMax" |  variable == "TbMax"),
       aes(x = year, y = value, colour = model)) + geom_line() + 
  mytheme + 
  facet_wrap(~variable, scales = "free_y") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  ylab("Degrees C")

# ggsave('~/Dropbox/sundox/plots/surface_bottom_temps_cali.png', dpi = 300,width = 384,height = 280, units = 'mm')

ggplot(subset(df, variable == "TotStratDur"), aes(x = year, y = value, colour = model)) + geom_point() + 
  mytheme + 
  scale_x_continuous(breaks = pretty_breaks()) +
  ylab("Days")

# ggsave('~/Dropbox/sundox/plots/totstratdur_cali.png', dpi = 300,width = 384,height = 280, units = 'mm')


ggplot(subset(df, variable == "TotIceDur"), aes(x = year, y = value, colour = model)) + geom_line() + 
  mytheme + 
  scale_x_continuous(breaks = pretty_breaks()) +
  ylab("Days")

# ggsave('~/Dropbox/sundox/plots/toticedur_cali.png', dpi = 300,width = 384,height = 280, units = 'mm')


ggplot(df, aes(x = year, y = value, colour = model)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  mytheme

ggplot(df, aes(x = year, y = mean)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y") + 
  geom_ribbon(data = df, aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6,
              linetype = 0.1,
              color = "grey") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  geom_line(data = subset(df, model == "Obs"), aes(year, value, col = "Obs"))










#### RMSE CALCULATIONS FOR METRICS IN RESULTS #########

#tsmax

wideform <- dcast(subset(df, variable == "TsMean"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/tsmean_cali_wideform.csv", row.names = FALSE)


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
paste0("rmse for tsmean is: ")
model_rmse

# # tsmin
# 
# wideform <- dcast(subset(df, variable == "TsMin"), year~model, value.var = "value")
# wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
#                      is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
#                      is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
# 
# 
# rmse <- c()
# models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM")
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$FLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GOTM)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$Simstrat)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$MyLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GLM)))
# 
# models <- as.data.frame(models)
# rmse <- as.data.frame(rmse)
# 
# model_rmse <- cbind(models, rmse)
# paste0("rmse for tsmin is: ")
# model_rmse


# tbmax

wideform <- dcast(subset(df, variable == "TbMean"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/tbmean_cali_wideform.csv", row.names = FALSE)


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
paste0("rmse for tbmax is: ")
model_rmse


# #tbmin
# 
# wideform <- dcast(subset(df, variable == "TbMin"), year~model, value.var = "value")
# wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
#                      is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
#                      is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
# 
# 
# rmse <- c()
# models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM")
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$FLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GOTM)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$Simstrat)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$MyLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GLM)))
# 
# models <- as.data.frame(models)
# rmse <- as.data.frame(rmse)
# 
# model_rmse <- cbind(models, rmse)
# paste0("rmse for tbmin is: ")
# model_rmse


#total stratification duration

wideform <- dcast(subset(df, variable == "TotStratDur"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/totstratdur_cali_wideform.csv", row.names = FALSE)


bias <- c()
models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM", "mean")
bias <- c(bias, (bias(wideform$Obs, wideform$FLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$GOTM)))
bias <- c(bias, (bias(wideform$Obs, wideform$Simstrat)))
bias <- c(bias, (bias(wideform$Obs, wideform$MyLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$GLM)))
bias <- c(bias, (bias(wideform$Obs, wideform$mean)))


models <- as.data.frame(models)
bias <- as.data.frame(bias)

model_rmse <- cbind(models, bias)
paste0("bias for totstratdur is: ")
model_rmse


wideform <- dcast(subset(df, variable == "TbMean"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/tbmean_cali_wideform.csv", row.names = FALSE)


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
paste0("rmse for tbmax is: ")
model_rmse


# #tbmin
# 
# wideform <- dcast(subset(df, variable == "TbMin"), year~model, value.var = "value")
# wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
#                      is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
#                      is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
# 
# 
# rmse <- c()
# models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM")
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$FLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GOTM)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$Simstrat)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$MyLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GLM)))
# 
# models <- as.data.frame(models)
# rmse <- as.data.frame(rmse)
# 
# model_rmse <- cbind(models, rmse)
# paste0("rmse for tbmin is: ")
# model_rmse


#total stratification duration

wideform <- dcast(subset(df, variable == "TotStratDur"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/totstratdur_cali_wideform.csv", row.names = FALSE)


bias <- c()
models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM", "mean")
bias <- c(bias, (bias(wideform$Obs, wideform$FLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$GOTM)))
bias <- c(bias, (bias(wideform$Obs, wideform$Simstrat)))
bias <- c(bias, (bias(wideform$Obs, wideform$MyLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$GLM)))
bias <- c(bias, (bias(wideform$Obs, wideform$mean)))


models <- as.data.frame(models)
bias <- as.data.frame(bias)

model_rmse <- cbind(models, bias)
paste0("bias for totstratdur is: ")
model_rmse


wideform <- dcast(subset(df, variable == "MixPer"), year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake + wideform$GLM + wideform$GOTM + wideform$MyLake + wideform$Simstrat)/5

wideform$mean <- wideformmean

write.csv(wideform ,"~/Dropbox/sunapee_LER_projections/LER_calibration/cali_calcs/mixper_cali_wideform.csv", row.names = FALSE)


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
paste0("rmse for tbmax is: ")
model_rmse


# #tbmin
# 
# wideform <- dcast(subset(df, variable == "TbMin"), year~model, value.var = "value")
# wideform <- filter(wideform, is.na(Obs) == FALSE & is.na(GLM) == FALSE &
#                      is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
#                      is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)
# 
# 
# rmse <- c()
# models <- c("FLake", "GOTM", "Simstrat", "MyLake", "GLM")
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$FLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GOTM)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$Simstrat)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$MyLake)))
# rmse <- c(rmse, (rmse(wideform$Obs, wideform$GLM)))
# 
# models <- as.data.frame(models)
# rmse <- as.data.frame(rmse)
# 
# model_rmse <- cbind(models, rmse)
# paste0("rmse for tbmin is: ")
# model_rmse


