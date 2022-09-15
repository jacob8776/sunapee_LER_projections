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
library(here)
library(scales)

setwd(here::here())

ncdf <- "./LER_validation/output/ensemble_output.nc"

out <- load_var(ncdf = ncdf, var = "temp")

 temp <- load_var(ncdf, "temp")
ice <- load_var(ncdf, "ice_height")
out <- lapply(1:length(temp), function(x) {
  # x = 1 # for debugging
  mlt <- reshape::melt(temp[[x]], id.vars = 1)
  mlt[, 2] <- as.numeric(gsub("wtr_", "", mlt[, 2]))
  if(names(out)[x] == "Obs") {
    analyse_strat(data = mlt)
  }
  analyse_strat(data = mlt, H_ice = ice[[x]][, 2])
})
names(out) <- names(temp)


df <- melt(out[1:6], id.vars = 1)
colnames(df)[4] <- "model"


df <- df %>% 
  dplyr :: group_by(year, variable) %>% 
  dplyr :: mutate(mean = mean(value, na.rm = TRUE)) %>% 
  dplyr :: mutate(sd = sd(value, na.rm = TRUE))


df <- filter(df, variable == "IceOff" & model != "Obs" & is.na(value) == FALSE)

iceoff_obs <- read.csv("./LER_analysis/iceoff_dates.csv")
colnames(iceoff_obs) <- c("year", "value")

iceoff_obs$variable <- "IceOff"
iceoff_obs$model <- "Obs"
iceoff_obs$mean <- NA
iceoff_obs$sd <- NA

iceoff_obs <- iceoff_obs %>% 
  select(year, variable, value, model, mean, sd) %>% 
  filter(year >= 2015 & year <= 2020)

df <- rbind(df, iceoff_obs)

wideform <- dcast(df, year~model, value.var = "value")
wideform <- filter(wideform, is.na(Obs) == FALSE &
                     is.na(GOTM) == FALSE & is.na(FLake) == FALSE & 
                     is.na(Simstrat) == FALSE & is.na(MyLake) == FALSE)

wideformmean <- (wideform$FLake +  wideform$GOTM + wideform$MyLake + wideform$Simstrat)/4

wideform$mean <- wideformmean

write.csv(wideform ,"./LER_validation/vali_calcs/toticedur_vali_wideform.csv", row.names = FALSE)



bias <- c()
models <- c("FLake", "GOTM", "Simstrat", "MyLake", "mean")
bias <- c(bias, (bias(wideform$Obs, wideform$FLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$GOTM)))
bias <- c(bias, (bias(wideform$Obs, wideform$Simstrat)))
bias <- c(bias, (bias(wideform$Obs, wideform$MyLake)))
bias <- c(bias, (bias(wideform$Obs, wideform$mean)))


models <- as.data.frame(models)
bias <- as.data.frame(bias)

model_bias <- cbind(models, bias)
model_bias

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

ggplot(df, aes(x = year, y = value, colour = model)) + geom_line() + 
  ggtitle("Ice Off Dates Calibration Period") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  mytheme
# ggsave('~/Dropbox/sundox/plots/iceoffcali.png', dpi = 300,width = 384,height = 280, units = 'mm')

