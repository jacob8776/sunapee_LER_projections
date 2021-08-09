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

setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")

ncdf <- 'GFDL-ESM2M_historical_output.nc'


wtemp <- load_var(ncdf = ncdf, var = 'temp', return = 'list')


#glm <- as.data.frame(wtemp[[1]])




out <- load_var(ncdf = ncdf, var = "temp")
#out <- as.data.frame(out[[1]])
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
## plot results
ggplot(df, aes(yday, value, colour = model)) +
  facet_wrap(~year) +
  geom_line() +
  labs(y = "Schmidt stability (J/m2)") +
  theme_classic() + ylim(-50, 1000)

# ts.td <- lapply(out, function(x) {
#   ts.thermo.depth(out, Smin = 0.1, na.rm = TRUE, mixed.cutoff = 4)
# })
# 
# 
# df <- melt(ts.td, id.vars = 1)
# colnames(df)[4] <- "model"



ts.td <- lapply(out, function(x) {
  ts.thermo.depth(x, Smin = 0.1, na.rm = TRUE)
})

df <- melt(ts.td, id.vars = 1)
colnames(df)[4] <- "model"
df$yday <- yday(df$datetime)
df$year <- year(df$datetime)
ggplot(df, aes(yday, value, colour = model)) +
  facet_wrap(~year) + 
  geom_line() +
  labs(y = "Thermocline depth (m)") +
  scale_y_continuous(trans = "reverse") +
  theme_classic()

ggplot(df, aes(datetime, value, colour = model)) +
  geom_line() +
  labs(y = "Thermocline depth (m)") +
  scale_y_continuous(trans = "reverse") +
  theme_classic() 


colnames(mantemp) <- c("datetime", "depths", "wtr")

# setwd("~/Dropbox/sunapee_LER_projections/LER_inputs/")
# mantemp <- read.csv("manual_buoy_temptst.csv")
# thermo <- ts.thermo.depth(mantemp, Smin = 0.1, na.rm = TRUE, seasonal = FALSE)

# analyse_strat(data = wtemp)




# Calculating stratification & ice metrics
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
out

df <- melt(out[1:5], id.vars = 1)
colnames(df)[4] <- "model"
ggplot(df, aes(x = year, y = value, col = model)) + geom_line() + 
  facet_wrap(~variable, scales = "free_y")


?analyse_strat
