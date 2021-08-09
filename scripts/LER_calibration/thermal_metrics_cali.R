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

ncdf <- "~/Dropbox/sunapee_LER_projections/LER_calibration/output/ensemble_output_all_models.nc"


######################## Calculating Schmidt Stability ################################


out <- load_var(ncdf = ncdf, var = "temp")
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
  theme_classic() + ylim(-50, 1000)


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
