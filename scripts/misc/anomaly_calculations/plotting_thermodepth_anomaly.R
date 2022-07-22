library(LakeEnsemblR)
library(lubridate)
library(plyr)
library(gotmtools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)
library(ggthemes)


mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


anomalies_rcp26 <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/thermodepth_annual_anomalies.csv_rcp26")

anomalies_rcp60 <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/thermodepth_annual_anomalies_rcp60.csv")

anomalies_rcp85 <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/thermodepth_annual_anomalies_rcp85.csv")

anomalies_master <- rbind(anomalies_rcp26, anomalies_rcp60, anomalies_rcp85)


write.csv(anomalies_master, "~/Dropbox/sunapee_LER_projections/anomaly_calculations/thermodepth_annual_anomalies_all.csv", row.names = F)

ggplot(subset(anomalies_master), aes(year, mean_model, colour = rcp)) +
  facet_wrap(~gcm) +
  geom_line() +
  geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
              linetype = .1)+
  labs(y = "Thermocline Depth (m)") +
  mytheme

ggsave('~/Dropbox/sundox/plots/thermo_depth_anom_year_fgcm_all_mean.png', dpi = 300,width = 384,height = 280, units = 'mm')


ggplot(subset(anomalies_master), aes(year, mean_gcm, colour = rcp)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~model) +
  geom_line() +
  labs(y = "Thermocline Depth (m)") +
  geom_ribbon(data = anomalies_master, aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
              linetype = .1) +
  mytheme

ggsave('~/Dropbox/sundox/plots/thermo_depth_anom_year_fler_all_mean.png', dpi = 300,width = 384,height = 280, units = 'mm')



anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050) 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099)
mean(anom_midcentury$anom, na.rm = TRUE)
mean(anom_endcentury$anom, na.rm = TRUE)



ggplot(subset(anomalies_master), aes(year, var_model)) +
  geom_point(col = "red") + 
  facet_wrap(model~rcp) +
  geom_point(data = anomalies_master, aes(year, var_gcm), col = "blue") +
  labs(y = "Thermocline Depth (m)") +
  mytheme

anom_gcm <- anomalies_master[anomalies_master$gcm == "MIROC5", ]
anom_model <- anomalies_master[anomalies_master$model == "MIROC5", ]

subset(anomalies_master, year == 2020)

ggplot(subset(anomalies_master, gcm == "MIROC5" & model == "GOTM"), aes(year, var_model)) +
  geom_line(col = "red") + 
  facet_wrap(~rcp) +
  geom_line(aes(year, var_gcm), col = "blue") +
  labs(y = "Thermocline Depth (m)") +
  mytheme

# geom_area






anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

mean(anom_midcentury$mean_model, na.rm = TRUE)
max(anom_midcentury$mean_model, na.rm = TRUE)
median(anom_midcentury$mean_model, na.rm = TRUE)
sum(anom_midcentury$var_model)
mean(anom_endcentury$mean_model, na.rm = TRUE)
max(anom_endcentury$mean_model, na.rm = TRUE)
median(anom_endcentury$mean_model, na.rm = TRUE)
sum(anom_endcentury$var_model)



mean(anom_midcentury$mean_gcm, na.rm = TRUE)
max(anom_midcentury$mean_gcm, na.rm = TRUE)
median(anom_midcentury$mean_gcm, na.rm = TRUE)
sum(anom_midcentury$var_gcm)
mean(anom_endcentury$mean_gcm, na.rm = TRUE)
max(anom_endcentury$mean_gcm, na.rm = TRUE)
median(anom_endcentury$mean_gcm, na.rm = TRUE)
sum(anom_endcentury$var_gcm)









