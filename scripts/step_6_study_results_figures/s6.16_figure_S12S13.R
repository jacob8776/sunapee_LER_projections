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
library(scales)
library(here)

setwd(paste0(here::here(), "/LER_projections/output/"))

anomalies_master <- read.csv("../../anomaly_calculations/multiple_annual_anomalies.csv")
anomalies_master_bot <- read.csv(file.path(lake_directory, "anomaly_calculations/bot_anomalies.csv"))
anomalies_master_sur <- read.csv(file.path(lake_directory, "anomaly_calculations/surf_anomalies.csv"))

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=22, colour='black'), axis.text.y=element_text(size=22, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(20), strip.text.y = element_text(size=20),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=25),
                 legend.title = element_text(size = 25), 
                 plot.title = element_text(size=25))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


choosercp <- "rcp85"


model_tsmean <- ggplot(subset(anomalies_master_sur, rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (ºC)") +
  ggtitle("Summer Mean Surface Temperature") + 
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_tsmean <- ggplot(subset(anomalies_master_sur, rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (ºC)") +
  ggtitle("Summer Mean Surface Temperature") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme



model_tbmean <- ggplot(subset(anomalies_master_bot, rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (ºC)") +
  ggtitle("Summer Mean Bottom Temperature") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_tbmean <- ggplot(subset(anomalies_master_bot, rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (ºC)") +
  ggtitle("Summer Mean Bottom Temperature") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme






model_strat <- ggplot(subset(anomalies_master, variable == "TotStratDur" & rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (Days)") +
  ggtitle("Summer Stratification Duration") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_strat <- ggplot(subset(anomalies_master, variable == "TotStratDur" & rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (Days)") +
  ggtitle("Summer Stratification Duration") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme






model_ice <- ggplot(subset(anomalies_master, variable == "TotIceDur" & rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (Days)") +
  ggtitle("Total Ice Duration") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_ice <- ggplot(subset(anomalies_master, variable == "TotIceDur" & rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (Days)") +
  ggtitle("Total Ice Duration") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme








# model_mix <- ggplot(subset(anomalies_master, variable == "MixPer" & rcp == "rcp85"), aes(year, mean_model, colour = gcm)) +
#   geom_hline(yintercept = 0) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#   #               linetype = .1) +
#   mytheme
# #
# #
# 
# gcm_mix <- ggplot(subset(anomalies_master, variable == "MixPer" & rcp == "rcp85"), aes(year, mean_gcm, colour = model)) +
#   geom_hline(yintercept = 0) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#   #               linetype = .1) +
#   mytheme




anomalies_master <- read.csv("../../anomaly_calculations/schmidt_annual_anomalies.csv")

y_expression <- expression(Anomaly~(J/m^2))

model_schmidt <- ggplot(subset(anomalies_master, variable == "schmidt.stability" & rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = y_expression) +
  ggtitle("Schmidt Stability") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_schmidt <- ggplot(subset(anomalies_master, variable == "schmidt.stability" & rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = y_expression) +
  ggtitle("Schmidt Stability") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme




anomalies_master <- read.csv("../../anomaly_calculations/thermodepth_annual_anomalies.csv")


model_thermo <- ggplot(subset(anomalies_master, variable == "thermo.depth" & rcp == choosercp), aes(year, mean_model, colour = gcm)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (m)") +
  ggtitle("Thermocline Depth") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme
#
#

gcm_thermo <- ggplot(subset(anomalies_master, variable == "thermo.depth" & rcp == choosercp), aes(year, mean_gcm, colour = model)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  labs(y = "Anomaly (m)") +
  ggtitle("Thermocline Depth") + 
  
  #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
  #               linetype = .1) +
  mytheme




ggarrange(model_tsmean, model_tbmean, model_schmidt, model_thermo, model_strat, model_ice, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave('../../figures/figureS13.png', dpi = 300,width = 384,height = 280, units = 'mm')



ggarrange(gcm_tsmean, gcm_tbmean, gcm_schmidt, gcm_thermo, gcm_strat, gcm_ice, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave('../../figures/figureS12.png', dpi = 300,width = 384,height = 280, units = 'mm')


