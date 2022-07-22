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


mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

lake_directory <- here::here()
anomalies_master <- read.csv(file.path(lake_directory, "anomaly_calculations/multiple_annual_anomalies.csv"))
anomalies_master_bot <- read.csv(file.path(lake_directory, "anomaly_calculations/bot_anomalies.csv"))
anomalies_master_sur <- read.csv(file.path(lake_directory, "anomaly_calculations/surf_anomalies.csv"))


anomalies_master$Scenario <- gsub("rcp26", "RCP 2.6", anomalies_master$rcp)
anomalies_master$Scenario <- gsub("rcp60", "RCP 6.0", anomalies_master$Scenario)
anomalies_master$Scenario <- gsub("rcp85", "RCP 8.5", anomalies_master$Scenario)

unique(anomalies_master$Scenario)

######## TsMax ###########

## RCP26

tsmax26 <- filter(anomalies_master_sur, rcp == "rcp26")
tsmax26

tsmax26 <- select(tsmax26, rcp, year, anom, sd_model, sd_gcm)


tsmax26 <- tsmax26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tsmax60 <- filter(anomalies_master_sur, rcp == "rcp60")
tsmax60

tsmax60 <- select(tsmax60, rcp, year, anom, sd_model, sd_gcm)


tsmax60 <- tsmax60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tsmax85 <- filter(anomalies_master_sur, rcp == "rcp85")
tsmax85

tsmax85 <- select(tsmax85, rcp, year, anom, sd_model, sd_gcm)


tsmax85 <- tsmax85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(tsmax26, tsmax60, tsmax85)

tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)

 
stempplot <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Degrees C)") +
  ylim(-2, 7) + 
  mytheme + 
  ggtitle("Mean Summer Surface Temperature") +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")


# ggsave('../../../sundox/plots/mean_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')


################# TbMax ########################


## RCP26

tbmax26 <- filter(anomalies_master_bot, rcp == "rcp26")
tbmax26

tbmax26 <- select(tbmax26, rcp, year, anom, sd_model, sd_gcm)


tbmax26 <- tbmax26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tbmax60 <- filter(anomalies_master_bot, rcp == "rcp60")
tbmax60

tbmax60 <- select(tbmax60, rcp, year, anom, sd_model, sd_gcm)


tbmax60 <- tbmax60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tbmax85 <- filter(anomalies_master_bot, rcp == "rcp85")
tbmax85

tbmax85 <- select(tbmax85, rcp, year, anom, sd_model, sd_gcm)


tbmax85 <- tbmax85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(tbmax26, tbmax60, tbmax85)

tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)

btempplot <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Degrees C)") +
  ylim(-1, 4.5) + 
  mytheme + 
  ggtitle("Mean Summer Bottom Temperature") +
  geom_line(y = 0, col = "black") +
  geom_vline(xintercept = 2006, linetype = "dashed")



# ggsave('../../../sundox/plots/mean_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')


################# totstratdur ##################

## RCP26

totstrat26 <- filter(anomalies_master, variable == "TotStratDur", rcp == "rcp26")
totstrat26

totstrat26 <- select(totstrat26, rcp, year, anom, sd_model, sd_gcm)


totstrat26 <- totstrat26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

totstrat60 <- filter(anomalies_master, variable == "TotStratDur", rcp == "rcp60")
totstrat60

totstrat60 <- select(totstrat60, rcp, year, anom, sd_model, sd_gcm)


totstrat60 <- totstrat60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

totstrat85 <- filter(anomalies_master, variable == "TotStratDur", rcp == "rcp85")
totstrat85

totstrat85 <- select(totstrat85, rcp, year, anom, sd_model, sd_gcm)


totstrat85 <- totstrat85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(totstrat26, totstrat60, totstrat85)

tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)


strat_plot <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Days)") +
  mytheme + 
  ylim(-30, 70) + 
  ggtitle("Summer Stratification Duration")  +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")



# ggsave('../../../sundox/plots/mean_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')


######## Schmidt Stability ###########

anomalies_master <- read.csv(file.path(lake_directory, "/anomaly_calculations/schmidt_annual_anomalies.csv"))


## RCP26

schmidt26 <- filter(anomalies_master, variable == "schmidt.stability", rcp == "rcp26")
schmidt26

schmidt26 <- select(schmidt26, rcp, year, anom, sd_model, sd_gcm)


schmidt26 <- schmidt26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

schmidt60 <- filter(anomalies_master, variable == "schmidt.stability", rcp == "rcp60")
schmidt60

schmidt60 <- select(schmidt60, rcp, year, anom, sd_model, sd_gcm)


schmidt60 <- schmidt60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

schmidt85 <- filter(anomalies_master, variable == "schmidt.stability", rcp == "rcp85")
schmidt85

schmidt85 <- select(schmidt85, rcp, year, anom, sd_model, sd_gcm)


schmidt85 <- schmidt85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(schmidt26, schmidt60, schmidt85)

tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)

schmidt_plot <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (J/m2)") +
  mytheme + 
  ylim(-45, 230) + 
  ggtitle("Schmidt Stability") + 
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")


# ggsave('../../../sundox/plots/mean_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')


######## Thermocline Depth ###########

anomalies_master <- read.csv(file.path(lake_directory, "/anomaly_calculations/thermodepth_annual_anomalies.csv"))


## RCP26

schmidt26 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp26")
schmidt26

schmidt26 <- select(schmidt26, rcp, year, anom, sd_model, sd_gcm)


schmidt26 <- schmidt26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

schmidt60 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp60")
schmidt60

schmidt60 <- select(schmidt60, rcp, year, anom, sd_model, sd_gcm)


schmidt60 <- schmidt60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

schmidt85 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp85")
schmidt85

schmidt85 <- select(schmidt85, rcp, year, anom, sd_model, sd_gcm)


schmidt85 <- schmidt85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(schmidt26, schmidt60, schmidt85)

tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)

thermo_plot <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (m)") +
  mytheme + 
  # ylim(-45, 230) + 
  ggtitle("Thermocline Depth") + 
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")


anomalies_master <- read.csv(file.path(lake_directory, "/anomaly_calculations/multiple_annual_anomalies.csv"))


######## Ice ###########

## RCP26

tsmax26 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp26")
tsmax26

tsmax26 <- select(tsmax26, rcp, year, anom, sd_model, sd_gcm)


tsmax26 <- tsmax26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tsmax60 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp60")
tsmax60

tsmax60 <- select(tsmax60, rcp, year, anom, sd_model, sd_gcm)


tsmax60 <- tsmax60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tsmax85 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp85")
tsmax85

tsmax85 <- select(tsmax85, rcp, year, anom, sd_model, sd_gcm)


tsmax85 <- tsmax85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(tsmax26, tsmax60, tsmax85)
tsmax$Scenario <- gsub("rcp26", "RCP 2.6", tsmax$rcp)
tsmax$Scenario <- gsub("rcp60", "RCP 6.0", tsmax$Scenario)
tsmax$Scenario <- gsub("rcp85", "RCP 8.5", tsmax$Scenario)


icedur <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Days)") +
  ylim(-100, 35) + 
  mytheme + 
  ggtitle("Total Ice Duration") +
  geom_line(y = 0, col = "black") +
  geom_vline(xintercept = 2006, linetype = "dashed")



# ggsave('../../../sundox/plots/mean_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')


### Mixing period


## RCP26

tsmax26 <- filter(anomalies_master, variable == "MixPer", rcp == "rcp26")
tsmax26

tsmax26 <- select(tsmax26, rcp, year, anom, sd_model, sd_gcm)

tsmax26 <- tsmax26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tsmax60 <- filter(anomalies_master, variable == "MixPer", rcp == "rcp60")
tsmax60

tsmax60 <- select(tsmax60, rcp, year, anom, sd_model, sd_gcm)


tsmax60 <- tsmax60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tsmax85 <- filter(anomalies_master, variable == "MixPer", rcp == "rcp85")
tsmax85

tsmax85 <- select(tsmax85, rcp, year, anom, sd_model, sd_gcm)


tsmax85 <- tsmax85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmax <- rbind(tsmax26, tsmax60, tsmax85)



mixper  <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = rcp)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = rcp), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Days)") +
  ylim(-50, 35) + 
  mytheme + 
  ggtitle("Mixing Period") +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")



ggarrange(stempplot, btempplot, schmidt_plot, thermo_plot, strat_plot, icedur, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave(paste0(lake_directory, '/figures/figure5.png'), dpi = 300,width = 384,height = 280, units = 'mm')



