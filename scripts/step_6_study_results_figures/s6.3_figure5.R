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


# mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
#                  axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'),
#                  axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
#                  strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
#                  panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
#                  legend.title = element_text(size = 20))
# scale_colour_discrete <- ggthemes::scale_colour_colorblind
# scale_fill_discrete <- ggthemes::scale_fill_colorblind



mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=22, colour='black'), axis.text.y=element_text(size=22, colour='black'), 
                 axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
                 strip.text.x = element_text(20), strip.text.y = element_text(size=20),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=25),
                 legend.title = element_text(size = 25), 
                 plot.title = element_text(size=25))
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

######## tsmean ###########

tsmeanhist <- filter(anomalies_master_sur, rcp == "historical")
tsmeanhist

tsmeanhist <- select(tsmeanhist, rcp, year, anom, sd_model, sd_gcm)


tsmeanhist <- tsmeanhist %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP26

tsmean26 <- filter(anomalies_master_sur, rcp == "rcp26")
tsmean26

tsmean26 <- select(tsmean26, rcp, year, anom, sd_model, sd_gcm)


tsmean26 <- tsmean26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tsmean60 <- filter(anomalies_master_sur, rcp == "rcp60")
tsmean60

tsmean60 <- select(tsmean60, rcp, year, anom, sd_model, sd_gcm)


tsmean60 <- tsmean60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tsmean85 <- filter(anomalies_master_sur, rcp == "rcp85")
tsmean85

tsmean85 <- select(tsmean85, rcp, year, anom, sd_model, sd_gcm)


tsmean85 <- tsmean85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tsmean <- rbind(tsmeanhist, tsmean26, tsmean60, tsmean85)

tsmean$Scenario <- gsub("historical", "Historical", tsmean$rcp)
tsmean$Scenario <- gsub("rcp26", "RCP 2.6", tsmean$rcp)
tsmean$Scenario <- gsub("rcp60", "RCP 6.0", tsmean$Scenario)
tsmean$Scenario <- gsub("rcp85", "RCP 8.5", tsmean$Scenario)


 
stempplot <- ggplot(subset(tsmean), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (ºC)") +
  ylim(-2, 8) + 
  mytheme + 
  ggtitle("Mean Surface Temperature") +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")

stempplot
# ggsave('../../../sundox/plots/mean_tsmean.png', dpi = 300,width = 384,height = 280, units = 'mm')


################# tbmean ########################


## RCP26

tbmean26 <- filter(anomalies_master_bot, rcp == "rcp26")
tbmean26

tbmean26 <- select(tbmean26, rcp, year, anom, sd_model, sd_gcm)


tbmean26 <- tbmean26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

tbmean60 <- filter(anomalies_master_bot, rcp == "rcp60")
tbmean60

tbmean60 <- select(tbmean60, rcp, year, anom, sd_model, sd_gcm)


tbmean60 <- tbmean60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

tbmean85 <- filter(anomalies_master_bot, rcp == "rcp85")
tbmean85

tbmean85 <- select(tbmean85, rcp, year, anom, sd_model, sd_gcm)


tbmean85 <- tbmean85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


tbmean <- rbind(tbmean26, tbmean60, tbmean85)

tbmean$Scenario <- gsub("rcp26", "RCP 2.6", tbmean$rcp)
tbmean$Scenario <- gsub("rcp60", "RCP 6.0", tbmean$Scenario)
tbmean$Scenario <- gsub("rcp85", "RCP 8.5", tbmean$Scenario)

btempplot <- ggplot(subset(tbmean), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (ºC)") +
  ylim(-1.5, 4.5) + 
  mytheme + 
  ggtitle("Mean Bottom Temperature") +
  geom_line(y = 0, col = "black") +
  geom_vline(xintercept = 2006, linetype = "dashed")

btempplot

# ggsave('../../../sundox/plots/mean_tsmean.png', dpi = 300,width = 384,height = 280, units = 'mm')


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


tsmean <- rbind(totstrat26, totstrat60, totstrat85)

tsmean$Scenario <- gsub("rcp26", "RCP 2.6", tsmean$rcp)
tsmean$Scenario <- gsub("rcp60", "RCP 6.0", tsmean$Scenario)
tsmean$Scenario <- gsub("rcp85", "RCP 8.5", tsmean$Scenario)


strat_plot <- ggplot(subset(tsmean), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Days)") +
  mytheme + 
  ylim(-30, 70) + 
  ggtitle("Summer Stratification Duration")  +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")

strat_plot


# ggsave('../../../sundox/plots/mean_tsmean.png', dpi = 300,width = 384,height = 280, units = 'mm')


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


tsmean <- rbind(schmidt26, schmidt60, schmidt85)

tsmean$Scenario <- gsub("rcp26", "RCP 2.6", tsmean$rcp)
tsmean$Scenario <- gsub("rcp60", "RCP 6.0", tsmean$Scenario)
tsmean$Scenario <- gsub("rcp85", "RCP 8.5", tsmean$Scenario)

y_expression <- expression(Anomaly~(J/m^2))

schmidt_plot <- ggplot(subset(tsmean), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = y_expression) +
  mytheme + 
  ylim(-45, 230) + 
  ggtitle("Schmidt Stability") + 
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")

schmidt_plot
# ggsave('../../../sundox/plots/mean_tsmean.png', dpi = 300,width = 384,height = 280, units = 'mm')


######## Thermocline Depth ###########

anomalies_master <- read.csv(file.path(lake_directory, "/anomaly_calculations/thermodepth_annual_anomalies_summer.csv"))


## RCP26

thermo26 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp26")


thermo26 <- select(thermo26, rcp, year, anom, sd_model, sd_gcm)


thermo26 <- thermo26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

thermo60 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp60")
thermo60

thermo60 <- select(thermo60, rcp, year, anom, sd_model, sd_gcm)


thermo60 <- thermo60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

thermo85 <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp85")
thermo85

thermo85 <- select(thermo85, rcp, year, anom, sd_model, sd_gcm)


thermo85 <- thermo85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


thermo <- rbind(thermo26, thermo60, thermo85)

thermo$Scenario <- gsub("rcp26", "RCP 2.6", thermo$rcp)
thermo$Scenario <- gsub("rcp60", "RCP 6.0", thermo$Scenario)
thermo$Scenario <- gsub("rcp85", "RCP 8.5", thermo$Scenario)

thermo_plot <- ggplot(subset(thermo), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (m)") +
  mytheme + 
  # ylim(-45, 230) + 
  ggtitle("Thermocline Depth") + 
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")

thermo_plot


######## Ice ###########
anomalies_master <- read.csv(file.path(lake_directory, "/anomaly_calculations/multiple_annual_anomalies.csv"))

## RCP26

ice26 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp26")
ice26

ice26 <- select(ice26, rcp, year, anom, sd_model, sd_gcm)


ice26 <- ice26 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


## RCP 60 

ice60 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp60")
ice60

ice60 <- select(ice60, rcp, year, anom, sd_model, sd_gcm)


ice60 <- ice60 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 

## RCP 85 

ice85 <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp85")
ice85

ice85 <- select(ice85, rcp, year, anom, sd_model, sd_gcm)


ice85 <- ice85 %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 


ice <- rbind(ice26, ice60, ice85)
ice$Scenario <- gsub("rcp26", "RCP 2.6", ice$rcp)
ice$Scenario <- gsub("rcp60", "RCP 6.0", ice$Scenario)
ice$Scenario <- gsub("rcp85", "RCP 8.5", ice$Scenario)


icedur <- ggplot(subset(ice), aes(year, mean_mean_model, colour = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (Days)") +
  ylim(-100, 35) + 
  mytheme + 
  ggtitle("Total Ice Duration") +
  geom_line(y = 0, col = "black") +
  geom_vline(xintercept = 2006, linetype = "dashed")

icedur

# ggsave('../../../sundox/plots/mean_tsmean.png', dpi = 300,width = 384,height = 280, units = 'mm')



## combined plot
ggarrange(stempplot, btempplot, schmidt_plot, thermo_plot, strat_plot, icedur, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave(paste0(lake_directory, '/figures/figure5.png'), dpi = 300,width = 384,height = 280, units = 'mm')

ggarrange(stempplot, icedur, 
          labels = c("A", "B"), 
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave(paste0(lake_directory, '/figures/testfig.png'), dpi = 300,width = 384,height = 93, units = 'mm')


