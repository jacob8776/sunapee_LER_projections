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


mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")


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



icedur <- ggplot(subset(tsmax), aes(year, mean_mean_model, colour = rcp)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = rcp), alpha = 0.2,
              linetype = .1)+
  labs(y = "Days") +
  ylim(-100, 20) + 
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
  labs(y = "Days") +
  ylim(-50, 20) + 
  mytheme + 
  ggtitle("Mixing Period") +
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept = 2006, linetype = "dashed")



ggarrange(icedur, mixper, 
          labels = c("A", "B"), 
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave('~/Dropbox/sundox/plots/mean_anoms_winter.png', dpi = 300,width = 384,height = 280, units = 'mm')

