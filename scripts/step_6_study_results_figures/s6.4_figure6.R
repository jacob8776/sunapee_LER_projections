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

setwd(here::here())

source("./scripts/misc/source_scripts/geom_violin.r")


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

level_order <- c("midcentury", "endcentury")



anomalies <- read.csv("./anomaly_calculations/multiple_annual_anomalies.csv")
anomalies_master_bot <- read.csv(file.path(lake_directory, "anomaly_calculations/bot_anomalies.csv"))
anomalies_master_sur <- read.csv(file.path(lake_directory, "anomaly_calculations/surf_anomalies.csv"))


## total strat duration

anomalies_master <- filter(anomalies, variable == "TotStratDur")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm





#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

stratdur_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Summer Stratification Duration") + 
  labs(y = "Anomaly (Days)", x = "Time Period") + geom_hline(yintercept = 0)

anom_midcentury_8.5$time <- as.character("midcentury")

stratdur_violin_test <- ggplot(data = anom_midcentury_8.5, mapping = aes(x = factor(time, level = level_order), y = anom, fill = model, col = model)) + 
  geom_split_violin(alpha = 0.2) + 
  mytheme + 
  ggtitle("Summer Stratification Duration") + 
  labs(y = "Anomaly (Days)", x = "Time Period") + geom_hline(yintercept = 0)




####################################################################################
####################################################################################
####################################################################################



anomalies_master <- anomalies_master_sur

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

tsmean_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Mean Summer Surface Temperature") + 
  labs(y = "Anomaly (ºC)", x = "Time Period")+ geom_hline(yintercept = 0)

tsmean_violin

sub <- anomalies_master[anomalies_master$rcp == "rcp85", ]

ggplot(sub) +
  geom_line(aes(year, mean_gcm, color = model)) +
  facet_wrap(~gcm)

ggplot(sub) +
  geom_line(aes(year, mean_model, color = gcm)) +
  facet_wrap(~model)


####################################################################################
####################################################################################
####################################################################################



anomalies_master <- anomalies_master_bot

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

tbmean_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Mean Summer Bottom Temperature") + 
  labs(y = "Anomaly (ºC)", x = "Time Period")+ geom_hline(yintercept = 0)

tbmean_violin


####################################################################################
####################################################################################
####################################################################################



anomalies_master <- filter(anomalies, variable == "TotIceDur")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

ice_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Total Ice Duration") + 
  labs(y = "Anomaly (Days)", x = "Time Period")+ geom_hline(yintercept = 0)

tbmean_violin




####################################################################################
####################################################################################
####################################################################################



anomalies_master <- filter(anomalies, variable == "MixPer")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

mix_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Total Mixing Period") + 
  labs(y = "Anomaly (Days)", x = "Time Period")+ geom_hline(yintercept = 0)

mix_violin



####################################################################################
####################################################################################
####################################################################################

anomalies <- read.csv("./anomaly_calculations/schmidt_annual_anomalies.csv")


anomalies_master <- filter(anomalies, variable == "schmidt.stability")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)

y_expression <- expression(Anomaly~(J/m^2))

schmidt_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Schmidt Stability") + 
  labs(y = y_expression, x = "Time Period") + geom_hline(yintercept = 0)

schmidt_violin




####################################################################################
####################################################################################
####################################################################################

anomalies <- read.csv("./anomaly_calculations/thermodepth_annual_anomalies.csv")


anomalies_master <- filter(anomalies, variable == "thermo.depth")

anom_midcentury_2.6 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_midcentury_6.0 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp60") 
anom_midcentury_8.5 <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 

anom_midcentury <- rbind(anom_midcentury_2.6, anom_midcentury_6.0, anom_midcentury_8.5)

anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury_2.6 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
anom_endcentury_6.0 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp60")
anom_endcentury_8.5 <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")

anom_endcentury <- rbind(anom_endcentury_2.6, anom_endcentury_6.0, anom_endcentury_8.5)

# par(mfrow=c(2,2))

# rcp 2.6
anomaly_model_2.6 <- anom_midcentury_2.6$mean_model
anomaly_gcm_2.6 <- anom_midcentury_2.6$mean_gcm


#### rcp 6.0 
anomaly_model_6.0 <- anom_midcentury_6.0$mean_model
anomaly_gcm_6.0 <- anom_midcentury_6.0$mean_gcm



#### rcp 8.5

anomaly_model_8.5 <- as.data.frame(anom_midcentury_8.5$mean_model)
anomaly_gcm_8.5 <- as.data.frame(anom_midcentury_8.5$mean_gcm)

anomaly_model_8.5$uncertainty <- "Climate Model"
anomaly_gcm_8.5$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5) <- c("anomaly", "Distribution")


midcentury_8.5 <- rbind(anomaly_model_8.5, anomaly_gcm_8.5)

#############################################################

anomaly_model_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_model)
anomaly_gcm_8.5_end <- as.data.frame(anom_endcentury_8.5$mean_gcm)

anomaly_model_8.5_end$uncertainty <- "Climate Model"
anomaly_gcm_8.5_end$uncertainty <- "Lake Model"

colnames(anomaly_model_8.5_end) <- c("anomaly", "Distribution")
colnames(anomaly_gcm_8.5_end) <- c("anomaly", "Distribution")


endcentury_8.5 <- rbind(anomaly_model_8.5_end, anomaly_gcm_8.5_end)

midcentury_8.5$time <- "midcentury"
endcentury_8.5$time <- "endcentury"

rcp8.5 <- rbind(midcentury_8.5, endcentury_8.5)


thermo_violin <- ggplot(data = rcp8.5, mapping = aes(x = factor(time, level = level_order), y = anomaly, fill = Distribution, col = Distribution)) + 
  geom_split_violin() + 
  mytheme + 
  ggtitle("Thermocline Depth") + 
  labs(y = "Anomaly (m)", x = "Time Period") + geom_hline(yintercept = 0)

schmidt_violin


ggarrange(tsmean_violin, tbmean_violin, schmidt_violin, thermo_violin, stratdur_violin, ice_violin, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave('./figures/figure6.png', dpi = 300,width = 384,height = 280, units = 'mm')