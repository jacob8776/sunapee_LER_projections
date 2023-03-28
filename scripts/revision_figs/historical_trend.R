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
anomalies_master_bot <- read.csv(file.path(lake_directory, "anomaly_calculations/bot_anomalies.csv"))
anomalies_master_sur <- read.csv(file.path(lake_directory, "anomaly_calculations/surf_anomalies.csv"))



######## mean surface temp (1.0m) ###########
## choose any RCP, historical period is the same for all 

tsmean <- filter(anomalies_master_sur, rcp == "rcp26")
tsmean

tsmean <- select(tsmean, rcp, year, anom, sd_model, sd_gcm)


tsmean <- tsmean %>% 
  group_by(year) %>% 
  dplyr::mutate(mean_mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_sd_model = sd(anom, na.rm = TRUE)) 



tsmean$Scenario <- gsub("historical", "Historical", tsmean$rcp)
tsmean$Scenario <- gsub("rcp26", "RCP 2.6", tsmean$rcp)
tsmean$Scenario <- gsub("rcp60", "RCP 6.0", tsmean$Scenario)
tsmean$Scenario <- gsub("rcp85", "RCP 8.5", tsmean$Scenario)

tsmean_hist <- tsmean %>% 
  filter(year < 2006)

stempplot <- ggplot(subset(tsmean_hist), aes(year, mean_mean_model)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_mean_model-sd_sd_model, ymax=mean_mean_model+sd_sd_model, fill = Scenario), alpha = 0.2,
              linetype = .1)+
  labs(y = "Anomaly (ºC)") +
  mytheme + 
  geom_smooth(method = "lm") +
  ggtitle("Mean Surface Temperature") +
  geom_line(y = 0, col = "black") 
stempplot

lm(tsmean_hist$mean_mean_model~tsmean_hist$year)
lm_hist <- lm(tsmean_hist$mean_mean_model~tsmean_hist$year)
lm_hist$coefficients[2]*10

