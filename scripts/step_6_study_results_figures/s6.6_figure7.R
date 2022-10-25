# install.packages("zoo")
# install.packages("roll")


library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(Metrics)
library(zoo) # moving averages        
library(tidyverse) # all tidyverse packages
library(dplyr)
library(here)

setwd(here::here())


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
                 axis.text.x=element_text(size=14, colour='black'), axis.text.y=element_text(size=14, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(20), strip.text.y = element_text(size=20),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=25),
                 legend.title = element_text(size = 25), 
                 plot.title = element_text(size=25))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind

### mean surface temperature

anomalies_master <- read.csv("./anomaly_calculations/multiple_annual_anomalies.csv")
anomalies_master_bot <- read.csv(file.path(lake_directory, "anomaly_calculations/bot_anomalies.csv"))
anomalies_master_sur <- read.csv(file.path(lake_directory, "anomaly_calculations/surf_anomalies.csv"))


anomalies_master <- filter(anomalies_master_sur, rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)
  


rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))

mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")


# mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
#                  axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
#                  axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
#                  axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
#                  strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
#                  panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
#                  legend.title = element_text(size = 20))
# scale_colour_discrete <- ggthemes::scale_colour_colorblind
# scale_fill_discrete <- ggthemes::scale_fill_colorblind





tsmean_plot <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme + 
  ggtitle("Summer Surface Temperature Mean")+   
  ylab("Proportional Variance") + 
  xlab("Year")

?geom_area()



# mean bottom temperature

anomalies_master <- filter(anomalies_master_bot, rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)



rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))

mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")





btemp_plot <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme + 
  ggtitle("Summer Bottom Temperature Mean")+   
  ylab("Proportional Variance") + 
  xlab("Year")

?geom_area()


# total ice duration

anomalies_master <- read.csv("./anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TotStratDur", rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)



rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))


mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")



strat_plot <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme + 
  ggtitle("Total Stratification Duration")+    
  ylab("Proportional Variance") + 
  xlab("Year")

?geom_area()

# total ice duration

anomalies_master <- read.csv("./anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TotIceDur", rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)



rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))


mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")



prop_ice <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme + 
  ggtitle("Total Ice Duration")+  
  ylab("Proportional Variance") + 
  xlab("Year")

  


# Schmidt stability


anomalies_master <- read.csv("./anomaly_calculations/schmidt_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "schmidt.stability", rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)



rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))


mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")



schmidt_plot <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme+
  ggtitle("Schmidt Stability")+  
  ylab("Proportional Variance") + 
  xlab("Year")





# Thermocline depth


anomalies_master <- read.csv("./anomaly_calculations/thermodepth_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "thermo.depth", rcp == "rcp85")

anomalies_master <- select(anomalies_master, year, rcp, gcm, model, variable, value, mean, anom)




rvar_gcm <- anomalies_master %>% 
  dplyr::group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -model) %>% 
  unique() %>% 
  pivot_wider(., names_from = gcm, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_gcm = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))



rvar_model <- anomalies_master %>% 
  dplyr::group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>% 
  dplyr::select(-value, -mean, -anom, -gcm) %>% 
  unique() %>% 
  pivot_wider(., names_from = model, values_from = mean_model) %>% 
  ungroup() %>% 
  dplyr::mutate(var_model = apply(.[4:7], 1, function(x) var(x, na.rm = TRUE))) %>% 
  dplyr::mutate(rvar_model = zoo::rollmean(var_model, 30, na.pad = TRUE, align = "right"))


rvar_gcm <- select(rvar_gcm, year, rvar_gcm)
rvar_model <- select(rvar_model, year, rvar_model)



rvar_df <- merge(rvar_gcm, rvar_model, by = "year")


rvar_df$tvar <- rvar_df$rvar_model + rvar_df$rvar_gcm

rvar_df$pvar_model <- rvar_df$rvar_model/rvar_df$tvar
rvar_df$pvar_gcm <- rvar_df$rvar_gcm/rvar_df$tvar

library(reshape2)

mlt <- pivot_longer(rvar_df, 
                    cols = starts_with("pvar"))

mlt$name <- str_replace(mlt$name, "pvar_model", "Lake Model")
mlt$name <- str_replace(mlt$name, "pvar_gcm", "Climate Model")

colnames(mlt) <- c("year", "rvar_gcm", "rvar_model", "tvar", "Uncertainty", "value")




thermodepth_plot <- ggplot(data = mlt) + 
  geom_area(aes(x = year, y = value, fill = Uncertainty), position = "stack") + 
  mytheme + 
  ggtitle("Thermocline Depth")+   
  ylab("Proportional Variance") + 
  xlab("Year")

?geom_area()




ggarrange(tsmean_plot, btemp_plot, schmidt_plot, thermodepth_plot, strat_plot, prop_ice, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

ggsave('./figures/figure7.png', dpi = 300,width = 384,height = 280, units = 'mm')






