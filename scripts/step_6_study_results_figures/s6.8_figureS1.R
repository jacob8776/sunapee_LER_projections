# combine manual and high frequency buoy data

library(tidyverse)
library(lubridate)
library(here)

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

setwd(paste0(here::here(), "/LER_inputs/"))



sim_folder <- getwd()

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


# download manual data from zenodo: https://zenodo.org/record/4652076#.YKKBbqhKg2x
manual <- read.csv(paste0(sim_folder, "/LSPALMP_1986-2020_v2021-03-29.csv"))
manual <- manual %>% 
  filter(parameter == 'temp_C') %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, depth_m, parameter, value, station) %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  unchop(everything()) # do this bc of strange formating with pivot wider

#ggplot(manual, aes(x = date, y = temp_C)) +
#  geom_point() +
#  facet_wrap(~station)

manual <- manual %>% 
  filter(station == 210) %>%  # this is the deep hole site
  mutate(time = hms("12:00:00")) %>% 
  mutate(DateTime = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC+5') + 60*60*16) %>% 
  dplyr::select(DateTime, depth_m, temp_C, station) %>% 
  mutate(method = 'manual')
colnames(manual) <- c('DateTime', 'Depth', 'Temp_manual', 'site', 'method')  
manual$site <- as.character(manual$site)
manual$DateDepth <- paste0(manual$DateTime, " ", manual$Depth)


buoy <- read.csv(paste0(sim_folder, '/field_temp_noon_obs.csv'))
buoy$DateTime <- as.POSIXct(buoy$DateTime)
buoy$site <- as.character('210') # set up buoy site to 210?
buoy$method <- 'buoy'
colnames(buoy) <- c('DateTime', 'Depth', 'Temp_buoy', 'site', 'method')
buoy <- na.omit(buoy)
buoy$DateDepth <- paste0(buoy$DateTime, " ", buoy$Depth)

ggplot(buoy, aes(x = DateTime, y = Temp_buoy)) +
  geom_line()

ggplot(manual, aes(x = DateTime, y = Temp_manual)) +
  geom_line()

manual$Depth <- round(manual$Depth)

manual$Depth <- round_any(manual$Depth, 1)
unique(manual$Depth)

str(manual)

buoy$Depth <- round_any(buoy$Depth, 1)
unique(buoy$Depth)



# remove days from buoy dataset where there is manual data
#remove <- manual$DateTime

#buoy <- buoy[!buoy$DateTime %in% remove,  ]

# combine the two datasets

manual$DateTime <- as.Date(manual$DateTime)
buoy$DateTime <- as.Date(buoy$DateTime)

str(manual)
str(buoy)

manual <- as.data.frame(manual)
buoy <- as.data.frame(buoy)

str(manual)
str(buoy)

temp_data <- full_join(manual, buoy, by = c("DateTime", "Depth"))

overlap <- filter(temp_data, is.na(Temp_manual) == F & is.na(Temp_buoy) == F)

overlap <- select(overlap, DateTime, Depth, Temp_manual, Temp_buoy)

#temp_data <- merge(manual, buoy)

str(manual)
str(buoy)


#data_nodups <- temp_data[!duplicated(temp_data[,1:2]),]
#
#table(duplicated(data_nodups[,1:2]))
#table(duplicated(temp_data[,1:2]))
#
#test <- filter(data_nodups, is.na(Temp_buoy) == FALSE)
#test <- filter(test, is.na(Temp_manual) == FALSE)


model <- lm(overlap$Temp_buoy~overlap$Temp_manual)

summary(model)



overlap <- ggplot(overlap, aes(y = Temp_buoy, x = Temp_manual))+ 
  geom_point()+
  geom_smooth(method="lm", col = "blue") + 
#  stat_regline_equation(label.x = 12, label.y = 24)+
  stat_regline_equation(label.x = 12, label.y = 24, aes(label = ..rr.label..)) + mytheme +
  ylab("Buoy temperature (ºC)") +
  xlab("Manual temperature (ºC)")

ggsave('../figures/figureS1.png', dpi = 300,width = 647,height = 434, units = 'mm') 

