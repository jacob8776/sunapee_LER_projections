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

# ggsave('output/ensemble_heatmap.png', p1,  dpi = 300,width = 384,height = 280, units = 'mm')


gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")

# 
# # List of RCP's to run 
# gcm <- c("GFDL-ESM2M")
rcp <- c("rcp26", "rcp60", "rcp85")




setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")


anomalies_master <- data.frame("year" = numeric(0), "month" = numeric(0), "yday" = numeric(0), 
                               "rcp" = character(0), "gcm" = character(0), "model" = character(0), "variable" = character(0),
                               "value" = numeric(0), "mean" = numeric(0), "anom" = numeric(0))


for(i in 1:length(gcm)){
  # Sets working directory to each gcm 
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    print(ncdf)
    
    out <- load_var(ncdf = ncdf, var = "temp")
    #out <- as.data.frame(out[[1]])
    bathy <- read.csv('~/Dropbox/sunapee_LER_projections/LER_inputs/sunapee_hypso.csv')
    colnames(bathy) <- c("depths", "areas")
    ts.td <- lapply(out, function(x) {
      ts.thermo.depth(x, Smin = 0.1, na.rm = TRUE)
    })
    
    df <- melt(ts.td, id.vars = 1)
    colnames(df)[4] <- "model"
    df$yday <- yday(df$datetime)
    df$year <- year(df$datetime)
    df$month <- month(df$datetime)
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    df <- df %>% filter(month >= 6 & month <= 9)
    
    hmeans <- df %>%  dplyr::group_by(model) %>% 
    filter(year >= 1975 & year <= 2005) %>% 
    dplyr::mutate(mean = mean(value, na.rm = TRUE))
      
    mean_all <- hmeans %>% 
      distinct(mean, .keep_all = TRUE) %>% 
        # mutate(gcm = gcm[[i]],
        #        rcp = rcp[[l]]) %>% 
       select(variable, model, mean, gcm)
    
      
      anomalies <- merge(mean_all, df, by = c("variable", "model", "gcm"
      )) %>% 
        filter(year >= 1975) %>% 
        mutate(anom = value - mean) %>%
        dplyr::filter(model != "Obs") %>% 
        select(year, month, yday, rcp, gcm, model, 
               variable, value, mean, anom)
      
      anomalies$variable <- as.character(anomalies$variable)
      
      anomalies_master <- rbind(anomalies_master, anomalies)
      
      
  }
}









anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


anomalies_master <- anomalies_by_year %>% 
  group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_model = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_model = var(anom, na.rm = TRUE)) %>%
  group_by(rcp, model, variable, year) %>%
  dplyr::mutate(mean_gcm = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_gcm = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_gcm = var(anom, na.rm = TRUE)) %>%
  group_by(gcm, model, variable, year) %>% 
  dplyr::mutate(mean_rcp = mean(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(sd_rcp = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_rcp = var(anom, na.rm = TRUE))


# anomalies_model <- anomalies_by_year %>% 
#   group_by(rcp, gcm, variable, year) %>% 
#   dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>%
#   dplyr::mutate(sd_model = sd(anom, na.rm = TRUE)) %>% 
#   dplyr::mutate(var_model = var(anom, na.rm = TRUE))
# 
# test_model <- anomalies_model %>% 
#   group_by(year) %>% 
#   dplyr::mutate(mean_var = mean(var_model, na.rm = TRUE) ) %>% 
#   distinct(year, .keep_all = TRUE) 
# 
# anomalies_gcm <- anomalies_by_year %>% 
#   group_by(rcp, model, variable, year) %>% 
#   dplyr::mutate(mean_gcm = mean(anom, na.rm = TRUE)) %>%
#   dplyr::mutate(sd_gcm = sd(anom, na.rm = TRUE)) %>% 
#   dplyr::mutate(var_gcm = var(anom, na.rm = TRUE)) 
# 
# test_gcm <- anomalies_gcm %>% 
#   group_by(year) %>% 
#   dplyr::mutate(mean_var = mean(var_gcm, na.rm = TRUE) ) %>% 
#   distinct(year, .keep_all = TRUE) 
#   
# anomalies_rcp <-  anomalies_by_year %>% 
#   group_by(gcm, model, variable, year) %>% 
#   dplyr::mutate(mean_rcp = mean(anom, na.rm = TRUE)) %>% 
#   dplyr::mutate(sd_rcp = sd(anom, na.rm = TRUE)) %>% 
#   dplyr::mutate(var_rcp = var(anom, na.rm = TRUE))




write.csv(anomalies_master, "../../anomaly_calculations/thermodepth_annual_anomalies_rcp85.csv", row.names = F)

anomalies_master <- read.csv("../../anomaly_calculations/thermodepth_annual_anomalies.csv_rcp26")


# library(scales)
# p1 <- ggplot(anomalies_gcm) +
#   geom_line(aes(year, var_gcm, color = model)) + 
#   mytheme
# 
# p1 / ggplot(anomalies_model, aes(year, var_model, color = gcm)) +
#   geom_line() + 
#   mytheme
# 
# anomalies_gcm <- anomalies_gcm %>% 
#   group_by(year, rcp, gcm, variable) %>% 
#   dplyr::mutate(sum_var_gcm = sum(var_gcm, na.rm = TRUE))
# 
# anomalies_model <- anomalies_model %>% 
#   group_by(year, rcp, model, variable) %>% 
#   dplyr::mutate(sum_var_model = sum(var_model, na.rm = TRUE))
# 
# ggplot(anomalies_gcm, aes(year, sum_var_gcm)) + 
#   geom_line(col = "green") + 
#   geom_line(data = anomalies_model, aes(year, sum_var_model), col = "blue") +
#   xlab("Year") + 
#   ylab("Variance") + 
#   mytheme
# 
# ggplot(anomalies_gcm, aes(year, sum_var_gcm)) + 
#   geom_area(fill = 4)
#   
#   
# ggplot(subset(anomalies_model, model == "GLM"), aes(year, var_model, colour = gcm)) +
#   # facet_wrap(~gcm) +
#   geom_line() +
#   geom_line(data = subset(anomalies_gcm, gcm == "MIROC5"), aes(year, var_gcm, color = "GCM")) +
#   labs(y = "Thermocline Depth (m)") +
#   mytheme
# 
# ggplot(test_gcm, aes(year, mean_var)) + 
#   geom_line(col = "green")+
#   geom_line(data = test_model, aes(year, mean_var), col = "blue")
# 
# 
# ggplot(anomalies_gcm, aes(year, anom, colour = model)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = gcm), alpha = 0.1,
#               linetype = .1)+
#   labs(y = "Thermocline Depth (m)") +
#   mytheme
# 
# ggsave('../../../sundox/plots/thermo_depth_anom_year_fgcm_rcp85.png', dpi = 300,width = 384,height = 280, units = 'mm')
# 
# 
# ggplot(subset(anomalies_master), aes(year, anom, colour = gcm)) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Thermocline Depth (m)") +
#   geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = model), alpha = 0.1,
#               linetype = .1) +
#   mytheme
# 
# ggsave('../../../sundox/plots/thermo_depth_anom_year_fler_rcp85.png', dpi = 300,width = 384,height = 280, units = 'mm')
# 
# 
# 
# anomalies_master <- read.csv("../../anomaly_calculations/thermodepth_annual_anomalies_rcp85.csv")
# 
# 
# anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp85") 
# anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp85")
# 
# mean(anom_midcentury$mean_model, na.rm = TRUE)
# max(anom_midcentury$mean_model, na.rm = TRUE)
# median(anom_midcentury$mean_model, na.rm = TRUE)
# sum(anom_midcentury$var_model)
# mean(anom_endcentury$mean_model, na.rm = TRUE)
# max(anom_endcentury$mean_model, na.rm = TRUE)
# median(anom_endcentury$mean_model, na.rm = TRUE)
# sum(anom_endcentury$var_model)
# 
# 
# 
# mean(anom_midcentury$mean_gcm, na.rm = TRUE)
# max(anom_midcentury$mean_gcm, na.rm = TRUE)
# median(anom_midcentury$mean_gcm, na.rm = TRUE)
# sum(anom_midcentury$var_gcm)
# mean(anom_endcentury$mean_gcm, na.rm = TRUE)
# max(anom_endcentury$mean_gcm, na.rm = TRUE)
# median(anom_endcentury$mean_gcm, na.rm = TRUE)
# sum(anom_endcentury$var_gcm)
# 
