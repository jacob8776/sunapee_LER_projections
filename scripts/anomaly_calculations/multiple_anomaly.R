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


# 
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# # # List of RCP's to run 
rcp <- c("rcp26", "rcp60", "rcp85") 




setwd("~/Dropbox/sunapee_LER_projections/LER_projections/output/")


anomalies_master <- data.frame("year" = numeric(0), 
                               "rcp" = character(0), "gcm" = character(0), "model" = character(0), "variable" = character(0),
                               "value" = numeric(0), "mean" = numeric(0), "anom" = numeric(0))



for(i in 1:length(gcm)){
  # i = 1
  # Sets working directory to each gcm 
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    # l = 1
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    print(ncdf)
    
    # out <- load_var(ncdf = ncdf, var = "temp")
    # out <- out[1:5]
    
    temp <- load_var(ncdf, "temp")
    ice <- load_var(ncdf, "ice_height")
    # ice <- lapply(1:length(ice), function(x){
    #   ice[[x]] %>%
    #     mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))    })
    # names(ice) <- c("FLake", "MyLake", "GLM", "Simstrat", "GOTM", "Obs")
    
    
    out <- lapply(1:length(temp), function(x) {
      # x = 1 # for debugging
      mlt <- reshape::melt(temp[[x]], id.vars = 1)
      mlt[, 2] <- as.numeric(gsub("wtr_", "", mlt[, 2]))
      analyze_strat(data = mlt, H_ice = ice[[x]][, 2])
    })
    names(out) <- names(temp)
    

    

    df <- melt(out[1:5], id.vars = 1)
    colnames(df)[4] <- "model"
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    
    hmeans <- df %>%  
    filter(year >= 1975 & year <= 2005) %>% 
    dplyr::group_by(model, gcm, rcp, variable) %>% 
    dplyr::mutate(mean = mean(value, na.rm = TRUE))
      
    mean_all <- hmeans %>% 
      dplyr::filter(model != "Obs") %>% 
      distinct(mean, .keep_all = TRUE) %>% 
      # mutate(gcm = gcm[[i]],
      #        rcp = rcp[[l]]) %>% 
      select(variable, model, mean, gcm)
    
    
    
    

      anomalies <- merge(mean_all, df, by = c("variable", "model", "gcm"
      )) %>% 
        filter(year >= 1975) %>% 
        mutate(anom = value - mean) %>%
        dplyr::filter(model != "Obs") %>% 
        select(year, rcp.y, gcm, model, 
               variable, value, mean, anom)
      colnames(anomalies)[2] <- c("rcp")
      
      
      anomalies$variable <- as.character(anomalies$variable)
      
      anomalies_master <- rbind(anomalies_master, anomalies)
      
      
      
      
      
    
    
    
  }
}




anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

anomalies_master <- anomalies_master %>% 
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


write.csv(anomalies_master, "../../anomaly_calculations/multiple_annual_anomalies.csv", row.names = F)

anomalies_master <- read.csv("../../anomaly_calculations/multiple_annual_anomalies.csv")


mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


# anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26")
# anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")
#
# mean_midcentury <- mean(anom_midcentury$anom, na.rm = TRUE)
# mean_endcentury <- mean(anom_endcentury$anom, na.rm = TRUE)
# max(anom_midcentury$anom, na.rm = TRUE)
# max(anom_endcentury$anom, na.rm = TRUE)
# median(anom_midcentury$anom, na.rm = TRUE)
# median(anom_endcentury$anom, na.rm = TRUE)


# # Plotting TsMax
#
# ggplot(subset(anomalies_master, variable == "TsMax"), aes(year, mean_model, colour = rcp)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
#               linetype = .1)+
#   labs(y = "Degrees C") +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/gcm_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# ggplot(subset(anomalies_master, variable == "TsMax"), aes(year, mean_gcm, colour = rcp)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/ler_tsmax.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# # plotting TsMin
#
#
# ggplot(subset(anomalies_master, variable == "TsMin"), aes(year, mean_model, colour = rcp)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
#               linetype = .1)+
#   labs(y = "Degrees C") +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/gcm_tsmin.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# ggplot(subset(anomalies_master, variable == "TsMin"), aes(year, mean_gcm, colour = rcp)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/ler_tsmin.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# # Plotting TbMax
#
# ggplot(subset(anomalies_master, variable == "TbMax"), aes(year, mean_model, colour = rcp)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
#               linetype = .1)+
#   labs(y = "Degrees C") +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/gcm_tbmax.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# ggplot(subset(anomalies_master, variable == "TbMax"), aes(year, mean_gcm, colour = rcp)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/ler_tbmax.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
#
# # Plotting TbMin
#
# ggplot(subset(anomalies_master, variable == "TbMin"), aes(year, mean_model, colour = rcp)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
#               linetype = .1)+
#   labs(y = "Degrees C") +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/gcm_tbmin.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# ggplot(subset(anomalies_master, variable == "TbMin"), aes(year, mean_gcm, colour = rcp)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
#   ylim(-5, 15) +
#   mytheme
#
# ggsave('../../../sundox/plots/ler_tbmin.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# # Plotting Total stratification duration
#
# ggplot(subset(anomalies_master, variable == "TotStratDur"), aes(year, mean_model, colour = rcp)) +
#   facet_wrap(~gcm) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
#               linetype = .1)+
#   labs(y = "Degrees C") +
#   ylim(-5, 100) +
#   mytheme
#
# ggsave('../../../sundox/plots/totstratdur_gcm.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# ggplot(subset(anomalies_master, variable == "TotStratDur"), aes(year, mean_gcm, colour = rcp)) +
#   geom_hline(yintercept = 0) +
#   facet_wrap(~model) +
#   geom_line() +
#   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
#   ylim(-5, 100) +
#   mytheme
#
#
# ggsave('../../../sundox/plots/totstratdur_ler.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
#
# # Plotting total ice duration
#
 # ggplot(subset(anomalies_master, variable == "TotIceDur"), aes(year, mean_gcm, colour = rcp)) +
 #   facet_wrap(~model) +
 #   geom_line() +
 #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
 #               linetype = .1)+
 #   labs(y = "Degrees C") +
 #   mytheme

# ggsave('../../../sundox/plots/toticedur_ler.png', dpi = 300,width = 384,height = 280, units = 'mm')
#
# unique(anomalies_master$variable)
#
 ggplot(subset(anomalies_master, variable == "TsMean" & rcp == "rcp85"), aes(year, anom, colour = gcm)) +
   geom_hline(yintercept = 0) +
   facet_wrap(~model) +
   geom_line() +
   labs(y = "Degrees C") +
#   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
#               linetype = .1) +
   mytheme
#
#
 
 ggplot(subset(anomalies_master, variable == "TsMean" & rcp == "rcp85"), aes(year, anom, colour = model)) +
   geom_hline(yintercept = 0) +
   facet_wrap(~gcm) +
   geom_line() +
   labs(y = "Degrees C") +
   #   geom_ribbon(aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
   #               linetype = .1) +
   mytheme
