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

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


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
      ts.sch <- lapply(out, function(x) {
        ts.schmidt.stability(x, bathy = bathy, na.rm = TRUE)
      })
      ## Reshape to data.frame
      df <- melt(ts.sch, id.vars = 1)
      colnames(df)[4] <- "model"
      df$yday <- yday(df$datetime)
      df$year <- year(df$datetime)
      df$month <- month(df$datetime)
      df$gcm <- gcm[[i]]
      df$rcp <- rcp[[l]]

        hmeans <- df %>%  dplyr::group_by(model, gcm, rcp) %>% 
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
        select(year, month, yday, rcp.y, gcm, model, 
             variable, value, mean, anom)
        colnames(anomalies)[4] <- c("rcp")
          
        
        anomalies$variable <- as.character(anomalies$variable)
        
        anomalies_master <- rbind(anomalies_master, anomalies)





        
      

      }
    }
  


  
anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


########################### COULD AFFECT VARIANCE DIFFERENCES ###############################################


# Model uncertainty grouped by all relevant columns except for lake model. Calculations are then carried out across lake models 
# for mean, standard deviation and variance. Same method for gcm uncertainty and rcp uncertainty. 

anomalies_model <- anomalies_by_year %>% 
  group_by(rcp, gcm, variable, year) %>% 
  dplyr::mutate(mean_model = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_model = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_model = var(anom, na.rm = TRUE)) %>% 
  dplyr::select(year, rcp, gcm, variable, mean_model:var_model)

ggplot(anomalies_model) +
  geom_line(aes(year, var_model, color = gcm)) +
  facet_wrap(~rcp) +
  coord_cartesian(ylim = c(0, 8000))

anomalies_gcm <- anomalies_by_year %>% 
  group_by(rcp, model, variable, year) %>% 
  dplyr::mutate(mean_gcm = mean(anom, na.rm = TRUE)) %>%
  dplyr::mutate(sd_gcm = sd(anom, na.rm = TRUE)) %>% 
  dplyr::mutate(var_gcm = var(anom, na.rm = TRUE)) %>% 
  dplyr::select(year, rcp, gcm, model, variable, mean_gcm:var_gcm)

ggplot(anomalies_gcm) +
  geom_line(aes(year, var_gcm, color = model)) +
  facet_wrap(~rcp) +
  coord_cartesian(ylim = c(0, 8000))

df <- anomalies_gcm %>% 
  group_by(year) %>% 
  mutate(tot_var = mean(var_gcm))

df2 <- anomalies_model %>% 
  group_by(year) %>% 
  mutate(tot_var = mean(var_model))

ggplot() +
  geom_line(data = df, aes(year, tot_var, color = "GCM")) +
  geom_line(data = df2, aes(year, tot_var, color = "Model"))


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

##################################################################################################################


write.csv(anomalies_master, "../../anomaly_calculations/schmidt_annual_anomalies.csv", row.names = F)

anomalies_master <- read.csv("../../anomaly_calculations/schmidt_annual_anomalies.csv")

ggplot(anomalies_master, aes(year, anom, colour = model)) +
  facet_wrap(gcm~rcp) +
  geom_line() +
  labs(y = "Schmidt stability (J/m2)") +
  geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model), alpha = 0.4,
              linetype = 0.1,
              color = "grey") 



ggplot(subset(anomalies_master, year <= 2010), aes(yday, mean_model, colour = gcm)) + 
  facet_wrap(~year) + 
  geom_line() + 
  geom_ribbon(data = subset(anomalies_master, year <= 2010), aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model), alpha = 0.4,
              linetype = 0.1,
              color = "grey") 


ggplot(subset(anomalies_master), aes(yday, mean_gcm, colour = model)) + 
  facet_wrap(~year) + 
  geom_line() + 
  geom_ribbon(data = subset(anomalies_master), aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm), alpha = 0.4,
              linetype = 0.1,
              color = "grey") 

anom_midcentury <- anomalies_master %>% filter(year >= 2020 & year <= 2050 & rcp == "rcp26") 
anom_endcentury <- anomalies_master %>% filter(year >= 2069 & year <= 2099 & rcp == "rcp26")

mean(anom_midcentury$mean_model, na.rm = TRUE)
max(anom_midcentury$mean_model, na.rm = TRUE)
median(anom_midcentury$mean_model, na.rm = TRUE)
sum(anom_midcentury$var_model)
mean(anom_endcentury$mean_model, na.rm = TRUE)
max(anom_endcentury$mean_model, na.rm = TRUE)
median(anom_endcentury$mean_model, na.rm = TRUE)
sum(anom_endcentury$var_model)



mean(anom_midcentury$mean_gcm, na.rm = TRUE)
max(anom_midcentury$mean_gcm, na.rm = TRUE)
median(anom_midcentury$mean_gcm, na.rm = TRUE)
sum(anom_midcentury$var_gcm)
mean(anom_endcentury$mean_gcm, na.rm = TRUE)
max(anom_endcentury$mean_gcm, na.rm = TRUE)
median(anom_endcentury$mean_gcm, na.rm = TRUE)
sum(anom_endcentury$var_gcm)



# 
# ggplot(data = anomalies_master, aes(x = sd_model, y = sd_gcm)) + geom_point()
# 
# linearmodel <- lm(anomalies_master$sd_gcm~anomalies_master$sd_model)
# summary(linearmodel)
# 
# 
# ggplot(data = anomalies_master, aes(x = yday, y = mean_model)) + geom_line() + 
#          geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model), alpha = 0.4,
#                      linetype = 0.1,
#                      color = "grey") +
#   geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_gcm, ymax=mean_model+sd_gcm), alpha = 0.4,
#               linetype = 0.1,
#               color = "red")
# 
# ggplot(anomalies_by_year, aes(x = year, y = mean_model, col = gcm)) +
#   geom_line() +
#   geom_ribbon(data = anomalies_by_year, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = gcm), alpha = 0.2,
#               linetype = .1) +
#   facet_wrap(~rcp)  
#   
# 
# ggplot(anomalies_by_year, aes(x = year, y = mean_gcm, col = model)) +
#   geom_line() +
#   geom_ribbon(data = anomalies_by_year, aes(ymin = mean_gcm-sd_gcm, ymax = mean_gcm+sd_gcm), alpha = 0.4,
#               linetype = 0.1,
#               col = "grey") + 
#   facet_wrap(~rcp)
# 
# 
# 
# ggplot(anomalies_by_year, aes(x = year, y = var_model, col = "model")) + geom_point()+
#   geom_point(data = anomalies_by_year, aes(x = year, y = var_rcp, col = "rcp")) +
#   geom_point(data = anomalies_by_year, aes(x = year, y = var_gcm, col = "gcm")) 
# 
# 


  
ggplot(subset(anomalies_master), aes(year, mean_model, colour = rcp)) +
  facet_wrap(~gcm) +
  geom_line() +
  geom_ribbon(data = anomalies_master, aes(ymin = mean_model-sd_model, ymax=mean_model+sd_model, fill = rcp), alpha = 0.2,
              linetype = .1)+
  labs(y = "Schmidt stability (J/m2)") +
  mytheme + 
  geom_line(y = 0, col = "black") + 
  geom_vline(xintercept=2006, linetype = "dashed")

ggsave('../../../sundox/plots/schmidt_anom_year_fgcm.png', dpi = 300,width = 384,height = 280, units = 'mm')


ggplot(subset(anomalies_master), aes(year, mean_gcm, colour = rcp)) +
  facet_wrap(~model) +
  geom_line() +
  labs(y = "Schmidt stability (J/m2)") +
  geom_ribbon(data = anomalies_master, aes(ymin = mean_gcm-sd_gcm, ymax=mean_gcm+sd_gcm, fill = rcp), alpha = 0.2,
              linetype = .1) +
  mytheme + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept=2006, linetype = "dashed")


ggsave('../../../sundox/plots/schmidt_anom_year_fler_rcp85.png', dpi = 300,width = 384,height = 280, units = 'mm')













