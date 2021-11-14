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

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind


setrcp <- c("rcp85")

anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/schmidt_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, rcp == setrcp)


# pull out relevant columns from anomalies_master dataset for model uncertainty 
selected_var_model <- select(anomalies_master, year, var_model)
# pull out unique variance values, should be five for each year 
test <- unique(selected_var_model)
# group by year and sum, remove column of unique variances per year 
test_sum_model <- test %>% 
  group_by(year) %>% 
  mutate(sum_var_model = sum(var_model)) %>% 
  select(-var_model) 
# removing duplicate values for each year, four each year
test_sum_model <- unique(test_sum_model)


selected_var_gcm <- select(anomalies_master, year, var_gcm)
test2 <- unique(selected_var_gcm)
test_sum_gcm <- test2 %>% 
  group_by(year) %>% 
  mutate(sum_var_gcm = sum(var_gcm)) %>% 
  select(-var_gcm) 
test_sum_gcm <- unique(test_sum_gcm)

merged_models <- merge(test_sum_model, test_sum_gcm)


merged_models <- merged_models %>% 
  group_by(year) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(merged_models, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(merged_models, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)
proportions <- filter(proportions, year >= 2006)







proportions_schmidt <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Schmidt Stability")+   ylim(0,1)

# ggplot(data = anomalies_master, mapping = aes(x = year, y = prop_model)) + geom_line() +
#    geom_line(data = anomalies_master, mapping = aes(x = year, y = prop_gcm)) 






## total mixing period

anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")


anomalies_master <- filter(anomalies_master, variable == "MixPer", rcp == setrcp)

anomalies_master <- anomalies_master %>% 
  group_by(year) %>% 
  mutate(sum_var_model = sum(var_model)) %>% 
  mutate(sum_var_gcm = sum(var_gcm)) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(anomalies_master, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(anomalies_master, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)
proportions <- filter(proportions, year >= 2006)





mix_period <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Total Mixing Period") +   ylim(0,1)





### mean surface temperature

anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TsMean", rcp == setrcp)

ggplot() +
  geom_line(data = anomalies_master, aes(year, anom, color = model)) +
  facet_wrap(~gcm)

ggplot() +
  geom_line(data = anomalies_master, aes(year, anom, color = gcm)) +
  facet_wrap(~model)


anomalies_master <- anomalies_master %>% 
  group_by(year) %>% 
  mutate(sum_var_model = mean(var_model)) %>% 
  mutate(sum_var_gcm = mean(var_gcm)) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(anomalies_master, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(anomalies_master, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)

proportions <- filter(proportions, year >= 2006)


proportions_tsmax <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Mean Surface Temperature")+   ylim(0,1)

proportions_tsmax

### maximum surface temperature

anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TbMean", rcp == setrcp)

anomalies_master <- anomalies_master %>% 
  group_by(year) %>% 
  mutate(sum_var_model = sum(var_model)) %>% 
  mutate(sum_var_gcm = sum(var_gcm)) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(anomalies_master, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(anomalies_master, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)
proportions <- filter(proportions, year >= 2006)


proportions_tbmax <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Mean Bottom Temperature")+   ylim(0,1)




### maximum surface temperature

anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TotStratDur", rcp == setrcp)

anomalies_master <- anomalies_master %>% 
  group_by(year) %>% 
  mutate(sum_var_model = sum(var_model)) %>% 
  mutate(sum_var_gcm = sum(var_gcm)) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(anomalies_master, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(anomalies_master, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)
proportions <- filter(proportions, year >= 2006)


proportions_totstratdur <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Total Stratification Duration")+   ylim(0,1)



anomalies_master <- read.csv("~/Dropbox/sunapee_LER_projections/anomaly_calculations/multiple_annual_anomalies.csv")

anomalies_master <- filter(anomalies_master, variable == "TotIceDur", is.na(value) == F, rcp == setrcp)

anomalies_master <- anomalies_master %>% 
  group_by(year) %>% 
  mutate(sum_var_model = sum(var_model, na.rm = TRUE)) %>% 
  mutate(sum_var_gcm = sum(var_gcm, na.rm = TRUE)) %>% 
  mutate(sum_vars = sum_var_model + sum_var_gcm) %>% 
  mutate(prop_model = sum_var_model/sum_vars) %>% 
  mutate(prop_gcm = sum_var_gcm/sum_vars)

prop_model <- select(anomalies_master, year, prop_model)
prop_model$method <- "model"
colnames(prop_model) <- c("year", "prop", "method")
prop_gcm <- select(anomalies_master, year, prop_gcm)
prop_gcm$method <- "gcm"
colnames(prop_gcm) <- c("year", "prop", "method")

proportions <- rbind(prop_model, prop_gcm)
proportions <- filter(proportions, year >= 2006)


proportions_toticedur <- ggplot(data = proportions, aes(x = year, y = prop, color = method)) + 
  geom_line() + mytheme + 
  ggtitle("Total Ice Duration")+   ylim(0,1)



ggarrange(proportions_tsmax, proportions_tbmax, proportions_schmidt,
          proportions_totstratdur, proportions_toticedur, mix_period,
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")


ggsave('~/Dropbox/sundox/plots/variance_prop85.png', dpi = 300,width = 384,height = 280, units = 'mm')

