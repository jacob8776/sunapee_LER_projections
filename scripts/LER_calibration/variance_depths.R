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
library(wesanderson)


ncdf <- "~/Dropbox/sunapee_LER_projections/LER_calibration/output/ensemble_output_all_models_31Aug21.nc"


out <- load_var(ncdf = ncdf, var = "temp")


df <- melt(out[1:5], id.vars = 1)
colnames(df)[4] <- "model"

df$variable <- as.character(df$variable)

df$variable <- sapply(strsplit(df$variable, split='_', fixed=TRUE), function(x) (x[2]))

df$variable <- as.numeric(df$variable)
df$variable <- round(df$variable, 0)
df$year <- year(df$datetime)
df$month <- month(df$datetime)


# df <- 
#   df %>% 
#   filter(month >= 6 & month <= 8) %>% 
#   group_by(variable, model, year) %>% 
#   dplyr::mutate(var = var(value, na.rm = TRUE))




df_obs <- melt(out[6], id.vars = 1)
colnames(df_obs)[4] <- "model"

df_obs$variable <- as.character(df_obs$variable)

df_obs$variable <- sapply(strsplit(df_obs$variable, split='_', fixed=TRUE), function(x) (x[2]))

df_obs$variable <- as.numeric(df_obs$variable)
df_obs$variable <- round(df_obs$variable, 0)
df_obs$year <- year(df_obs$datetime)
df_obs$month <- month(df_obs$datetime)


# df_obs <- 
#   df_obs %>% 
#   filter(month >= 6 & month <= 8) %>% 
#   group_by(variable, model, year) %>% 
#   dplyr::mutate(var = var(value, na.rm = TRUE))






df_obs_long <- rbind(df_obs, df_obs, df_obs, df_obs, df_obs)
df_bound <- cbind(df, df_obs_long)

colnames(df_bound) <- c("datetime", "variable", "value", "model", "year", "month", "datetime_obs", 
                        "variable_obs", "value_obs", "model_obs", "year_obs", "month_obs")

df_bound <- filter(df_bound, is.na(value_obs) == FALSE)

df_bound_test <- df_bound

df_bound_test$layer <- NA
df_bound_test$layer[df_bound_test$variable < 7] <- "epi"
df_bound_test$layer[df_bound_test$variable < 7] <- "epi"
df_bound_test$layer[df_bound_test$variable >= 7 & df_bound_test$variable <= 10] <- "meta"
df_bound_test$layer[df_bound_test$variable > 10] <- "hyp"


df_bound_test <- df_bound_test %>% 
  filter(month >= 6 & month <= 8) %>%
  group_by(variable, model, year) %>% 
  dplyr::mutate(var = var(value, na.rm = TRUE)) %>% 
  dplyr::mutate(var_obs = var(value_obs, na.rm = TRUE)) 



mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"), legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))
scale_colour_discrete <- ggthemes::scale_color_economist()
scale_fill_discrete <- ggthemes::scale_color_economist()

ggplot(data = subset(df_bound_test), aes(x = var, y = var_obs, shape = model)) + 
  geom_point(aes(color = layer)) +   
  facet_wrap(~year) + 
  geom_abline(slope = 1, intercept = 0) +
  coord_equal(xlim = c(0, 20), ylim = c(0,20)) + 
  xlab("Model Variance") + 
  ylab("Observed Variance") + 
  mytheme

ggsave('~/Dropbox/sundox/plots/variance_v_variance.png', dpi = 300,width = 384,height = 280, units = 'mm')


ggplot(data = subset(df_bound_test), aes(x = var, y = var_obs, shape = model)) + 
  geom_point(aes(color = dep_cat)) +   
  facet_wrap(~year) + 
  geom_abline(slope = 1, intercept = 0) +
  coord_equal(xlim = c(0, 30), ylim = c(0,30)) + 
  mytheme











  




