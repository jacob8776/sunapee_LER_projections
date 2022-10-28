library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)
library(LakeEnsemblR)
library(ggpubr)
library(here)
# Set working directory
getwd()
setwd(paste0(here::here()))


folder <- "figures"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}
setwd(paste0(here::here(), '/LER_calibration'))



model <- c("FLake", "Simstrat", "GOTM", "MyLake", "GLM")


ncdf <- "./output/ensemble_output_all_models_2022-08-16.nc"

lst <- load_var(ncdf, "temp")
summary(lst$Obs)
lst$MyLake$wtr_33 <- NA
summary(lst$MyLake)



png("../figures/figure3.png", width = 9,height = 5, units = 'in', res = 500)
#plot_heatmap(ncdf, model = model) +
#  scale_colour_gradientn(limits = c(0, 32),
#                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()
#



plot_heatmap <- function(ncdf = NULL, var = "temp", dim = "model", dim_index = 1,
                         var_list = NULL, model = NULL) {
  
  # check if model input is correct
#  model <- check_models(model)
  if(!is.null(ncdf)){
    # Check if netCDF exists
    if(!file.exists(ncdf)){
      stop("File '", ncdf, "' does not exist. Check you have the correct filepath.")
    }
    # Check if var is in ncdf
    vars <- gotmtools::list_vars(ncdf)
    if(!(var %in% vars)){
      stop("Variable '", var, "' is not present in the netCDF file '", ncdf, "'")
    }
    # get variable
    var_list <- load_var(ncdf, var = var, return = "list",
                         dim = dim, dim_index = dim_index)
  }else{
    var_list <- var_list
  }
  
  # only the selected models
  if(!is.null(model)){
    var_list <- var_list[c(model, "Obs")]
  }
  
  mod_names <- names(var_list)
  
  # Melt list down into long dataframe
  data <- var_list %>%
    reshape2::melt(id.vars = "datetime") %>%
    dplyr::group_by(datetime)
  colnames(data) <- c("datetime", "Depth", "value", "Model")
  data$depth <- -as.numeric(gsub("wtr_", "", data$Depth))
  data <- as.data.frame(data)
  data$Model <- factor(data$Model)
  data$Model <- factor(data$Model, levels = mod_names)
  data$value[data$Model == "MyLake" & data$depth == -33] <- NA
  
  
  spec <- RColorBrewer::brewer.pal(11, "Spectral")
  
  # Remove NAs
  data <- data[!is.na(data$value), ] # Remove NAs
  if(nrow(data) == 0) {
    stop("Modelled  and observed data is all NAs.
         Please inspect the model output and re-run 'run_ensemble()' if necessary.")
  }
  
  
  
  p1 <- ggplot(data) +
    geom_point(aes(datetime, depth, colour = value), shape = 15) +
    scale_colour_gradientn(colours = rev(spec)) +
    facet_wrap(~Model, ncol = 2)
  
  return(p1)
}


plot_heatmap(ncdf, model = model) +
  scale_colour_gradientn(name = "Temperature ºC", limits = c(0, 32),
  colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + 
  theme_classic() + 
  ylab("Depth (m)") + 
  xlab("Year") 

dev.off()

