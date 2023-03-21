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
library(ncdf4)

#source(paste0(here::here(), "/scripts/misc/source_scripts/lvar.R"))


# gcm <- c("GFDL-ESM2M", "HADGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
# # # # List of RCP's to run 
# rcp <- c("rcp85") 


setwd(paste0(here::here(), "/LER_projections/output/"))


anomalies_master <- data.frame("year" = numeric(0), 
                               "rcp" = character(0), "gcm" = character(0), "model" = character(0), "variable" = character(0),
                               "value" = numeric(0), "mean" = numeric(0), "anom" = numeric(0))



gcm <- c("IPSL-CM5A-LR", "MIROC5", "GFDL-ESM2M", "HADGEM2-ES")
# # # List of RCP's to run 
rcp <- c("rcp26", "rcp60", "rcp85") 

for(i in 1:length(gcm)){
  #i = 1
  # Sets working directory to each gcm 
  # Nested for loop goes into RCP scenarios for GCMs 
  for(l in 1:length(rcp)){
    #l = 1
    ncdf <- paste0(gcm[[i]], "_", rcp[[l]], "_output.nc")
    print(ncdf)
    
    # out <- load_var(ncdf = ncdf, var = "temp")
    # out <- out[1:5]
    
    #temp <- load_var(ncdf, var = "temp", return = "list", dim = "model", dim_index = 1, print = TRUE)
    
    return = "list"
    var = "temp"
    dim = "model"
    dim_index = 1
    #load_var <- function(ncdf, var, return = "list", dim = "model", dim_index = 1, print = TRUE){
    
    match.arg(return, c("list", "array"))
    match.arg(dim, c("model", "member"))
    
    # Load Rdata
    data("lake_var_dic", package = "LakeEnsemblR", envir = environment())
    
    if(!file.exists(ncdf)) {
      stop(ncdf, " does not exist. Check the filepath is correct.")
    }
    
    # check if variable is an allowed name
    if(!var %in% lake_var_dic$short_name) {
      stop(paste0("Variable '", var, "' unknown. Allowed names for var: ",
                  paste0(lake_var_dic$short_name, collapse = ", ")))
    }
    
    tryCatch({
      fid <- ncdf4::nc_open(ncdf) # Open netCDF
      
      # Extract the time
      tim <- ncdf4::ncvar_get(fid, "time")
      tunits <- ncdf4::ncatt_get(fid, "time")
      tustr <- strsplit(tunits$units, " ")
      # step <- tustr[[1]][1]
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      tdstr <- strsplit(unlist(tustr)[4], ":")
      thour <- as.integer(unlist(tdstr)[1])
      tmin <- as.integer(unlist(tdstr)[2])
      origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                                  "-", tday, " ", thour, ":", tmin),
                           format = "%Y-%m-%d %H:%M", tz = "UTC")
      time <- as.POSIXct(tim, origin = origin, tz = "UTC")
      
      # Extract model names
      mod_names <- ncdf4::ncatt_get(fid, "model", "Model")$value
      mod_names <- strsplit(mod_names, ", ")[[1]]
      mod_names <- substring(mod_names, 5)
      
      # Extract members
      mem <- ncdf4::ncvar_get(fid, "member")
      
      # Extract variable
      var1 <- ncdf4::ncvar_get(fid, var)
      tunits <- ncdf4::ncatt_get(fid, var)
      miss_val <- tunits$missing_value
      var1[var1 >= miss_val] <- NA # Replace large values with NAs
      var_dim <- strsplit(tunits$coordinates, " ")[[1]]
      
      # Extract depths if dimensions are greater than 2
      if(length(dim(var1)) > 2){
        z <- ncdf4::ncvar_get(fid, "z")
      }
      
    }, warning = function(w) {
      return_val <- "Warning"
    }, error = function(e) {
      return_val <- "Error"
      warning("Error extracting variable from netCDF file!")
    }, finally = {
      ncdf4::nc_close(fid) # Close netCDF file
    })
    
    mat <- matrix(data = c(var, tunits$units, tunits$coordinates),
                  dimnames = list(c("short_name",
                                    "units", "dimensions"), c()))
    
    if(length(dim(var1)) == 4) {
      n_vals <- dim(var1)[2] * (dim(var1)[3]) * (dim(var1)[4])
      # Check for empty members
      for(m in seq_len(dim(var1)[1])) {
        nas <- sum(is.na(var1[m, , , ]))
        if(nas == n_vals) {
          break
        }
      }
      if(m != dim(var1)[1]) {
        var1 <- var1[(seq_len(m-1)), , , ]
      }
    } else if(length(dim(var1)) == 3) {
      n_vals <- dim(var1)[2] * (dim(var1)[3])
      # Check for empty members
      for(m in seq_len(dim(var1)[1])) {
        nas <- sum(is.na(var1[m, , ]))
        if(nas == n_vals) {
          break
        }
      }
      if(m != dim(var1)[1]) {
        var1 <- var1[(seq_len(m-1)), , ]
      }
    }
    
    
    
    if(return == "array"){
      
      if(length(dim(var1)) == 4) {
        dimnames(var1) <- list(paste0("member_", seq_len(dim(var1)[1])),
                               mod_names, as.character(time), z)
      }
      if(length(dim(var1)) == 3 & var == "temp") {
        dimnames(var1) <- list(mod_names, as.character(time), z)
      }
      if(length(dim(var1)) == 3 & var == "ice_height") {
        dimnames(var1) <- list(paste0("member_", seq_len(dim(var1)[1])),
                               mod_names, as.character(time))
      }
      if(length(dim(var1)) == 2) {
        dimnames(var1) <- list(mod_names, as.character(time))
      }
      
      return(var1)
    }
    
    if(return == "list"){
      # 3-D var w/member ----
      if("z" %in% var_dim) {
        if(length(dim(var1)) == 4){
          if(dim == "model") {
            
            if ( dim_index > dim(var1)[1] ) {
              stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                   paste(seq_len(dim(var1)[1]), collapse = ","))
            }
            
            var_list <- lapply(seq(dim(var1)[2]), function(x)var1[dim_index, x, , ])
            names(var_list) <- mod_names
          } else if ( dim == "member" ) {
            
            if (dim_index > dim(var1)[2]) {
              stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                   paste(seq_len(dim(var1)[2]), collapse = ","))
            }
            
            var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, dim_index, , ])
            
            n_vals <- dim(var_list[[1]])[1] * (dim(var_list[[1]])[2])
            
            # Check for empty members
            for(m in seq_len(length(var_list))) {
              nas <- sum(is.na(var_list[[m]]))
              if(nas == n_vals) {
                break
              }
            }
            if(m != length(var_list)) {
              var_list <- var_list[(seq_len(m-1))]
            }
            names(var_list) <- paste0(mod_names[dim_index], "_member_",
                                      seq_len(length(var_list)))
            
          }
          
          # Add datetime column + columns names for rLake Analyzer
          var_list <- lapply(var_list, function(x){
            x <- as.data.frame(x)
            x <- cbind(time, x)
            colnames(x) <- c("datetime", paste0("wtr_", abs(z)))
            return(x)
          })
          # 3-D var & no members ----
        } else if(length(dim(var1)) == 3) {
          var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, , ])
          names(var_list) <- mod_names
          
          # Add datetime column + columns names for rLake Analyzer
          var_list <- lapply(var_list, function(x){
            x <- as.data.frame(x)
            x <- cbind(time, x)
            colnames(x) <- c("datetime", paste0("wtr_", abs(z)))
            return(x)
          })
        }
        
        # 2-D variables no members ----
      } else {
        if (length(dim(var1)) == 2) {
          var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, ])
          names(var_list) <- mod_names
          
          # Add datetime column + columns names for rLake Analyzer
          var_list <- lapply(var_list, function(x){
            x <- as.data.frame(x)
            x <- cbind(time, x)
            colnames(x)[2] <- var
            return(x)
          })
          # 2-D w/ members ----
        } else if (length(dim(var1)) == 3) {
          
          if(dim == "model") {
            
            if ( dim_index > dim(var1)[2] ) {
              stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                   paste(seq_len(dim(var1)[2]), collapse = ","))
            }
            
            var_list <- lapply(seq(dim(var1)[2]), function(x)var1[dim_index, x, ])
            names(var_list) <- mod_names
          } else {
            
            var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, dim_index, ])
            n_vals <- length(var_list[[1]])
            # Check for empty members
            for(m in seq_len(length(var_list))) {
              nas <- sum(is.na(var_list[[m]]))
              if(nas == n_vals) {
                break
              }
            }
            if(m != length(var_list)) {
              var_list <- var_list[(seq_len(m-1))]
            }
            names(var_list) <- paste0(mod_names[dim_index], "_member_",
                                      seq_len(length(var_list)))
          }
          
          # Add datetime column + columns names for variable
          var_list <- lapply(var_list, function(x){
            x <- as.data.frame(x)
            x <- cbind(time, x)
            colnames(x)[2] <- var
            return(x)
          })
          
        }
      }
    }
    
    temp <- var_list
    
    df <- melt(temp[1:5], id.vars = 1)
    colnames(df)[4] <- "model"
    df$gcm <- gcm[[i]]
    df$rcp <- rcp[[l]]
    df$yday <- yday(df$datetime)
    df$year <- year(df$datetime)
    df$month <- month(df$datetime)
    df <- df %>% 
      filter(variable == "wtr_1" | variable == "wtr_30")
    df <- df %>% filter(month >= 6 & month <= 8)
    
    
    
    
    hmeans <- df %>%  
      dplyr::group_by(model, gcm, rcp, variable) %>% 
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
    
    
    
    rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "array"])
    
    
    
  }
}




anomalies_by_year <- anomalies_master %>% 
  group_by(year, rcp, gcm, model, variable, mean) %>% 
  dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

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


write.csv(anomalies_master, "../../anomaly_calculations/surf_bot_anomalies_summer.csv", row.names = F)

anomalies_master <- read.csv("../../anomaly_calculations/surf_bot_anomalies_summer.csv")


anomalies_master_bot <- filter(anomalies_master, variable == "wtr_30")
anomalies_master_sur <- filter(anomalies_master, variable == "wtr_1")

write.csv(anomalies_master_bot, "../../anomaly_calculations/bot_anomalies_summer.csv", row.names = F)
write.csv(anomalies_master_sur, "../../anomaly_calculations/surf_anomalies_summer.csv", row.names = F)


