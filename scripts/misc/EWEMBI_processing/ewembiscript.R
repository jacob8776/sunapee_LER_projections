library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(ncdf4)
library(LakeMetabolizer)


setwd("~/Dropbox/JHW_thesis/Sunapee/EWEMBI")



flist <- list.files(pattern = "^.*\\.(nc|NC|Nc|Nc)$")


# Open a connection to the first file in our list
flist
nc <- nc_open(paste0(flist[1]))


# vars = c('hurs', #Near surface relative humidity
#          'huss', #Near surface specific humidity
#          'pr', #Precipitation
#          'prsn', #Snowfall flux
#          'ps', #Surface Pressure 
#          'psl', #Sea Level Pressure
#          'rlds', #Surface downwelling longwave radiation
#          'rsds', #Surface Downwelling Shortwave Radiation
#          'sfcWind', #Surface wind speed
#          'tas', #Near surface Air temperature
#          'tasmax', #Daily maximum near surface air temperature
#          'tasmin', #Daily near surface air temperature minimum
#          'uas', #Eastward near-surface wind
#          'vas' #Northward near-surface wind
# )



# Save the print(nc) dump to a text file (same name as the nc file with a txt extension)
{
  sink(paste0( flist[1], ".txt"))
  print(nc)
  sink()
}


# Get a list of the NetCDF's R attributes:
attributes(nc)$names

##  [1] "filename"    "writable"    "id"          "safemode"    "format"     
##  [6] "is_GMT"      "groups"      "fqgn2Rindex" "ndims"       "natts"      
## [11] "dim"         "unlimdimid"  "nvars"       "var"

print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))

## [1] "The file has 3 variables, 2 dimensions and 52 NetCDF attributes"


# Get a list of the nc variable names.
attributes(nc$var)$names

## [1] "CHL1_mean"  "CHL1_flags" "CHL1_error"

# Take a look at the chlorophyll variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[1])

## $standard_name
## [1] "mass_concentration_of_chlorophyll_a_in_sea_water"
## 
## $long_name
## [1] "Chlorophyll concentration - Mean of the binned pixels"
## 
## $`_FillValue`
## [1] -999
## 
## $units
## [1] "mg/m3"
## 
## $pct_characterised_error
## [1] 43.31

# Retrieve a matrix of the chlorophyll data using the ncvar_get function:
varoi <- ncvar_get(nc, attributes(nc$var)$names[1])

# Print the data's dimensions
dim(varoi)

## [1] 453 256


variableofi <- as.data.frame(varoi)

variableofi <- t(variableofi)

variableofi <- as.data.frame(varoi)


varoi
time <- as.data.frame(nc$dim$time$vals)

?seq.dates


# new <- cbind(huss, time)


# colnames(new) <- c("huss", "time")
# 
# plot(new$time, new$huss)


?seq.Date

dates <- as.data.frame(seq.Date(from = as.Date("1979-01-01"), by = "day", length.out = 13880))

both <- cbind(dates, variableofi)


colnames(both) <- c("datetime", "Var")


metd <- read_csv("met_d.csv")
air_temp <- read_csv("airtemp_mean_filtered.csv")
wind_mean <- read_csv("wind_mean_filtered.csv")
par_mean <- read_csv("par_mean_filtered.csv")
shortwave <- par.to.sw(par_mean, par.col = "PAR_umolm2s")



metd$Air_Temperature_kelvin <- metd$Air_Temperature_celsius+274.15
air_temp$mean_temp <- air_temp$mean_temp+274.15


colnames_list <- colnames(metd)
colnames_list


?merge

merged <- merge(metd, both_par, by = "datetime")

mergedtemp <- merge(air_temp, both, by = "datetime")

# wind_mean$datetime <- as.Date(wind_mean$datetime)
# mergedwind <- merge(wind_mean, both, by = "datetime")

#merged_sw <- merge(shortwave, both, by = "datetime")

# ggplot(data = subset(metd, datetime >= "1979-01-01" & datetime <= "1981-01-01"),
#        aes(x = datetime, y = Shortwave_Radiation_Downwelling_wattPerMeterSquared))+
#   geom_point(color = "blue") +
#   geom_point(data = subset(both, datetime >= "1979-01-01" & datetime <= "1981-01-01"),
#              aes(x = datetime, y = Var), color = "red") +
#   ylab("Surface_Level_Barometric_Pressure_pascal")





ggplot(data = subset(mergedwind, datetime <= "2011-08-28"), 
       aes(x = datetime, y = WindSp_ms)) + 
  geom_point(color = "blue") + 
  geom_point(data = subset(mergedwind, datetime <= "2011-08-28"), 
             aes(x = datetime, y = Var), color = "red") + 
  ylab("Windspeed (m/s)") 






mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=14), axis.text = element_text(size = 16))

fp_cat_full <- lm(mean_temp~Var, data = mergedtemp)

routing <- list(paste('Model: ', round(coef(fp_cat_full)[1], 3), ' + ',
                      round(coef(fp_cat_full)[2], 3), 'x,', ' R^2 = ', round(summary(fp_cat_full)[['r.squared']], 3), sep = ''))

resi <- resid(fp_cat_full)




summary(fp_cat_full)

#run overlap between logical combinations of vars
plot1 <- ggplot(data = mergedtemp, aes(x = Var, y = mean_temp))+
  geom_point(size = 1)+
#  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 5)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "cyan4")+
  geom_text(aes(x = 270, y = 240, label = routing[[1]]), vjust = 0, colour = "black") +
#  geom_text(aes(label = datetime), hjust=0, vjust=0)+
  xlab("EWEMBI Windspeed (m/s)")+
  ylab("Local Windspeed (m/s)")+
  mytheme
plot1













