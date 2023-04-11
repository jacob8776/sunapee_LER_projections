
library(dplyr)
library(here)

setwd(paste0(here::here(), "/LER_inputs/"))

manual_buoy_temptst <- read.csv("manual_buoy_temp.csv")
colnames(manual_buoy_temptst) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")

manual_buoy_temptst$datetime <- as.POSIXct(manual_buoy_temptst$datetime) - 4*60*60
oneday <- filter(manual_buoy_temptst, datetime == "1986-07-03 12:00:00")


oneday$Water_Temperature_celsius <- 4



oneday$datetime <- as.POSIXct("1975-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")

write.csv(oneday, row.names = FALSE, file = "ic_historical.csv")


onedayother <- oneday

onedayother$datetime <- as.POSIXct("2006-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")


write.csv(onedayother, row.names = FALSE, file = "ic_projections.csv")

ic_historical <- read.csv("ic_historical.csv")

ic_historical$datetime <- as.Date(ic_historical$datetime)
head(ic_historical)

ic_historical$hour <- c("00:00:00")
head(ic_historical)
ic_historical$datetime <-  paste0(ic_historical$datetime, " ", ic_historical$hour)

# ic_historical$datetime <- as.POSIXct(ic_historical$datetime, format = "%Y-%m-%d %H:%M:%S")
head(ic_historical)
ic_historical <- select(ic_historical, -hour)
str(ic_historical)

write.csv(ic_historical, "ic_historical_hr0.csv", row.names = FALSE)


setwd(paste0(here::here(), "/LER_inputs/"))


ic_projections <- read.csv("ic_projections.csv")

ic_projections$datetime <- as.Date(ic_projections$datetime)
head(ic_projections)

ic_projections$hour <- c("00:00:00")
head(ic_projections)
ic_projections$datetime <-  paste0(ic_projections$datetime, " ", ic_projections$hour)

# ic_historical$datetime <- as.POSIXct(ic_historical$datetime, format = "%Y-%m-%d %H:%M:%S")
head(ic_projections)
ic_projections <- select(ic_projections, -hour)
str(ic_projections)

write.csv(ic_projections, "ic_projections_hr0.csv", row.names = FALSE)




