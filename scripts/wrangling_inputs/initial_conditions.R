
library(dplyr)

setwd("~/Dropbox/sunapee_LER_projections/LER_inputs/")

oneday <- filter(manual_buoy_temptst, datetime == "1986-07-03 12:00:00")


oneday$Water_Temperature_celsius <- 4



oneday$datetime <- as.POSIXct("1975-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")

write.csv(oneday, row.names = FALSE, file = "ic_historical.csv")


onedayother <- oneday

onedayother$datetime <- as.POSIXct("2006-01-01 12:00:00", format = "%Y-%m-%d %H:%M:%S")


write.csv(onedayother, row.names = FALSE, file = "ic_projections.csv")



