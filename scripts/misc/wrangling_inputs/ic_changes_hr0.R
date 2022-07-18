setwd("~/Dropbox/sunapee_LER_projections/LER_inputs/")

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
















