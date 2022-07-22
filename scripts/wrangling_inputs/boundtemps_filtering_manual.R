library(lubridate)

setwd("/Users/jacobwynne/Dropbox/sunapee_ensemble")



mantemptime <- read_csv("mantemptime.csv")
newtemp <- read_csv("new_temp.csv")

newtemp <- na.omit(newtemp)

mantemptime$source <- "manual"
newtemp$source <- "buoy"

bound_newtemp <- full_join(newtemp, mantemptime, all.x = TRUE)

dups <- bound_newtemp[c("datetime", "Depth_meter")]

filtered_buoy_manual <- bound_newtemp[!duplicated(dups),]

mantemptime <- filter(mantemptime, datetime <= "2007-08-27")

buoy_manual <- rbind(mantemptime, filtered_buoy_manual)




buoy_manual$time <- as.character("00:00:00")
buoy_manual$datetime <- as.Date(buoy_manual$datetime)
formt <- "%Y-%m-%d %H:%M:%S"
buoy_manual$datetime <- as.POSIXct(paste(buoy_manual$datetime, buoy_manual$time), format=formt, tz = 'UTC')
str(buoy_manual)

buoy_manual <- select(buoy_manual, -time, -source)


write.csv(buoy_manual, "~/Dropbox/sunapee_LER_projections/LER_inputs/calibration_inputs/buoy_manual.csv", row.names = FALSE)
