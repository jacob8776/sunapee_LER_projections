# combine the two datasets

setwd("/Users/jacobwynne/Dropbox/sunapee_ensemble")

manual <- read_csv("mantemptime.csv")
manual$method <- "manual"
buoy <- read_csv("new_temp.csv")
buoy$method <- "buoy"



temp_data <- full_join(manual, buoy)
temp_data$method <- ordered(temp_data$method, levels = c("manual", "buoy"))
temp_data <- temp_data[order(temp_data$method, temp_data$DateTime, temp_data$Depth),]
dups <- temp_data[c("DateTime", "Depth")]
dup_rows <- duplicated(temp_data[,1:2])
test <- temp_data[!duplicated(temp_data[,1:2]),]
table(duplicated(test[,1:2]))
table(duplicated(temp_data[,1:2]))