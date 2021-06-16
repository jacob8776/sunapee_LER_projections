library(tidyverse)

getwd()

setwd("/Users/jacobwynne/Dropbox/sunapee_ensemble")



mantemptime <- read_csv("mantemptime.csv")
newtemp <- read_csv("new_temp.csv")

newtemp <- na.omit(newtemp)

mantemptime$source <- "manual"
newtemp$source <- "buoy"

?merge

boundtemps <- rbind(mantemptime, newtemp)
str(boundtemps)

boundtemp2 <- filter(boundtemps, duplicated(boundtemps$datetime, boundtemps$Depth_meter) == FALSE)

boundtemp2 <- boundtemps %>% 
  group_by(datetime, Depth_meter) %>% 
  mutate(mean_temp = mean(Water_Temperature_celsius, na.rm = TRUE))


table(duplicated(boundtemps))


boundtemps <- boundtemps %>% 
  drop_na()

ggplot(data = boundtemp2, aes(x = datetime, y = Water_Temperature_celsius, col = as.factor(source))) + 
  geom_point()

ggplot(data = newtemp, aes(x = datetime, y = Water_Temperature_celsius)) + geom_point()


# write.csv(boundtemps, "boundtemps.csv", row.names = FALSE)

