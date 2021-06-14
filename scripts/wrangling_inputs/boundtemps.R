library(tidyverse)

getwd()

setwd("/Users/jacobwynne/Dropbox/sunapee_ensemble")



mantemptime <- read_csv("mantemptime.csv")
newtemp <- read_csv("new_temp.csv")

boundtemps <- rbind(mantemptime, newtemp)
str(boundtemps)

boundtemps <- boundtemps %>% 
  drop_na()

write.csv(boundtemps, "boundtemps.csv", row.names = FALSE)
