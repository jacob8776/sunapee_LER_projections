setwd("~/Dropbox/sunapee_ensemble")



temp <- read_csv("mantemp.csv")

plot(x = temp$datetime, y = temp$Depth_meter)

plot(x = subset(temp$datetime, temp$Depth_meter == 24), y = subset(temp$Water_Temperature_celsius, temp$Depth_meter == 24), type = 'line')



unique(temp$datetime)


met <- read_csv("met_d.csv")
plot(x = met$datetime)
     

plot(x = temp$datetime)



inflow <- read_csv("infens.csv")


plot(x = inflow$datetime, y = inflow$Flow_metersCubedPerSecond)

unique(inflow$Flow_metersCubedPerSecond)




730/13521

729/37




setwd("~/Dropbox/GLM_Sunapee_Drive")

read_csv("Sun")









