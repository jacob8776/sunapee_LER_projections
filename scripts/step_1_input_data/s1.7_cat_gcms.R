library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(ncdf4)
library(LakeMetabolizer)
library(here)



setwd(paste0(here::here(), "/met_files_processed/"))


## Extending GFDL
gfdl_hist <- read.csv("GFDL-ESM2M/GFDL-ESM2M_historical.csv")
gfdl_26 <- read.csv("GFDL-ESM2M/GFDL-ESM2M_rcp26.csv")
gfdl_60 <- read.csv("GFDL-ESM2M/GFDL-ESM2M_rcp60.csv")
gfdl_85 <- read.csv("GFDL-ESM2M/GFDL-ESM2M_rcp85.csv")

gfdl_26_long <- rbind(gfdl_hist, gfdl_26)
gfdl_60_long <- rbind(gfdl_hist, gfdl_60)
gfdl_85_long <- rbind(gfdl_hist, gfdl_85)

write.csv(gfdl_26_long, "cat_files/GFDL-ESM2M/GFDL-ESM2M_rcp26.csv", row.names = FALSE)
write.csv(gfdl_60_long, "cat_files/GFDL-ESM2M/GFDL-ESM2M_rcp60.csv", row.names = FALSE)
write.csv(gfdl_85_long, "cat_files/GFDL-ESM2M/GFDL-ESM2M_rcp85.csv", row.names = FALSE)


## Extending HadGEM
hadgem_hist <- read.csv("HadGEM2-ES/HadGEM2-ES_historical.csv")
hadgem_26 <- read.csv("HadGEM2-ES/HadGEM2-ES_rcp26.csv")
hadgem_60 <- read.csv("HadGEM2-ES/HadGEM2-ES_rcp60.csv")
hadgem_85 <- read.csv("HadGEM2-ES/HadGEM2-ES_rcp85.csv")

hadgem_26_long <- rbind(hadgem_hist, hadgem_26)
hadgem_60_long <- rbind(hadgem_hist, hadgem_60)
hadgem_85_long <- rbind(hadgem_hist, hadgem_85)

write.csv(hadgem_26_long, "cat_files/HadGEM2-ES/HadGEM2-ES_rcp26.csv", row.names = FALSE)
write.csv(hadgem_60_long, "cat_files/HadGEM2-ES/HadGEM2-ES_rcp60.csv", row.names = FALSE)
write.csv(hadgem_85_long, "cat_files/HadGEM2-ES/HadGEM2-ES_rcp85.csv", row.names = FALSE)


## Extending IPSL

ipsl_hist <- read.csv("IPSL-CM5A-LR/IPSL-CM5A-LR_historical.csv")
ipsl_26 <- read.csv("IPSL-CM5A-LR/IPSL-CM5A-LR_rcp26.csv")
ipsl_60 <- read.csv("IPSL-CM5A-LR/IPSL-CM5A-LR_rcp60.csv")
ipsl_85 <- read.csv("IPSL-CM5A-LR/IPSL-CM5A-LR_rcp85.csv")

ipsl_26_long <- rbind(ipsl_hist, ipsl_26)
ipsl_60_long <- rbind(ipsl_hist, ipsl_60)
ipsl_85_long <- rbind(ipsl_hist, ipsl_85)

write.csv(ipsl_26_long, "cat_files/IPSL-CM5A-LR/IPSL-CM5A-LR_rcp26.csv", row.names = FALSE)
write.csv(ipsl_60_long, "cat_files/IPSL-CM5A-LR/IPSL-CM5A-LR_rcp60.csv", row.names = FALSE)
write.csv(ipsl_85_long, "cat_files/IPSL-CM5A-LR/IPSL-CM5A-LR_rcp85.csv", row.names = FALSE)


## Extending MIROC

miroc_hist <- read.csv("MIROC5/MIROC5_historical.csv")
miroc_26 <- read.csv("MIROC5/MIROC5_rcp26.csv")
miroc_60 <- read.csv("MIROC5/MIROC5_rcp60.csv")
miroc_85 <- read.csv("MIROC5/MIROC5_rcp85.csv")

miroc_26_long <- rbind(miroc_hist, miroc_26)
miroc_60_long <- rbind(miroc_hist, miroc_60)
miroc_85_long <- rbind(miroc_hist, miroc_85)

write.csv(miroc_26_long, "cat_files/MIROC5/MIROC5_rcp26.csv", row.names = FALSE)
write.csv(miroc_60_long, "cat_files/MIROC5/MIROC5_rcp60.csv", row.names = FALSE)
write.csv(miroc_85_long, "cat_files/MIROC5/MIROC5_rcp85.csv", row.names = FALSE)

