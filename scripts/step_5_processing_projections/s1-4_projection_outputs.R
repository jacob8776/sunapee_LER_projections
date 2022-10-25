## This file is used to download output files that were created through steps 1-4 
## of Sunapee_LER_Projections. If you have run through steps 1-4, do not
## Run this script. Otherwise, this script is necessary to analyze projection output 
## and in order to complete steps 5-6 in Sunapee_LER_projections.

download.file("https://zenodo.org/record/7232735/files/output.zip?download=1", 
paste0(here::here(), "/LER_projections/output.zip"), method = "libcurl")

unzip(paste0(here::here(), "/LER_projections/output.zip"), exdir = paste0(here::here(), "/LER_projections"))

