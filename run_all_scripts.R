
# fils <- list.files("scripts/step_1_input_data/", full.names = TRUE)
# 
# sapply(fils, function(f) {
#   source(f)
#   setwd(here::here())
# })

fils <- list.files("scripts/step_2_calibration/", full.names = TRUE)

sapply(fils, function(f) {
  source(f)
  setwd(here::here())
})

fils <- list.files("scripts/step_3_validation/", full.names = TRUE)

sapply(fils, function(f) {
  source(f)
  setwd(here::here())
})

fils <- list.files("scripts/step_4_running_projections/", full.names = TRUE)

sapply(fils, function(f) {
  source(f)
  setwd(here::here())
})

fils <- list.files("scripts/step_5_processing_projections/", full.names = TRUE)

sapply(fils, function(f) {
  source(f)
  setwd(here::here())
})

fils <- list.files("scripts/step_6_study_results_figures/", full.names = TRUE)

sapply(fils, function(f) {
  source(f)
  setwd(here::here())
})


