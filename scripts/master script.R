#master script for the whole project

source("scripts/01-data_prep.R")

source("scripts/02-hare_density_prep.R")

source("scripts/03-calc_HRarea.R")

source("scripts/04-extract_weights.R")

source("scripts/05-food_adds.R")

source("scripts/06-snow_depth.R")

source("scripts/07-merge_data.R")

source("scripts/08-system_plots.R")

source("scripts/09-homerange_plots.R")

source("scripts/10-final_models_figures.R")
