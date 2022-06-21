# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Data --------------------------------------------------------------------

gps <- readRDS("Data/all_gps.rds")



# Subset data -------------------------------------------------------------

onebun <- gps[id %in% sample(unique(gps$id), 1, replace = FALSE)]

morebun <- gps[id %in% sample(unique(gps$id), 5, replace = FALSE)] 


# Tests -------------------------------------------------------------------


out1 <- area_asym(onebun)

outmore <- morebun[, area_asym(DT = .SD), by = c("id", "winter", "season")]
