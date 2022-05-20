# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Data --------------------------------------------------------------------

gps <- readRDS("Data/all_gps.rds")



# Subset data -------------------------------------------------------------

onebun <- gps[ID %in% sample(unique(gps$ID), 1, replace = FALSE)]

goodbun <- gps[ID == "22130" & winter == "2016-2017"]

morebun <- gps[ID %in% sample(unique(gps$ID), 5, replace = FALSE)] 


# Tests -------------------------------------------------------------------


out1 <- area_asym(onebun)

outmore <- morebun[, area_asym(DT = .SD), by = c("ID", "winter")]
