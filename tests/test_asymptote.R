# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Data --------------------------------------------------------------------

gps <- fread(sample(files, 1))



# Subset data -------------------------------------------------------------

onebun <- gps[ID == sample(ID, 1) & date < as.Date()]

goodbun <- gps[ID == "22130" & winter == "2016-2017"]

morebun <- gps[ID %in% sample(unique(gps$ID), 5, replace = FALSE)] 


# Tests -------------------------------------------------------------------


out1 <- area_asym(onebun)

outmore <- morebun[, area_asym(DT = .SD), by = c("ID", "winter")]
