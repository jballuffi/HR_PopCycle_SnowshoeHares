# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Data --------------------------------------------------------------------

gps <- fread(sample(files, 1))



# Subset data -------------------------------------------------------------

onebun <- gps[ID == sample(ID, 1) & date < as.Date()]
twobun <- rbindlist(lapply(sample(files, 2), fread))
goodbun <- gps[ID == "22130" & winter == "2016-2017"]



# Tests -------------------------------------------------------------------


out1 <- area_asym(onebun)
