# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)




# Data --------------------------------------------------------------------
# tar_load()


# Tests -------------------------------------------------------------------
# One individual

#grab just one bunny year
onebun <- gps[ID == "22130" & winter == "2016-2017"]
#calculate difference between fix date and first date
onebun[, diffday := date - min(date)]


# Two individuals

