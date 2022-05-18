# Test mcp_area ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)




# Data --------------------------------------------------------------------

gps <- fread(sample(files, 1))



# subset data -------------------------------------------------------------

onebun <- gps[ID == sample(ID, 1) & date < as.Date()]
twobun <- rbindlist(lapply(sample(files, 2), fread))
goodbun <- gps[ID == "22130" & winter == "2016-2017"]



# Tests -------------------------------------------------------------------

# One bun
mcp_area(gpsdata = onebun, x = "x.utm", y = "y.utm", utmzone = utm7N)


# Two individuals

