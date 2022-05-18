# Test mcp_area ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)




# Data --------------------------------------------------------------------
files <- dir("data/Cleaned_gps", full.names = TRUE)

onebun <- fread(sample(files, 1))
twobun <- rbindlist(lapply(sample(files, 2), fread))

# Tests -------------------------------------------------------------------
# One individual

mcp_area(gpsdata = onebun, x = onebun$x.utm, y = onebun$y.utm, utmzone = utm7N)


# Two individuals

