# Test mcp_area ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)




# Data --------------------------------------------------------------------

gps <- readRDS("Data/all_gps.rds")



# subset data -------------------------------------------------------------

onebun <- gps[id == sample(id, 1)]
#twobun <- rbindlist(lapply(sample(files, 2), fread))
#goodbun <- gps[id == "22130" & winter == "2016-2017"]



# Tests -------------------------------------------------------------------

# One bun
mcp_area(onebun, x = 'x_proj', y = 'y_proj', utmzone = utm7N, vol = 90)


# Two individuals

