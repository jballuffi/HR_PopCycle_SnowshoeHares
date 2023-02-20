# Test asymptote ----------------------------------------------------------



# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Data --------------------------------------------------------------------

gps <- readRDS("Data/all_gps.rds")



# Subset data -------------------------------------------------------------

onebun <- gps[id %in% sample(unique(gps$id), 1, replace = FALSE)]

morebun <- gps[id %in% sample(unique(gps$id), 10, replace = FALSE)] 


# Tests ----- --------------------------------------------------------------


out1 <- area_asym(onebun)

outmore <- morebun[, area_asym(DT = .SD), by = c("id", "winter")]

outmore$id <- as.factor(outmore$id)


ggplot(outmore)+
  geom_path(aes(x = daycount, y = area, group = id, color = id))


full <- gps[, area_asym(DT = .SD), by = c("id", "winter")]

