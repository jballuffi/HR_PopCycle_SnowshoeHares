

# prep and import data ----------------------------------------------------


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in HR areas
areas <- readRDS("output/results/hrareas.rds")

#import hare densities
hdensity <- fread("data/Hare_density_simple.csv")

#import lynx densities
ldensity <- fread("data/Lynx Density_simple.csv")

#import weight results
weights <- readRDS("output/results/bodymass.rds")


# clean hare density ------------------------------------------------------


hdensity[, season := tstrsplit(Season_year, " ", keep = 1)]
hdensity[, year := tstrsplit(Season_year, " ", keep = 2)]

hdensity[, year := as.integer(year)]

hdensity[season == "Fall", season := "early"]
hdensity[season == "Spring", season := "late"]

hdensity <- hdensity[year > 2010]

hdensity[season == "early", winter := paste(year, "-", year+1)]
hdensity[season == "late", winter := paste(year-1, "-", year)]

hdensity[, winter := gsub(" ", "", winter)]

hdensity[, Season_year := NULL]

setnames(hdensity, "Density", "haredensity")

hdensity[, haredensity := haredensity*10000]

# merge densities ---------------------------------------------------------

#rename lynx data
names(ldensity) <- c("winter", "ltracks", "ltrack_se", "ltrack_lower", "ltrack_upper", "lynxdensity")
#subset just two columns of interest
lynx <- ldensity[, .(winter, ltracks, lynxdensity)]


densities <- merge(hdensity, ldensity, by = "winter", all.x = TRUE)

densities[, ppratio := lynxdensity/haredensity]

plot(densities$ldensity ~ densities$hdensity)



# merge all data ----------------------------------------------------------

areas[, id := as.factor(id)]

#merge weights with area
area.weight <- merge(areas, weights, by = c("id", "winter", "season"), all.x = TRUE)

#merge in densities
area.weight.densities <- merge(area.weight, densities, by = c("winter", "season"), all.x = TRUE)

#save merged data
saveRDS(area.weight.densities, "output/results/compileddata.rds")
