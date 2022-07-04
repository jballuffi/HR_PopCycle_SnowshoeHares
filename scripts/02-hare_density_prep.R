#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#import hare densities
hdensity <- fread("data/Hare_density_monthly.csv")



# clean hare density ------------------------------------------------------

#classify months as either early or late winter for year stuff
late <- c(1, 2, 3, 4)
early <- c(10, 11, 12)

#change col names
setnames(hdensity, "Year", "winter")
setnames(hdensity, "hdensity", "haredensity")

#pull months, years, and days into separate col
hdensity[, mnth := month(Time)]
hdensity[mnth %in% early, y := tstrsplit(winter, "-", keep = 1)]
hdensity[mnth %in% late, y := tstrsplit(winter, "-", keep = 2)]
hdensity[, day := day(Time)]

#create a date col
hdensity[, date := dmy(paste0(day, "-", mnth, "-", y))]

#create a day col
hdensity[, winterday := date - min(date), winter]
hdensity[, winterday := as.integer(winterday)]

#recalculate hare density from hectare to 100km2
hdensity[, haredensity := haredensity*10000]

#last step: remove time col
hdensity[, Time := NULL]



# run linear models of density decrease by winter -------------------------

#run predictdens function by winter (lm of density over time, predicts for each day)
densitypred <- hdensity[, predictdens(yvar = haredensity, xvar = winterday), by = winter]

#recreate date based on winter day if the min date is oct 1
densitypred[, minyear := tstrsplit(winter, "-", keep = 1)]
densitypred[, mindate := dmy(paste0("30-09", "-", minyear))]
densitypred[, date := mindate + winterday]

saveRDS(densitypred, "output/results/dailyharedensities.rds")
