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
#remove time col
hdensity[, Time := NULL]


#create a date col
hdensity[, date := dmy(paste0(day, "-", mnth, "-", y))]

#create a day col
hdensity[, winterday := date - min(date), winter]
hdensity[, winterday := as.integer(winterday)]

#recalculate hare density from hectare to 100km2
hdensity[, haredensity := haredensity*10000]



# Categorize years intpo cycle phases -------------------------------------

#categorizing years into cycle phases based on Keith 1990
#took the information from Mike's oecologia paper

#spring to spring, take all april densities
springs <- hdensity[mnth == 4]

#compare last year's density to this year's density
springs[, prevdens := shift(haredensity, n = 1, type = "lag")]

#calculate the finite rate of change
springs[, change := haredensity/prevdens]

#categorize based on rate of change
springs[change > 1.89, phase := "increase"]
springs[winter == "2015-2016", phase := "increase"] #there's no prev winter for this one
springs[change < 0.44, phase := "decrease"]
springs[is.na(phase) & haredensity < 1000, phase := "low"]
springs[is.na(phase) & haredensity > 4000, phase := "peak"]

#pull out just the phases and winters
phases <- springs[, .(winter, phase)]

# run linear models of density decrease by winter -------------------------

#run predictdens function by winter (lm of density over time, predicts for each day)
densitypred <- hdensity[, predictdens(yvar = haredensity, xvar = winterday), by = winter]

#recreate date based on winter day if the min date is oct 1
densitypred[, minyear := tstrsplit(winter, "-", keep = 1)]
densitypred[, mindate := dmy(paste0("30-09", "-", minyear))]
densitypred[, date := mindate + winterday]

#merge in cycle phases 
densitypred <- merge(densitypred, phases, by = "winter", all.x = TRUE)

saveRDS(densitypred, "output/results/dailyharedensities.rds")
