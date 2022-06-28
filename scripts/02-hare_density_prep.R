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

#function runs model and creates a prediction for each day
predictdens <- function(yvar, xvar) {
  # Make the model
  model <- lm(yvar ~ xvar)
  #pull out slopes and intercepts
  slope <- coef(model)["xvar"]
  int <- coef(model)["(Intercept)"]
  #create data frame of dates
  output <- data.table(winterday = seq(1, 197, by = 1))
  #predict densities for each date
  output[, hdensity := (slope*winterday) + int]
  return(output)
}


test <- hdensity[, regtable(yvar = haredensity, xvar = date), by = winter]

