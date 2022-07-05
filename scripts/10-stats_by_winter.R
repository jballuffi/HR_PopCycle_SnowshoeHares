
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!HRninety > 20]
dat <- dat[!winter == "2021-2022"]

# correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- dat[, .(haredensity, lynxdensity, ppratio, mass, winterday)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)

#winter day correlates with hare density 
#hare density correlates with lynx density




lm_year <- function(yvar, xvar1, xvar2) {
  
  model <- lm(yvar ~ xvar1 + xvar2)
  
  #summarize model
  out <- summary(model)
  
  #collect coef values
  coefOut <- data.table(t(out$coefficients[, 1]))
  coefOut<-round(coefOut, 3)
  
  #collect standard errors
  seOut <- data.table(t(out$coefficients[, 2]))
  seOut<-round(seOut, 3)
  
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 3)
  
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}



mod <- lm(HRninety ~ haredensity + mass + Food, dat)

out <- dat[, lm_year(yvar = HRninety, xvar1 = ppratio, xvar2 = mass), by = winter]


