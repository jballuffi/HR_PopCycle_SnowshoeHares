
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!M90 > 20]
#dat <- dat[!phase == "low"]




# Run AIC by phase of cycle -----------------------------------------------

#make dat into a list by phase, lapply this function? 

makeAIC <- function(hr, p, c, w, s, f){ 
  
  #list models
  pred <- lm(hr ~ p)
  comp <- lm(hr ~ c)
  res <- lm(hr ~ w + s + f)
  pred_res <- lm(hr ~ p + w + s + f)
  comp_res <- lm(hr ~ c + w + s + f)
  null <- lm(hr ~ 1)
  
  #list models and provide names
  mods <- list(pred, comp, res, pred_res, comp_res, null)
  names <- c("Predation", "Competition", "Resource", "Predation+Resource", "Competition+Resource", "Null")
  
  AIC <- as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
  AIC[, ModelLik := NULL]
  AIC[, Cum.Wt := NULL]
  AIC <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits
  
  #which models are less than 2 delta AIC? 
  #topmods <- AIC[Delta_AICc < 2, return((Modnames))]
  
  return(AIC)

  }

increase <- dat[phase == "increase", makeAIC(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD, f = Food)]




dat[phase == "peak", makeAIC(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD, f = Food)]

dat[phase == "decrease", makeAIC(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD, f = Food)]

#in order to run the models in the low you must remove food add as a factor
#dat[phase == "low", makeAIC(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD)]




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



mod <- lm(M90 ~ haredensity + mass + Food, dat)

out <- dat[, lm_year(yvar = M90, xvar1 = SD, xvar2 = mass), by = winter]


