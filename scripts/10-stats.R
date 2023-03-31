
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!M90 > 20]
dat <- dat[!is.na(SD)]
#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]


# correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- dat[, .(haredensity, mortrate, mass, SD)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# AIC for analysis across winters -----------------------------------------

#list models
cycle <- lm(M90 ~ phase, dat)
predation <- lm(M90 ~ ppratio, dat)
competition <- lm(M90 ~ haredensity, dat)
resource <- lm(M90 ~ mass + SD + Food*sex, dat)
pred_res <- lm(M90 ~ ppratio + mass + SD + Food*sex, dat)
comp_res <- lm(M90 ~ haredensity + mass + SD + Food*sex, dat)
cycle_res <- lm(M90 ~ phase + mass + SD + Food*sex, dat)
yr_res <- lm(M90 ~ winter + mass + SD + Food*sex, dat)

#list models and provide names
mods <- list(yr, cycle, predation, competition, resource, pred_res, comp_res, cycle_res, yr_res)
names <- c("year", "cycle", "predation", "competition", "resource", 
           "predator and resource", "competition and resource", "cycle and resource", "year and resource")

#create AIC table on list of models
AIC<-as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
AIC <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits

#which models are less than 2 delta AIC? 
topmods <- AIC[Delta_AICc < 2, return((Modnames))]


#apply the lm_out function to the top to same list of models as in AIC
outall <- lapply(mods, lm_out)
outall <- rbindlist(outall, fill = TRUE)
outall$Model <- names
outall[, `(Intercept)` := NULL]



#function to swap out specific words in column names for new ones
nameswap <- function(old, new, Data) {
  older<-colnames(Data)[grep(old, colnames(Data))]
  newer <- gsub(old, new, older)
  setnames(Data, older, newer)
}

#change names of columns in output table
nameswap("Food1", "Food add", outall)
nameswap("winterday", "Day", outall)
nameswap("mass", "Body mass", outall)
nameswap("haredensity", "Hare density", outall)
nameswap("ppratio", "Lynx:hare ratio", outall)
nameswap("rsq", "R2", outall)
