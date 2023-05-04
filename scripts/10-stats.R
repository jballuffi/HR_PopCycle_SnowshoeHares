
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
forcor <- dat[, .(haredensity, mortrate, Weight, SD)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# AIC for analysis across winters -----------------------------------------

#I am thinking we run three models with all variables, split by the demographic variable
#this tests each prediction across all winters, shows which is the most explanatory 
# not an AIC

#list models
cycle <- lm(M90 ~ phase + Weight + SD + Food, dat)
pred <- lm(M90 ~ mortrate + Weight + SD + Food, dat)
comp <- lm(M90 ~ haredensity + Weight + SD + Food, dat)
null <- lm(M90 ~ 1, dat)

#list models and provide names
mods <- list(cycle, pred, comp, null)
names <- c("Cycle", "Predation", "Competition", "Null")

#create AIC table on list of models
AIC<-as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
AIC <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits

#which models are less than 2 delta AIC? 
topmods <- AIC[Delta_AICc < 2, return((Modnames))]




# Table with coef outputs -------------------------------------------------


#apply the lm_out function to the top to same list of models as in AIC
outall <- lapply(mods, lm_out)
outall <- rbindlist(outall, fill = TRUE)
outall$Model <- names
outall[, `(Intercept)` := NULL]

outall <- outall[!Model == "Null"]
outall[, V1 := NULL]


setcolorder(outall, c("Model", "phasepeak", "phasedecrease", "phaselow", "haredensity", "mortrate", "Weight", "Food1", "SD", "rsq"))
names(outall) <- c("Model", "Phase peak", "Phase decrease", "Phase low", "Hare density", "Mortality rate", 
                   "Weight", "Food add", "Snow depth", "R2")


