
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
forcor <- dat[, .(haredensity, mortrate, Weight, SD, year(date))]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# AIC for analysis across winters -----------------------------------------

#list models

#null

#basic models for variables alone
cycle <- lm(M90 ~ phase, dat)
pred <- lm(M90 ~ mortrate, dat)
comp <- lm(M90 ~ haredensity, dat)

bodysize <- lm(M90 ~ Weight, dat)
snow <- lm(M90 ~ SD, dat)
food <- lm(M90 ~ Food, dat)

#all resources plus demographic data
res <- lm(M90 ~ Weight + SD + Food, dat)
pred_res <- lm(M90 ~ mortrate + Weight + SD + Food, dat)
comp_res <- lm(M90 ~ haredensity + Weight + SD + Food, dat)
cycle_res <- lm(M90 ~ phase + Weight + SD + Food, dat)


#list models and provide names
mods <- list(cycle, pred, comp, bodysize, snow, food, res, pred_res, comp_res, cycle_res)
names <- c("cycle", "predation", "competition", 
           "body size", "snow", "food",
          "resource",  "predator and resource", "competition and resource", "cycle and resource")



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
