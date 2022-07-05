
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!HRninety > 20]



# correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- dat[, .(haredensity, lynxdensity, ppratio, mass, winterday)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)

#winter day correlates with hare density 
#hare density correlates with lynx density



# AIC for analysis across winters -----------------------------------------

#list models
predation <- lm(HRninety ~ ppratio, dat)
competition <- lm(HRninety ~ haredensity, dat)
resource <- lm(HRninety ~ mass + winterday + Food, dat)
pred_res <- lm(HRninety ~ ppratio + mass + winterday + Food, dat )
comp_res <- lm(HRninety ~ haredensity + mass + Food, dat)


mods <- list(predation, competition, resource, pred_res, comp_res)
names <- c("predation", "competition", "resource", "pred_res", "comp_res")
AIC<-as.data.table(aictab(REML=F, cand.set = mods, modnames = names, sort = TRUE))
AIC[,ModelLik:=NULL]
AIC[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)

topmods <- AIC[Delta_AICc < 2, return((Modnames))]


#apply to same list of models as in AIC
OutAll<-lapply(mods, lm_out)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-names
