
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!M90 > 20]
dat <- dat[!is.na(SD)]

#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#convert weight to kg for ease of coefficient rounding
dat[, Weight := Weight/1000]

#read in snow for markdown doc
snow <- readRDS("data/snowgrids.rds")
#remove winter with no collar data from snow data
snow <- snow[!winter == "2014-2015" & !winter == "2021-2022"]


# home range covariate correlation test -----------------------------------

cor(dat$M90, dat$M75)
cor(dat$M75, dat$M50)
cor(dat$M50, dat$M90)

# covariate correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- dat[, .(haredensity, mortrate, Weight, SD)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# AIC for analysis across winters -----------------------------------------

#I am thinking we run three models with all variables, split by the demographic variable
#this tests each prediction across all winters, shows which is the most explanatory 
# not an AIC

#build models
phase_res <- lm(M90 ~ phase + Weight*Food + SD, dat)
pred_res <- lm(M90 ~ mortrate + Weight*Food + SD, dat)
comp_res <- lm(M90 ~ haredensity + Weight*Food + SD, dat)
null <- lm(M90 ~ 1, dat)

#list models and provide names
mods <- list(phase_res, pred_res, comp_res, null)
names <- c("Phase and resource", "Predation and resource", "Competition and resource", "Null")

#create AIC table on list of models
AIC<-as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
AICcycle <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits

#which models are less than 2 delta AIC? 
topmods <- AICcycle[Delta_AICc < 2, return((Modnames))]





# Table with coef outputs -------------------------------------------------


#apply the lm_out function to the top to same list of models as in AIC
outall <- lapply(mods, lm_out)
outall <- rbindlist(outall, fill = TRUE)
outall$Model <- names
outall[, `(Intercept)` := NULL]

outall <- outall[!Model == "Null"]
outall[, V1 := NULL]



setcolorder(outall, c("Model", "phasepeak", "phasedecrease", "phaselow", "haredensity", "mortrate",
                      "Weight", "Food1", "Weight:Food1", "SD", "rsq"))

names(outall) <- c("Model", "Phase peak", "Phase decrease", "Phase low", "Hare density", "Mortality rate", 
                   "Weight", "Food add", "Weight*Food", "Snow depth", "R2")



# Figure ------------------------------------------------------------------

#these are showing trends from the comp_res models
d <- coef(comp_res)["haredensity"]
int_d <- coef(comp_res)["(Intercept)"]
sd_d <- coef(comp_res)["SD"]

#trends from the pred_res model
p <- coef(pred_res)["mortrate"]
int_p <- coef(pred_res)["(Intercept)"]

(byhdensity <- 
   ggplot(dat)+
   geom_point(aes(x = haredensity, y = M90))+
   geom_abline(aes(intercept = int_d, slope = d))+ 
   labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
   theme_densities)

(bysnow <- 
    ggplot(dat)+
    geom_point(aes(x = SD, y = M90))+
    geom_abline(aes(intercept = int_d, slope = sd_d))+
    labs(y = "", x = "Snow depth (cm)")+
    theme_densities)

(bymort <- 
    ggplot(dat)+
    geom_point(aes(x = mortrate, y = M90))+
    geom_abline(aes(intercept = int_p, slope = p))+
    labs(y = "90% MCP area (ha)", x = "Mortality rate")+
    theme_densities)

(bycycle <- 
    ggplot(dat)+
    geom_boxplot(aes(x = phase, y = M90))+
    labs(y = "", x = "Cycle phase")+
    theme_boxplots)
    
(hrcrosscycle <- ggarrange(byhdensity, bysnow, bymort, bycycle,
                      ncol = 2, nrow = 2))




# save results ------------------------------------------------------------



ggsave("output/figures/HRcrosscycle.jpeg", hrcrosscycle, width = 12, height = 11, units = "in")

fwrite(AIC, "Output/results/AIC_cross_cycle.csv")

fwrite(outall, "Output/results/models_cross_cycle.csv")
