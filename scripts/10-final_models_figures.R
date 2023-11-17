
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")

#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#convert weight to kg for ease of coefficient rounding
dat[, Weight := Weight/1000]

#rename food categories
dat[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#remove last homeranges by ID
#dat <- dat[lastHR == "no"]

#pull out the years with food add
foodyears <- dat[Food == "Food add", unique(winter)]

#make a data frame with only control hares. This will be all years
nofood <- dat[Food == "Control"]

#make a data frame to only include the winters with food add 
yesfood <- dat[winter %in% foodyears]

# #make a data frame for no food in early season
# nofood_early <- nofood[season == "early"]
# 
# #make a dataframe for no food late season
# nofood_late <- nofood[season == "late"]
# 
# #make a data fram for with food early season 
# yesfood_early <- yesfood[season == "early"]
# 
# #makes a data frame for with food late season
# yesfood_late <- yesfood[season == "late"]



# covariate correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- nofood[, .(haredensity, mortrate, SD)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# basic tests and stats ---------------------------------------------------

# test if sex has an effect on home range
HRsex <- anova(lm(M90 ~ Sex, data = nofood))
Psex <- HRsex$`Pr(>F)`[1]
DFsex <- HRsex$`Df`[2]
Fsex <- HRsex$`F value`[1]

#how many fixes in a home range on avg
nfix <- dat[, mean(n.fixes)]

#did treatment have a significant effect on home ranges alone
summary(lm(M90 ~ Food, data = yesfood))



# no food combined seasons -----------------------------------------------------------

# linear mixed model for mort rate and hare density
NFmixed <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood)

#to get line predictions for both variables
effsP_NF <- ggpredict(NFmixed, terms = c("mortrate"))
effsD_NF <- ggpredict(NFmixed, terms = c("haredensity"))

#coefficients for density
NFdcoef <- fixef(NFmixed)["haredensity"]
NFdse <- se.fixef(NFmixed)["haredensity"]

#coefficients for predation
NFpcoef <- fixef(NFmixed)["mortrate"]
NFpse <- se.fixef(NFmixed)["mortrate"]


# no food separate seasons -------------------------------------------

# early winter only
NFearly <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood_early)
effsP_NFearly <- ggpredict(NFearly, terms = c("mortrate"))
effsD_NFearly <- ggpredict(NFearly, terms = c("haredensity"))

#late winter only
NFlate <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood_late)
effsP_NFlate <- ggpredict(NFlate, terms = c("mortrate"))
effsD_NFlate <- ggpredict(NFlate, terms = c("haredensity"))



# with food combined seasons ----------------------------------------------------

# linear mixed model for mort rate and hare density
WFmixed <- lmer(M90 ~ mortrate*Food + haredensity*Food + (1|id), data = yesfood)

#to get effects for the interactions in the food add model
effsP_WF <- as.data.table(ggpredict(WFmixed, terms = c("mortrate", "Food")))
effsD_WF <- as.data.table(ggpredict(WFmixed, terms = c("haredensity", "Food")))



# with food separate seasons ----------------------------------------------

WFearly <- lmer(M90 ~ mortrate*Food + haredensity*Food + (1|id), data = yesfood_early)

#to get effects for the interactions in the food add model
effsP_WFearly <- as.data.table(ggpredict(WFearly, terms = c("mortrate", "Food")))
effsD_WFearly <- as.data.table(ggpredict(WFearly, terms = c("haredensity", "Food")))

WFlate <- lmer(M90 ~ mortrate*Food + haredensity*Food + (1|id), data = yesfood_late)

#to get effects for the interactions in the food add model
effsP_WFlate <- as.data.table(ggpredict(WFlate, terms = c("mortrate", "Food")))
effsD_WFlate <- as.data.table(ggpredict(WFlate, terms = c("haredensity", "Food")))



# Figures with NO FOOD combined seasons ------------------------------------------------------------------

(NFdensity <- 
   ggplot()+
   geom_point(aes(x = haredensity, y = M90), data = nofood)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsD_NF)+
   geom_line(aes(x = x, y = predicted), size = 1, data = effsD_NF)+
   labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
   theme_densities)

(NFmort <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90), data = nofood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsP_NF)+
    geom_line(aes(x = x, y = predicted), size = 1, data = effsP_NF)+
    #geom_abline(aes(intercept = NFint, slope = NFp))+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality")+
    theme_densities)

    
(hrNOFOOD <- ggarrange(NFdensity, NFmort, ncol = 1, nrow = 2))



# Figures no food separate seasons  ---------------------------------------

(NFdensity_early <- 
   ggplot()+
   geom_point(aes(x = haredensity, y = M90), data = nofood_early)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsD_NFearly)+
   geom_line(aes(x = x, y = predicted), size = 1, data = effsD_NFearly)+
   labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Early winter")+
   theme_densities)

(NFmort_early <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90), data = nofood_early)+
    #geom_smooth(aes(x = mortrate, y = M90), data = nofood_early, method = "lm")+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsP_NFearly)+
    geom_line(aes(x = x, y = predicted), size = 1, data = effsP_NFearly)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Early winter")+
    theme_densities)

(NFdensity_late <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90), data = nofood_late)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsD_NFlate)+
    geom_line(aes(x = x, y = predicted), size = 1, data = effsD_NFlate)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Late winter")+
    theme_densities)

(NFmort_late <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90), data = nofood_late)+
    #geom_smooth(aes(x = mortrate, y = M90), data = nofood_early, method = "lm")+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effsP_NFlate)+
    geom_line(aes(x = x, y = predicted), size = 1, data = effsP_NFlate)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Late winter")+
    theme_densities)

NOFOODseason <- ggarrange(NFdensity_early, NFdensity_late, NFmort_early, NFmort_late, ncol = 2, nrow = 2)



# Figures with food combined seasons --------------------------------------------

foodcols <- c("Food add" = "red3", "Control" = "grey30")

(WFdensity <- 
   ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
              colour = "grey80", alpha = .3, data = effsD_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsD_WF)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)


(WFmort <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
            colour = "grey80", alpha = .3, data = effsP_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
            size = 1, data = effsP_WF)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality")+
    theme_densities)

(hrYESFOOD <- ggarrange(WFdensity, WFmort, ncol = 1, nrow = 2))



# figures with food separate seasons --------------------------------------

(WFdensity_early <- 
   ggplot()+
   geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood_early)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
               colour = "grey80", alpha = .3, data = effsD_WFearly)+
   geom_line(aes(x = x, y = predicted, group = group, color = group),
             size = 1, data = effsD_WFearly)+
   scale_color_manual(values = foodcols, guide = NULL)+
   scale_fill_manual(values = foodcols)+
   labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Early winter")+
   theme_densities)

(WFmort_early <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood_early)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
                colour = "grey80", alpha = .3, data = effsP_WFearly)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsP_WFearly)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Early winter")+
    theme_densities)

(WFdensity_late <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood_late)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
                colour = "grey80", alpha = .3, data = effsD_WFlate)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsD_WFlate)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Late winter")+
    theme_densities)

(WFmort_late <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood_late)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
                colour = "grey80", alpha = .3, data = effsP_WFlate)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsP_WFlate)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Late winter")+
    theme_densities)

YESFOODseason <- ggarrange(WFdensity_early, WFdensity_late, WFmort_early, WFmort_late, ncol = 2, nrow = 2)



 # Create mixed model outputs ----------------------------------------------------

#list models and provide names
mods <- list(NFmixed, WFmixed)
names <- c("Control-only", "With treatment")


#apply the lm_out function to the top to same list of models as in AIC
Mout <- lapply(mods, lmer_out)
Mout <- rbindlist(Mout, fill = TRUE)
Mout$Model <- names


setcolorder(Mout, c("Model", "(Intercept)", "haredensity", "mortrate", "FoodControl", 
                      "FoodControl:haredensity", "mortrate:FoodControl", 
                      "R2m", "R2c"))

names(Mout) <- c("Model", "Intercept", "Density", "Mortality", "Treatment",
                   "Treatment*Density", "Treatment*Mortality",
                   "R2m", "R2c")






# save results ------------------------------------------------------------



ggsave("output/figures/HRnofood.jpeg", hrNOFOOD, width = 6, height = 8, units = "in")

ggsave("output/figures/HRwithfood.jpeg", hrYESFOOD, width = 6, height = 8, units = "in")

ggsave("output/figures/HRnofoodseasons.jpeg", NOFOODseason, width = 10, height = 8, units = "in")

ggsave("output/figures/HRwithfoodseasons.jpeg", YESFOODseason, width = 10, height = 8, unit = "in")

fwrite(Mout, "Output/results/model_outputs.csv")
