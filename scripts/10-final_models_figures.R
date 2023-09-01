
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

#rename food categories
dat[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#make a data frame with only control hares. This will be all years
nofood <- dat[Food == "Control"]

#pull out the years with food add
foodyears <- dat[Food == "Food add", unique(winter)]

#make a data frame to only include the winters with food add 
yesfood <- dat[winter %in% foodyears]



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



# models without food add -----------------------------------------------------------

# linear model for mort rate and hare density
NFlinear <- lm(M90 ~ mortrate + haredensity, data = nofood)

# linear mixed model for mort rate and hare density
NFmixed <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood)

#HR by just phase
NFphase <- lm(M90 ~ phase, data = nofood)

#to get line predictions for both variables
effsP_NF <- ggpredict(NFlinear, terms = c("mortrate"))
effsD_NF <- ggpredict(NFlinear, terms = c("haredensity"))

#coefficients for density
NFdcoef <- coef(NFlinear)["haredensity"]
NFdse <- se.coef(NFlinear)["haredensity"]

#coefficients for predation
NFpcoef <- coef(NFlinear)["mortrate"]
NFpse <- se.coef(NFlinear)["mortrate"]



# models with food add ----------------------------------------------------

# linear model for mort rate and hare density
WFlinear <- lm(M90 ~ mortrate*Food + haredensity*Food, data = yesfood)

# linear mixed model for mort rate and hare density
WFmixed <- lmer(M90 ~ mortrate*Food + haredensity*Food + (1|id), data = yesfood)

WFphase <- lm(M90 ~ phase*Food, data = yesfood)

#to get effects for the interactions in the food add model
effsP_WF <- as.data.table(ggpredict(WFlinear, terms = c("mortrate", "Food")))
effsD_WF <- as.data.table(ggpredict(WFlinear, terms = c("haredensity", "Food")))



# Figures with NO FOOD ------------------------------------------------------------------

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
    labs(y = "90% MCP area (ha)", x = "Mortality rate")+
    theme_densities)

    
(hrNOFOOD <- ggarrange(NFdensity, NFmort, 
                      ncol = 1, nrow = 2))




# Figures from food add years  --------------------------------------------

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
    labs(y = "90% MCP area (ha)", x = "Mortality rate")+
    theme_densities)

(hrYESFOOD <- ggarrange(WFdensity, WFmort, ncol = 1, nrow = 2))



# Liner model outputs -----------------------------------------------------

#list models and provide names
mods <- list(NFlinear, WFlinear)
names <- c("Without treatment", "With treatment")


#apply the lm_out function to the top to same list of models as in AIC
Lout <- lapply(mods, lm_out)
Lout <- rbindlist(Lout, fill = TRUE)
Lout$Model <- names


setcolorder(Lout, c("Model", "(Intercept)", "haredensity", "mortrate", "FoodControl", 
                      "FoodControl:haredensity", "mortrate:FoodControl", 
                      "rsq"))

names(Lout) <- c("Model", "Intercept", "Density", "Mortality", "Treatment",
                   "Treatment*Density", "Treatment*Mortality",
                   "rsq")





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

fwrite(Lout, "Output/results/model_outputs.csv")
