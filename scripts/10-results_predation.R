
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")

#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename food categories
dat[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#pull out the years with food add
foodyears <- dat[Food == "Food add", unique(winter)]

#make a data frame with only control hares. This will be all years
nofood <- dat[Food == "Control"]

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
ggplot(yesfood)+geom_boxplot(aes(x = Food, y = M90))



# no food no seasons -----------------------------------------------------------

# linear mixed model for mort rate and hare density
NF <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood)

#to get line predictions for both variables
effsP_NF <- ggpredict(NF, terms = c("mortrate"))
effsD_NF <- ggpredict(NF, terms = c("haredensity"))

#coefficients for density
NFdcoef <- fixef(NF)["haredensity"]
NFdse <- se.fixef(NF)["haredensity"]

#coefficients for predation
NFpcoef <- fixef(NF)["mortrate"]
NFpse <- se.fixef(NF)["mortrate"]


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


(hrCONTROL <- ggarrange(NFdensity, NFmort, ncol = 1, nrow = 2))



# no food with seasons -------------------------------------------

#classify seasons into shapes
seasonshapes <- c("early" = 4, "late" = 19)

# linear mixed model for mort rate and hare density
WS <- lmer(M90 ~ mortrate*season + haredensity*season + (1|id), data = nofood[!is.na(season)])

#to get effects for the interactions in the food add model
effsP_WS <- as.data.table(ggpredict(WS, terms = c("mortrate", "season")))
effsD_WS <- as.data.table(ggpredict(WS, terms = c("haredensity", "season")))

(WSdensity <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, shape = season), data = nofood[!is.na(season)])+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), colour = "grey80", alpha = .2, data = effsD_WS)+
    geom_line(aes(x = x, y = predicted, group = group, linetype = group), size = 1, data = effsD_WS)+
    scale_shape_manual(values = seasonshapes)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)

(WSmort <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90, shape = season), data = nofood[!is.na(season)])+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), colour = "grey80", alpha = .2, data = effsP_WS)+
    geom_line(aes(x = x, y = predicted, group = group, linetype = group), size = 1, data = effsP_WS)+
    scale_shape_manual(values = seasonshapes)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality")+
    theme_densities)

(hrSEASON <- ggarrange(WSdensity, WSmort, ncol = 1, nrow = 2))


# with food no seasons ----------------------------------------------------

# linear mixed model for mort rate and hare density
WF <- lmer(M90 ~ mortrate*Food + haredensity*Food + (1|id), data = yesfood)

#to get effects for the interactions in the food add model
effsP_WF <- as.data.table(ggpredict(WF, terms = c("mortrate", "Food")))
effsD_WF <- as.data.table(ggpredict(WF, terms = c("haredensity", "Food")))


foodcols <- c("Food add" = "red3", "Control" = "grey30")

(WFdensity <- 
    ggplot()+
    #geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood)+
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
    #geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
                colour = "grey80", alpha = .3, data = effsP_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsP_WF)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality")+
    theme_densities)

(hrFOOD <- ggarrange(WFdensity, WFmort, ncol = 1, nrow = 2))



# with food and seasons ----------------------------------------------


#three way interaction between food and season
WFS <- lmer(M90 ~ mortrate*Food*season + haredensity*Food*season + (1|id), data = yesfood)

#to get effects for the interactions in the food add model
effsP_WFS <- as.data.table(ggpredict(WFS, terms = c("mortrate", "Food", "season")))
effsD_WFS <- as.data.table(ggpredict(WFS, terms = c("haredensity", "Food", "season")))

#combine group and facet into one category
effsP_WFS[, Category := paste0(group, " ", facet)]
effsD_WFS[, Category := paste0(group, " ", facet)]


(WFSdensity <- 
    ggplot(effsD_WFS)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = Category, fill = group), alpha = .2)+
    geom_line(aes(x = x, y = predicted, group = Category, color = group, linetype = facet), size = 1)+
    scale_color_manual(values = foodcols)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)

(WFSmort <- 
    ggplot(effsP_WFS)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = Category, fill = group), alpha = .2)+
    geom_line(aes(x = x, y = predicted, group = Category, color = group, linetype = facet), size = 1)+
    scale_color_manual(values = foodcols)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Probability of mortality")+
    theme_densities)

hrFOODSEASON <- ggarrange(WFSdensity, WFSmort, ncol = 1, nrow = 2)



# figures with food separate seasons --------------------------------------
# 
# (WFdensity_early <- 
#    ggplot()+
#    geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood_early)+
#    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
#                colour = "grey80", alpha = .3, data = effsD_WFearly)+
#    geom_line(aes(x = x, y = predicted, group = group, color = group),
#              size = 1, data = effsD_WFearly)+
#    scale_color_manual(values = foodcols, guide = NULL)+
#    scale_fill_manual(values = foodcols)+
#    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Early winter")+
#    theme_densities)
# 
# (WFmort_early <- 
#     ggplot()+
#     geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood_early)+
#     geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
#                 colour = "grey80", alpha = .3, data = effsP_WFearly)+
#     geom_line(aes(x = x, y = predicted, group = group, color = group),
#               size = 1, data = effsP_WFearly)+
#     scale_color_manual(values = foodcols, guide = NULL)+
#     scale_fill_manual(values = foodcols)+
#     labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Early winter")+
#     theme_densities)
# 
# (WFdensity_late <- 
#     ggplot()+
#     geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood_late)+
#     geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
#                 colour = "grey80", alpha = .3, data = effsD_WFlate)+
#     geom_line(aes(x = x, y = predicted, group = group, color = group),
#               size = 1, data = effsD_WFlate)+
#     scale_color_manual(values = foodcols, guide = NULL)+
#     scale_fill_manual(values = foodcols)+
#     labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)", title = "Late winter")+
#     theme_densities)
# 
# (WFmort_late <- 
#     ggplot()+
#     geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood_late)+
#     geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
#                 colour = "grey80", alpha = .3, data = effsP_WFlate)+
#     geom_line(aes(x = x, y = predicted, group = group, color = group),
#               size = 1, data = effsP_WFlate)+
#     scale_color_manual(values = foodcols, guide = NULL)+
#     scale_fill_manual(values = foodcols)+
#     labs(y = "90% MCP area (ha)", x = "Probability of mortality", title = "Late winter")+
#     theme_densities)
# 
# Fodbyseason <- ggarrange(WFdensity_early, WFdensity_late, WFmort_early, WFmort_late, ncol = 2, nrow = 2)
# 


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



ggsave("output/figures/HRnofood.jpeg", hrCONTROL, width = 6, height = 8, units = "in")

ggsave("output/figures/HRwithfood.jpeg", hrFOOD, width = 6, height = 8, units = "in")

ggsave("output/figures/HRnofoodseasons.jpeg", hrSEASON, width = 6, height = 8, units = "in")

ggsave("output/figures/HRwithfoodseasons.jpeg", hrFOODSEASON, width = 6, height = 8, unit = "in")

fwrite(Mout, "Output/results/model_outputs.csv")
