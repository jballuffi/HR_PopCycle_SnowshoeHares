
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("data/compileddata.rds")
densities <- readRDS("data/densities.rds")

#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

dat[, winter := factor(winter)]

#rename food categories
dat[Food == 1, Food := "Suppl."][Food == 0, Food := "Control"]

#pull out the years with food add
foodyears <- dat[Food == "Suppl.", unique(winter)]

#make a data frame with only control hares. This will be all years
nofood <- dat[Food == "Control"]

#make a data frame to only include the winters with food add 
yesfood <- dat[winter %in% foodyears]

#classify seasons into shapes
seasonshapes <- c("early" = 19, "late" = 4)

#classify food treatment into colors
foodcols <- c("Suppl." = "red3", "Control" = "grey30")



# summary stats ----------------------------------------------------

mindens <- round(densities[, min(haredensity)], 3)
maxdens <- round(densities[, max(haredensity)], 2)

minhr <- round(dat[, min(M90)], 2)
maxhr <- round(dat[, max(M90)], 2)

#create summary table for supplemental information
sumtable <- dat[order(winter, Food), .(`Mean date` = mean(date), 
                                       `Mean hare density` = round(mean(haredensity), 2),
                                       `N home ranges` = .N,
                                       `N hares total` = length(unique(id))),
                by = .(winter, Food)]


# basic tests and stats ---------------------------------------------------

# test if sex has an effect on home range
HRsex <- anova(lm(M90 ~ Sex, data = nofood))
Psex <- HRsex$`Pr(>F)`[1]
DFsex <- HRsex$`Df`[2]
Fsex <- HRsex$`F value`[1]

#test if female controls differ from female food adds
femyesfood <- yesfood[Sex == 2]
femHR <- anova(lm(M90 ~ Food, data = femyesfood))
femPfood <- femHR$`Pr(>F)`[1]
femDFfood <- femHR$`Df`[2]
femFfood <- femHR$`F value`[1]


#how many fixes in a home range on avg
nfix <- round(dat[, mean(n.fixes)], 0)
#min fix
minfix <- round(dat[, min(n.fixes)], 0)
#max fix
maxfix <- round(dat[, max(n.fixes)], 0)



# Base control model -----------------------------------------------------------

# linear mixed model for mort rate and hare density
controlNF <- lmer(M90 ~ haredensity + (1|id), data = nofood)

#to get line predictions for both variables
effs_NF <- as.data.table(ggpredict(controlNF, terms = c("haredensity")))

#coefficients for density
NFdcoef <- fixef(controlNF)["haredensity"]
NFdse <- se.fixef(controlNF)["haredensity"]

(NFplot <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90), alpha = 0.6, data = nofood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .3, data = effs_NF)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_NF)+
    labs(y = "90% MCP area (ha)", x = "Hare density (hares/ha)")+
    ylim(0, maxhr)+
    themethesisright)



# makes models --------------------------------------------------------

# all models use the food add data set (one less year)
# linear mixed model that interacts hare density with other terms

null <- lmer(M90 ~ (1|id), data = yesfood)
controlYF <- lmer(M90 ~ haredensity + (1|id), data = yesfood)
season <- lmer(M90 ~  haredensity*season + (1|id), data = yesfood)
food <- lmer(M90 ~ haredensity*Food + (1|id), data = yesfood)
foodseason <- lmer(M90 ~ haredensity*Food*season + (1|id), data = yesfood)
foodtest <- lmer(M90 ~ Food + (1|id), data = yesfood)
seasontest <- lmer(M90 ~ season +(1|id), data = yesfood)



# collect model coefficients ----------------------------------------------

#list models and provide names
mods <- list(null, controlYF, season, food, foodseason, foodtest, seasontest)
names <- c("Null", "Control", "Season", "Treatment", "Season-treatment", "Treatment test", "Season test")

#apply the lm_out function to the top to same list of models as in AIC
Mout <- lapply(mods, lmer_out)
Mout <- rbindlist(Mout, fill = TRUE)
Mout$Model <- names


setcolorder(Mout, c("Model", "(Intercept)", "haredensity", "seasonlate", "FoodControl", 
                    "haredensity:seasonlate", "haredensity:FoodControl", "FoodControl:seasonlate", "haredensity:FoodControl:seasonlate", 
                    "R2m", "R2c"))

names(Mout) <- c("Model", "Intercept", "Density", "Season", "Food",
                 "Season*Density", "Food*Density", "Food*Season", "Food*Season*Density",
                 "R2m", "R2c")



# run AIC comparison -------------------------------------------------------------

#run AIC function on same list of models as above
AIC <- as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]

#round whole table to 3 decimal places
AIC <- AIC %>% mutate_if(is.numeric, round, digits=3)

#rename
setnames(AIC, "Modnames", "Model")

#grab just AIC delta
AICd <- AIC[, .(Model, Delta_AICc)]



# merge AIC and model outputs ---------------------------------------------

Mout <- merge(Mout, AICd, by = "Model")

#order by delta AIC
Mout <- Mout[order(Delta_AICc)]



# Multi-panel figure -------------------------------------------


#to get effects for the interactions in the food add model
effs_WS <- as.data.table(ggpredict(season, terms = c("haredensity", "season")))

#coefficients for density
WScoef <- fixef(season)["haredensity"]
WSse <- se.fixef(season)["haredensity"]

#season model
(WSplot <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, shape = season), alpha = 0.6, data = yesfood[!is.na(season)])+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), alpha = .2, data = effs_WS)+
    geom_line(aes(x = x, y = predicted, group = group, linetype = group), linewidth = 1, data = effs_WS)+
    scale_shape_manual(values = seasonshapes, name = "Season")+
    scale_linetype(name = "Season")+
    labs(y = "90% MCP area (ha)", x = " ", title = "A) Season")+
    ylim(0, maxhr)+
    themethesisright)


#to get effects for the interactions in the food add model
effs_WF <- as.data.table(ggpredict(food, terms = c("haredensity", "Food")))

#coefficients for density
WFcoef <- fixef(food)["haredensity"]
WFse <- se.fixef(food)["haredensity"]


#Food treatment model
(WFplot <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), alpha = 0.6, data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), alpha = .3, data = effs_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group), size = 1, data = effs_WF)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    labs(y = "90% MCP area (ha)", x = " ", title = "B) Food treatment")+
    ylim(0, maxhr)+
    themethesisright)


#coefficients for density from food-season model
WFSdcoef <- fixef(foodseason)["haredensity"]
WFSdse <- se.fixef(foodseason)["haredensity"]

#to get effects for the interactions in the food add model
effs_WFS <- as.data.table(ggpredict(foodseason, terms = c("haredensity", "Food", "season")))

#combine group and facet into one category
effs_WFS[, Category := paste0(group, " ", facet)]

(WFSplot <- 
    ggplot(effs_WFS)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = Category, fill = group), alpha = .2)+
    geom_line(aes(x = x, y = predicted, group = Category, color = group, linetype = facet), size = 1)+
    scale_color_manual(values = foodcols, name = "Food treatment")+
    scale_fill_manual(values = foodcols, name = "Food treatment")+
    scale_linetype(name = "Season")+
    labs(y = "90% MCP area (ha)", x = "Hare density (hares/ha)", title = "C) Food and season")+
    ylim(0, maxhr)+
    themethesisright)


fullfig <- ggarrange(WSplot, WFplot, WFSplot, ncol = 1, nrow = 3)



# save results ------------------------------------------------------------

ggsave("Output/figures/all_density.jpeg", fullfig, width = 6, height = 10, unit = "in")
ggsave("Output/figures/control_density.jpeg", NFplot, width = 6, height = 4, unit = "in")

fwrite(Mout, "Output/results/model_outputs.csv")

fwrite(sumtable, "Output/results/supplemental_table.csv")

# fwrite(AICYF, "Output/results/AIC_fooddataset.csv")
