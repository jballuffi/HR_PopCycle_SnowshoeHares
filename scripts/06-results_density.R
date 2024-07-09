
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
densities <- readRDS("output/results/densities.rds")

#reorder phase cycles
dat[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

dat[, winter := factor(winter)]

#rename food categories
dat[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#pull out the years with food add
foodyears <- dat[Food == "Food add", unique(winter)]

#make a data frame with only control hares. This will be all years
nofood <- dat[Food == "Control"]

#make a data frame to only include the winters with food add 
yesfood <- dat[winter %in% foodyears]



# summary stats ----------------------------------------------------

mindens <- round(densities[, min(haredensity)], 3)
maxdens <- round(densities[, max(haredensity)], 2)

minhr <- round(dat[, min(M90)], 2)
maxhr <- round(dat[, max(M90)], 2)

#create summary table for supplemental information
sumdat <- dat[, .(`Mean date` = mean(date), 
                  `Mean hare density` = round(mean(haredensity), 2), 
                  `N home ranges` = .N, 
                  `N hares total` = length(unique(id))),
              winter]

#sumdensities <- densities[, .(`Mean density` = round(mean(haredensity), 2)), winter]

sumfood <- dat[Food == "Food add", .(`N food add hares` = length(unique(id))), winter]

#sumtable <- merge(sumdat, sumdensities, by = "winter")

sumtable <- merge(sumdat, sumfood, by = "winter", all.x = TRUE)



# basic tests and stats ---------------------------------------------------

# test if sex has an effect on home range
HRsex <- anova(lm(M90 ~ Sex, data = nofood))
Psex <- HRsex$`Pr(>F)`[1]
DFsex <- HRsex$`Df`[2]
Fsex <- HRsex$`F value`[1]

#how many fixes in a home range on avg
nfix <- round(dat[, mean(n.fixes)], 0)
#min fix
minfix <- round(dat[, min(n.fixes)], 0)
#max fix
maxfix <- round(dat[, max(n.fixes)], 0)



# no food no seasons -----------------------------------------------------------

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
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_NF)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_NF)+
    labs(y = "90% MCP area (ha)", x = "Hare density (hares/ha)")+
    ylim(0, maxhr)+
    theme_densities)

(NFfortalk <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90), alpha = 0.6, data = nofood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_NF)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_NF)+
    labs(y = "HR area (ha)", x = "Hare density (hares/ha)")+
    ylim(0, maxhr)+
    theme_densities)



# no food with seasons -------------------------------------------

#classify seasons into shapes
seasonshapes <- c("early" = 19, "late" = 4)

# linear mixed model for mort rate and hare density
seasonNF <- lmer(M90 ~  haredensity*season + (1|id), data = nofood)


#to get effects for the interactions in the food add model
effs_WS <- as.data.table(ggpredict(seasonNF, terms = c("haredensity", "season")))

#coefficients for density
WScoef <- fixef(seasonNF)["haredensity"]
WSse <- se.fixef(seasonNF)["haredensity"]

(WSplot <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, shape = season), alpha = 0.6, data = nofood[!is.na(season)])+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), colour = "grey80", alpha = .2, data = effs_WS)+
    geom_line(aes(x = x, y = predicted, group = group, linetype = group), linewidth = 1, data = effs_WS)+
    scale_shape_manual(values = seasonshapes)+
    labs(y = "90% MCP area (ha)", x = " ", title = "A) Season")+
    ylim(0, maxhr)+
    theme_densities)

(seasonfortalk <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, shape = season), alpha = 0.6, data = nofood[!is.na(season)])+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), colour = "grey80", alpha = .2, data = effs_WS)+
    geom_line(aes(x = x, y = predicted, group = group, linetype = group), linewidth = 1, data = effs_WS)+
    scale_shape_manual(values = seasonshapes)+
    labs(y = "HR area (ha)", x = "Hare density (hares/ha)")+
    ylim(0, maxhr)+
    theme_densities)



# with food no seasons ----------------------------------------------------

# linear mixed model for mort rate and hare density
foodYF <- lmer(M90 ~ haredensity*Food + (1|id), data = yesfood)


#to get effects for the interactions in the food add model
effs_WF <- as.data.table(ggpredict(foodYF, terms = c("haredensity", "Food")))

#coefficients for density
WFcoef <- fixef(foodYF)["haredensity"]
WFse <- se.fixef(foodYF)["haredensity"]

foodcols <- c("Food add" = "red3", "Control" = "grey30")

(WFplot <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), alpha = 0.6, data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), colour = "grey80", alpha = .3, data = effs_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group), size = 1, data = effs_WF)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = " ", title = "B) Food treatment")+
    ylim(0, maxhr)+
    theme_densities)

(foodfortalk <- 
    ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), alpha = 0.6, data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), colour = "grey80", alpha = .3, data = effs_WF)+
    geom_line(aes(x = x, y = predicted, group = group, color = group), size = 1, data = effs_WF)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "HR area (ha)", x = "Hare density (hares/ha)")+
    ylim(0, maxhr)+
    theme_densities)



# with food and seasons ----------------------------------------------

#three way interaction between food and season
foodseasonYF <- lmer(M90 ~ haredensity*Food*season + (1|id), data = yesfood)

#coefficients for density
WFSdcoef <- fixef(foodseasonYF)["haredensity"]
WFSdse <- se.fixef(foodseasonYF)["haredensity"]

#to get effects for the interactions in the food add model
effs_WFS <- as.data.table(ggpredict(foodseasonYF, terms = c("haredensity", "Food", "season")))

#combine group and facet into one category
effs_WFS[, Category := paste0(group, " ", facet)]

(WFSplot <- 
    ggplot(effs_WFS)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = Category, fill = group), alpha = .2)+
    geom_line(aes(x = x, y = predicted, group = Category, color = group, linetype = facet), size = 1)+
    scale_color_manual(values = foodcols)+
    scale_fill_manual(values = foodcols)+
    labs(y = "90% MCP area (ha)", x = "Hare density (hares/ha)", title = "C) Food and season")+
    ylim(0, maxhr)+
    theme_densities)



# Combine panels ----------------------------------------------------------

fullfig <- ggarrange(WSplot, WFplot, WFSplot, ncol = 1, nrow = 3)



# Additional models ------------------------------------------------------

#did treatment have a significant effect on home ranges alone
foodtestYF <- lmer(M90 ~ Food + (1|id), data = yesfood)
ggplot(yesfood)+geom_boxplot(aes(x = Food, y = M90))

#did treatment have a significant effect on home ranges alone
seasontestNF <- lmer(M90 ~ season +(1|id), data = nofood)
ggplot(nofood)+geom_boxplot(aes(x = season, y = M90))

#control model for food add data set
controlYF <- lmer(M90 ~ haredensity + (1|id), data = yesfood)

seasonYF <- lmer(M90 ~  haredensity*season + (1|id), data = yesfood)

seasontestYF <- lmer(M90 ~ season +(1|id), data = yesfood)



# Create mixed model outputs ----------------------------------------------------

#list models and provide names
mods <- list(controlNF, seasonNF, foodYF, foodseasonYF, foodtestYF, seasontestNF)
names <- c("Control", "Season", "Treatment", "Season-treatment", "Treatment test", "Season test")

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



# AIC results for models that use the food add dataset -------------------------------------------------------------

modsYF <- list(controlYF, seasonYF, foodYF, foodseasonYF, foodtestYF, seasontestYF)
namesYF <- c("Control", "Season", "Treatment", "Season-treatment", "Food test", "Season test")

AICYF <- as.data.table(aictab(REML = F, cand.set = modsYF, modnames = namesYF, sort = TRUE))
AICYF[, ModelLik := NULL]
AICYF[, Cum.Wt := NULL]
#round whole table to 3 dec places
AICYF <- AICYF %>% mutate_if(is.numeric, round, digits=3)

outYF <- lapply(modsYF, lmer_out)
outYF <- rbindlist(outYF, fill = TRUE)
outYF$Modnames <- namesYF
outYF <- outYF[, .(Modnames, R2m, R2c)]

AICYF <- merge(AICYF, outYF, by = "Modnames")

AICYF <- AICYF[order(Delta_AICc)]



# save results ------------------------------------------------------------

ggsave("Output/figures/all_density.jpeg", fullfig, width = 6, height = 10, unit = "in")
ggsave("Output/figures/control_density.jpeg", NFplot, width = 6, height = 4, unit = "in")

ggsave("Output/figures/control_density_fortalks.jpeg", NFfortalk, width = 6, height = 4, unit = "in")
ggsave("Output/figures/foodadd_density_fortalks.jpeg", foodfortalk, width = 7, height = 4, unit = "in")
ggsave("Output/figures/seasons_density_fortalks.jpeg", seasonfortalk, width = 7, height = 4, unit = "in")

fwrite(Mout, "Output/results/model_outputs.csv")

fwrite(sumtable, "Output/results/supplemental_table.csv")

fwrite(AICYF, "Output/results/AIC_fooddataset.csv")
