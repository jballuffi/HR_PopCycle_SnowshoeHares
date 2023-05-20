
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

#make a dataframe with only control hares. This will be all years
nofood <- dat[Food == "Control"]

#pull out the years with food add
foodyears <- dat[Food == "Food add", unique(winter)]

#make a dataframe to only include the winters with food add 
yesfood <- dat[winter %in% foodyears]




# covariate correlation test --------------------------------------------------------

#subset data to only variables that we need to test co linearity on (numeric only)
forcor <- nofood[, .(haredensity, mortrate, weekwinterday)]

#run correlation, look at matrix style output
round(cor(forcor, use = "complete.obs"), digits = 2)



# Create models -----------------------------------------------------------

# NO FOOD mixed model for mort rate and hare density
NFmixed <- lmer(M90 ~ mortrate + haredensity + (1|id), data = nofood)

# NO FOOD linear model for mort rate and hare density
NFlinear <- lm(M90 ~ mortrate + haredensity , data = nofood)

# YES FOOD linear model for mort rate and hare density
WFlinear <- lm(M90 ~ mortrate*Food + haredensity*Food , data = yesfood)

#to get effects for the coat colour in the energetics model
effsP <- ggpredict(WFlinear, terms = c("mortrate", "Food"))

effsD <- ggpredict(WFlinear, terms = c("haredensity", "Food"))


# Figures with NO FOOD ------------------------------------------------------------------

#these are coefficients from the No food linear model
NFd <- coef(NFlinear)["haredensity"]
NFp <- coef(NFlinear)["mortrate"]
NFint <- coef(NFlinear)["(Intercept)"]

(NFdensity <- 
   ggplot(nofood)+
   geom_point(aes(x = haredensity, y = M90))+
   labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
   theme_densities)

(NFmort <- 
    ggplot(nofood)+
    geom_point(aes(x = mortrate, y = M90))+
    geom_abline(aes(intercept = NFint, slope = NFp))+
    labs(y = "", x = "Mortality rate")+
    theme_densities)

    
(hrNOFOOD <- ggarrange(NFdensity, NFmort, 
                      ncol = 2, nrow = 1))




# Figures from food add years  --------------------------------------------

foodcols <- c("Food add" = "blue3", "Control" = "grey40")

#these are coefficients from the No food linear model
WFd <- coef(WFlinear)["haredensity"]
WFp <- coef(WFlinear)["mortrate"]
WFint <- coef(WFlinear)["(Intercept)"]


(WFdensity <- 
   ggplot()+
    geom_point(aes(x = haredensity, y = M90, color = Food), data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
              colour = "grey80", alpha = .3, data = effsD)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
              size = 1, data = effsD)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols, guide = NULL)+
    labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)


(WFmort <- 
    ggplot()+
    geom_point(aes(x = mortrate, y = M90, color = Food), data = yesfood)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group),
            colour = "grey80", alpha = .3, data = effsP)+
    geom_line(aes(x = x, y = predicted, group = group, color = group),
            size = 1, data = effsP)+
    scale_color_manual(values = foodcols, guide = NULL)+
    scale_fill_manual(values = foodcols)+
    labs(y = "", x = "Mortality rate")+
    theme_densities)

(hrYESFOOD <- ggarrange(WFdensity, WFmort, ncol = 2, nrow = 1))



# Create model outputs ----------------------------------------------------

#list models and provide names
mods <- list(NFlinear, WFlinear)
names <- c("Without treatment", "With treatment")


#apply the lm_out function to the top to same list of models as in AIC
outall <- lapply(mods, lm_out)
outall <- rbindlist(outall, fill = TRUE)
outall$Model <- names
outall[, `(Intercept)` := NULL]

outall <- outall[!Model == "Null"]



setcolorder(outall, c("Model", "haredensity", "mortrate", "FoodControl", 
                      "FoodControl:haredensity", "mortrate:FoodControl", 
                      "rsq"))

names(outall) <- c("Model", "Density", "Mortality", "Treatment",
                   "Treatment*Density", "Treatment*Mortality",
                   "R2")






# save results ------------------------------------------------------------



ggsave("output/figures/HRnofood.jpeg", hrNOFOOD, width = 12, height = 6, units = "in")

ggsave("output/figures/HRwithfood.jpeg", hrYESFOOD, width = 12, height = 6, units = "in")


fwrite(outall, "Output/results/model_outputs.csv")
