
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in data
dat <- readRDS("output/results/compileddata.rds")
dat <- dat[!M90 > 20]
#dat <- dat[!phase == "low"]




# Run AIC by phase of cycle -----------------------------------------------

#make dat into a list by phase, lapply this function? 
makeAIC <- function(hr, p, c, w, s, f){ 
  
  #list models
  pred <- lm(hr ~ p)
  comp <- lm(hr ~ c)
  res <- lm(hr ~ s + w*f)
  pred_res <- lm(hr ~ p + s + w*f)
  comp_res <- lm(hr ~ c + s + w*f)
  null <- lm(hr ~ 1)
  
  #list models and provide names
  mods <- list(pred, comp, res, pred_res, comp_res, null)
  names <- c("Predation", "Competition", "Resource", "Predation+Resource", "Competition+Resource", "Null")
  
  AIC <- as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
  AIC[, ModelLik := NULL]
  AIC[, Cum.Wt := NULL]
  AIC <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits
  
  #which models are less than 2 delta AIC? 
  #topmods <- AIC[Delta_AICc < 2, return(Modnames)]
  
  return(AIC)

  }


#AIC for data from the low, this removed food adds
makeAIC_low <- function(hr, p, c, w, s){ 
  
  #list models
  pred <- lm(hr ~ p)
  comp <- lm(hr ~ c)
  res <- lm(hr ~ s + w)
  pred_res <- lm(hr ~ p + s + w)
  comp_res <- lm(hr ~ c + s + w)
  null <- lm(hr ~ 1)
  
  #list models and provide names
  mods <- list(pred, comp, res, pred_res, comp_res, null)
  names <- c("Predation", "Competition", "Resource", "Predation+Resource", "Competition+Resource", "Null")
  
  AIC <- as.data.table(aictab(REML = F, cand.set = mods, modnames = names, sort = TRUE))
  AIC[, ModelLik := NULL]
  AIC[, Cum.Wt := NULL]
  AIC <- AIC %>% mutate_if(is.numeric, round, digits=3) #round whole table to 3 digits
  
  #which models are less than 2 delta AIC? 
  #topmods <- AIC[Delta_AICc < 2, return(Modnames)]
  
  return(AIC)
  
}

#run first AIC function on all but low
phases <- dat[!phase == "low", makeAIC(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD, f = Food), phase]

#in order to run the models in the low you must remove food add as a factor
low <- as.data.table(dat[phase == "low", makeAIC_low(hr = M90, p = mortrate, c = haredensity, w = Weight, s = SD)])
low[, phase := "low"]

#bin the two tables together for all phases
AIC_by_phase <- rbind(phases, low)



mort <- ggplot(dat)+
  geom_point(aes(y = M90, x = mortrate))+
  #geom_smooth(aes(y = M90, x = mortrate))+
  labs(y = "90% MCP area (ha)", x = "Mortality rate")+
  facet_wrap(~phase, scales = "free")+
  theme_densities

dens <- ggplot(dat)+
  geom_point(aes(y = M90, x = haredensity))+
  #geom_smooth(aes(y = M90, x = haredensity))+
  labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
  facet_wrap(~phase, scales = "free")+
  theme_densities

snow <- ggplot(dat)+
  geom_point(aes(y = M90, x = SD))+
  #geom_smooth(aes(y = M90, x = SD))+
  labs(y = "90% MCP area (ha)", x = "Snow depth (cm)")+
  facet_wrap(~phase, scales = "free")+
  theme_densities

weight <- ggplot(dat)+
  geom_point(aes(y = M90, x = Weight))+
  #geom_smooth(aes(y = M90, x = Weight))+
  labs(y = "90% MCP area (ha)", x = "Weight (g)")+
  facet_wrap(~phase, scales = "free")+
  theme_densities

food <- ggplot(dat)+
  geom_boxplot(aes(y = M90, x = Food))+
  #labs(y = "90% MCP area (ha)", x = "Snow depth (cm)")+
  facet_wrap(~phase, scales = "free")+
  theme_densities







# save things -------------------------------------------------------------

ggsave("output/figures/mortbyphase.jpeg", mort, width = 7, height = 7, units = "in")
ggsave("output/figures/densitybyphase.jpeg", dens, width = 7, height = 7, units = "in")
ggsave("output/figures/snowbyphase.jpeg", snow, width = 7, height = 7, units = "in")
ggsave("output/figures/weightbyphase.jpeg", weight, width = 7, height = 7, units = "in")
ggsave("output/figures/foodbyphase.jpeg", food, width = 7, height = 7, units = "in")


fwrite(AIC_by_phase, "Output/results/AIC_by_phase.csv")
