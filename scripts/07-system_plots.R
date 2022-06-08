
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("data/densities.rds")



# plot showing animal densities over time ----------------------------------

#rename the season categories for figures
densities[season == "late", season := "late winter"]
densities[season == "early", season := "early winter"]

#hare density over time
(h <- ggplot(densities)+
  geom_line(aes(x = winter, y = haredensity, group = season, color = season))+
  labs(x = "", y = "Hares per 100 km2")+
  theme_densities)

#lynx density over time
(l <- ggplot(densities)+
  geom_path(aes(x = winter, y = lynxdensity, group = 1))+
  labs(x = "", y = "Lynx per 100 km2")+
  theme_densities)

#pred-prey ratio over time
(pp <- ggplot(densities)+
  geom_path(aes(x = winter, y = ppratio, group = season, color = season))+
  labs(x = "Winter", y = "Lynx:Hare Ratio")+
  theme_densities+
  theme(legend.position = "none"))

#ggarrange all densities
(densityplots <- ggarrange(h, l, pp, ncol = 1, nrow = 3))



# body mass plots ---------------------------------------------------------

#rename the season categories for figures
DT[season == "late", season := "late winter"]
DT[season == "early", season := "early winter"]


(seasonmass <- 
  ggplot(DT)+
  geom_boxplot(aes(x = season, y = mass), width = .5)+
  labs(x = "", y = "Body Mass (g)")+
  theme_boxplots)

(yearmass <- 
  ggplot(DT)+
  geom_boxplot(aes(x = winter, y = mass))+
  geom_jitter(aes(x = winter, y = mass), width = .25, alpha = .5)+
  labs(x = "Winter", y = "Body Mass (g)")+
  theme_boxplots)

(bodymassplots <- ggarrange(seasonmass, yearmass, ncol = 1, nrow = 2))


ggsave("output/figures/bodymass.jpeg", bodymassplots, width = 6, height = 8, units = "in")
ggsave("output/figures/densities.jpeg", bodymassplots, width = 6, height = 9, units = "in")
