
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
densities <- readRDS("output/results/densities.rds")



# plot showing animal densities over time ----------------------------------

#pull means by year
wintermeans <- densities[, .(mean(haredensity), mean(lynxdensity), mean(ppratio), phase), by = winter]
names(wintermeans) <- c("winter", "haredensity", "lynxdensity", "ppratio", "phase")


#hare density over time
(h <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = haredensity, group = 1))+
  geom_point(aes(x = winter, y = haredensity, color = phase), size = 2)+
  labs(x = "", y = "Hares per 100 km2")+
  theme_densities)

#lynx density over time
(l <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = lynxdensity, group = 1))+
  geom_point(aes(x = winter, y = lynxdensity, color = phase), size = 2)+
  labs(x = "", y = "Lynx per 100 km2")+
  theme_densities)

#pred-prey ratio over time
(pp <- ggplot(wintermeans)+
  geom_path(aes(x = winter, y = ppratio, group = 1))+
  geom_point(aes(x = winter, y = ppratio, color = phase), size = 2)+
  labs(x = "Winter", y = "Lynx:Hare Ratio")+
  theme_densities)

#ggarrange all densities
(densityplots <- ggarrange(h, l, pp, ncol = 1, nrow = 3))



# predation risk over winter ---------------------------------------------------

(ppwinter <-
  ggplot(densities)+
  geom_line(aes(x = winterday, y = ppratio, color = phase, group = winter), linewidth = .8)+
  labs(y = "Lynx:Hare Ratio", x = "Days into winter")+
  theme_densities)




# weights by phase --------------------------------------------------------

ggplot(DT)+
  geom_boxplot(aes(x = winter, y = mass, color = phase))+
  labs(x = "Winter", y = "Body mass (g)")+
  theme_densities






ggsave("output/figures/densities.jpeg", densityplots, width = 6, height = 9, units = "in")
ggsave("output/figures/ppratios.jpeg", ppwinter, width = 6, height = 4, units = "in")
