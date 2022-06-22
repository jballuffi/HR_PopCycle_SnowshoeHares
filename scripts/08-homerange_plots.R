#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

(byyear <- 
  ggplot(DT)+
  geom_boxplot(aes(x = winter, y = HRninety), outlier.shape = NA)+
  geom_jitter(aes(x = winter, y = HRninety, colour= season), alpha = .5, width = .3)+
  labs(y = "90% MCP area (ha)", x = "Winter")+
  theme_boxplots)


(byhdensity <- 
  ggplot(DT)+
  geom_point(aes(x = haredensity/10000, y = HRninety, colour= season))+
  labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
  theme_densities)

(byppratio <- 
  ggplot(DT)+
  geom_point(aes(x = ppratio, y = HRninety, colour= season))+
  labs(y = "90% MCP area (ha)", x = "Lynx:Hare Ratio")+
  theme_densities)

(hrsize <- ggarrange(byyear, byhdensity, byppratio, ncol = 1, nrow = 3))


ggsave("output/figures/hrsize_withseason.jpeg", hrsize, width = 6, height = 8, units = "in")


#75% MCP plots
(byyear <- 
    ggplot(DT)+
    geom_boxplot(aes(x = winter, y = HR75), outlier.shape = NA)+
    geom_jitter(aes(x = winter, y = HR75, colour= season), alpha = .5, width = .3)+
    labs(y = "75% MCP area (ha)", x = "Winter")+
    theme_boxplots)


(byhdensity <- 
    ggplot(DT)+
    geom_point(aes(x = haredensity/10000, y = HR75, colour= season))+
    labs(y = "75% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)

(byppratio <- 
    ggplot(DT)+
    geom_point(aes(x = ppratio, y = HR75, colour= season))+
    labs(y = "75% MCP area (ha)", x = "Lynx:Hare Ratio")+
    theme_densities)

(hrsize <- ggarrange(byyear, byhdensity, byppratio, ncol = 1, nrow = 3))


ggsave("output/figures/hrsize_75MCP_withseason.jpeg", hrsize, width = 6, height = 8, units = "in")
