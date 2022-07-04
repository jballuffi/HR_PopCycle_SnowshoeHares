#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

DT <- DT[!HRninety > 20]



# 90% MCP  -------------------------------------------------------

(byyear90 <- 
  ggplot(DT)+
  geom_boxplot(aes(x = winter, y = HRninety), outlier.shape = NA)+
  geom_jitter(aes(x = winter, y = HRninety), alpha = .7, width = .3)+
  labs(y = "90% MCP area (ha)", x = "Winter")+
  theme_boxplots)

(byhdensity90 <- 
  ggplot(DT)+
  geom_point(aes(x = haredensity/10000, y = HRninety))+
  labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
  theme_densities)

(byppratio90 <- 
  ggplot(DT)+
  geom_point(aes(x = ppratio, y = HRninety))+
  labs(y = "90% MCP area (ha)", x = "Lynx:Hare Ratio")+
  theme_densities)

(hrsize90 <- ggarrange(byyear90, byhdensity90, byppratio90, ncol = 1, nrow = 3))




# 50% MCP -----------------------------------------------------------------


(byyear50 <- 
    ggplot(DT)+
    geom_boxplot(aes(x = winter, y = HRfifty), outlier.shape = NA)+
    geom_jitter(aes(x = winter, y = HRfifty), alpha = .7, width = .3)+
    labs(y = "50% MCP area (ha)", x = "Winter")+
    theme_boxplots)

(byhdensity50 <- 
    ggplot(DT)+
    geom_point(aes(x = haredensity/10000, y = HRfifty))+
    labs(y = "50% MCP area (ha)", x = "Hare Density (hares per ha)")+
    theme_densities)

(byppratio50 <- 
    ggplot(DT)+
    geom_point(aes(x = ppratio, y = HRfifty))+
    labs(y = "50% MCP area (ha)", x = "Lynx:Hare Ratio")+
    theme_densities)

(hrsize50 <- ggarrange(byyear50, byhdensity50, byppratio50, ncol = 1, nrow = 3))




ggsave("output/figures/hrsize_90MCP.jpeg", hrsize90, width = 6, height = 8, units = "in")
ggsave("output/figures/hrsize_50MCP.jpeg", hrsize50, width = 6, height = 8, units = "in")
