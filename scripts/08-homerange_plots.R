#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

#Need to figure out these outliers!
DT <- DT[!HRninety > 20] 

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

# 90% MCP  -------------------------------------------------------

(byyear90 <- 
  ggplot(DT)+
  geom_boxplot(aes(x = winter, y = HRninety))+
  #geom_jitter(aes(x = winter, y = HRninety), alpha = .7, width = .3)+
  labs(y = "90% MCP area (ha)", x = "Winter")+
  theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))

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

(byphase <- 
  ggplot(DT)+
  geom_boxplot(aes(x = phase, y = HRninety))+
  labs(y = "90% MCP area (ha)", x = "Cycle Phase")+
  theme_boxplots)

(hrsize90 <- ggarrange(byyear90, byhdensity90, byppratio90, byphase,
                       ncol = 2, nrow = 2))


ggsave("output/figures/hrsize_90MCP.jpeg", hrsize90, width = 10, height = 6, units = "in")
