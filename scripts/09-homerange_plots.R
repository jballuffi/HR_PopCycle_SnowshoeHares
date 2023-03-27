#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

#Need to figure out these outliers!
DT <- DT[!M90 > 20] 

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename some variables for figures
DT[sex == 1, sex := "Male"][sex == 2, sex := "Female"]
DT[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]





ggplot(DT[!is.na(sex)])+
  geom_boxplot(aes(x = sex, y = M90, color = Food))+
  labs(y = "90% MCP area (ha)", x = "Sex")+
  theme_boxplots


# 90% MCP  -------------------------------------------------------

(byyear90 <- 
  ggplot(DT)+
  geom_boxplot(aes(x = winter, y = M90))+
  #geom_jitter(aes(x = winter, y = HRninety), alpha = .7, width = .3)+
  labs(y = "90% MCP area (ha)", x = "Winter")+
  theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))

(byhdensity90 <- 
  ggplot(DT)+
  geom_point(aes(x = haredensity/10000, y = M90))+
  labs(y = "90% MCP area (ha)", x = "Hare Density (hares per ha)")+
  theme_densities)

(byldensity <- 
  ggplot(DT)+
  geom_point(aes(x = lynxdensity, y = M90))+
  labs(y = "90% MCP area (ha)", x = "Lynx Density (Lynx per ha)")+
  theme_densities)

(byphase <- 
  ggplot(DT)+
  geom_boxplot(aes(x = phase, y = M90))+
  labs(y = "90% MCP area (ha)", x = "Cycle Phase")+
  theme_boxplots)

(hrsize90 <- ggarrange(byyear90, byhdensity90, byldensity, byphase,
                       ncol = 2, nrow = 2))


ggsave("output/figures/hrsize_90MCP.jpeg", hrsize90, width = 10, height = 6, units = "in")



