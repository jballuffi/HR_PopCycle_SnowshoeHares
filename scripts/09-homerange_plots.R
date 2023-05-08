#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# preparations ------------------------------------------------------------

#read in data
DT <- readRDS("output/results/compileddata.rds")
DT[, Food := as.factor(Food)]

#Need to figure out these outliers! -Liam looked into them, summary in email
incl.out <- copy(DT)
DT <- DT[!M90 > 20] 

#reorder phase cycles
DT[, phase := factor(phase, levels = c("increase", "peak", "decrease", "low"))]

#rename some variables for figures
DT[Sex == 1, Sex := "Male"][Sex == 2, Sex := "Female"]
DT[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", decrease = "orange", low = "red3")



# By year only ------------------------------------------------------------

(byyear <- 
   ggplot(DT)+
   geom_boxplot(aes(x = winter, y = M90, color = phase))+
   labs(y = "90% MCP area (ha)", x = "Winter")+
   scale_color_manual(values = cols)+
   theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))



# Home range in response to resource parameters --------------------------------------------------------

(byfood <-
   ggplot(DT[!is.na(Sex)])+
   geom_boxplot(aes(x = Sex, y = M90, color = Food))+
   labs(y = "90% MCP area (ha)", x = "Sex")+
   theme_boxplots)

(bysnow <- 
  ggplot(DT)+
  geom_point(aes(x = SD, y = M90, color = phase))+
  labs(y = "90% MCP area (ha)", x = "Snow depth (cm)")+
  theme_densities)

(bymass <- 
  ggplot(DT)+
  geom_point(aes(x = Weight, y = M90, color = phase))+
  labs(y = "90% MCP area (ha)", x = "Body mass (g)")+
  theme_densities)

(hrresource <- ggarrange(byfood, bysnow, bymass, ncol = 1, nrow = 3))


# Distribution of home range sizes by year
(hr_distrn <- ggplot(incl.out, aes(x = M90)) + 
    geom_histogram(binwidth=1, color="black", fill="grey") +
    labs(x="90% MCP area (Ha) - bins are 1 Ha each", y="Count") + 
    facet_wrap(~phase))
    





# save figures ------------------------------------------------------------


ggsave("output/figures/HRbyresource.jpeg", hrresource, width = 5, height = 8, units = "in")

ggsave("Output/figures/HRbyyear.jpeg", byyear, width = 6, height = 4, units = "in")

ggsave("Output/figures/HR_distribution.jpeg", hr_distrn, width = 11, height = 8.5, units = "in")

