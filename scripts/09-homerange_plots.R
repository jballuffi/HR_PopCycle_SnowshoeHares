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

#rename sex categories
DT[Sex == 1, Sex := "Male"][Sex == 2, Sex := "Female"]
#rename food categories
DT[Food == 1, Food := "Food add"][Food == 0, Food := "Control"]

#create 2nd data frame that has no food adds
DTnofood <- DT[!Food == "Food add"]

#set colors for cycle phases
cols <- c("increase" = "purple", "peak" = "green4", decrease = "orange", low = "red3")


# By year and phase for non-food add data ------------------------------------------------------------

(byyear <- 
   ggplot(DTnofood)+
   geom_boxplot(aes(x = winter, y = M90, color = phase))+
   labs(y = "90% MCP area (ha)", x = "Winter")+
   scale_color_manual(values = cols)+
   theme_boxplots+
   theme(axis.text.x.bottom = element_text(size = 8)))

#haven't saved this yet but it's an option
(byphase <- 
    ggplot(DTnofood)+
    geom_boxplot(aes(x = phase, y = M90))+
    labs(y = "", x = "Cycle phase")+
    theme_boxplots)



ggsave("Output/figures/HRbyyear.jpeg", byyear, width = 7, height = 6, units = "in")

