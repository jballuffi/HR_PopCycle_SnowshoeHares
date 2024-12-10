library(ggplot2)

theme_densities <- theme(axis.title = element_text(size=12),
                         axis.text.y = element_text(size=10),
                         axis.text.x = element_text(size = 9),
                         legend.key = element_blank(),
                         legend.title = element_blank(),
                         panel.background = element_blank(),
                         axis.line.x.top = element_blank(),
                         axis.line.y.right = element_blank(),
                         axis.line.x.bottom = element_line(size=.5),
                         axis.line.y.left = element_line(size=.5),
                         panel.border = element_blank(),
                         panel.grid.major = element_line(size = 0.5, color = "grey90")
                          )



theme_boxplots <- theme(axis.title = element_text(size=12),
                         axis.text = element_text(size=10),
                         legend.position = "right",
                         legend.key = element_blank(),
                         legend.title = element_blank(),
                         panel.background = element_blank(),
                         axis.line.x.top = element_blank(),
                         axis.line.y.right = element_blank(),
                         axis.line.x.bottom = element_line(size=.5),
                         axis.line.y.left = element_line(size=.5),
                         panel.border = element_blank(),
                         panel.grid.major = element_line(size = 0.5, color = "grey90"),
                         panel.grid.major.x = element_blank()
)

themepoints <- theme(text = element_text(family = "serif"),
                     title = element_text(family = "serif"),
                     axis.title = element_text(size=13, family = "serif"),
                     axis.text = element_text(size=10, family = "serif"),
                     axis.text.x.bottom = element_text(size = 8),
                     legend.position = "right",
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     strip.background = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(size=.5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))
