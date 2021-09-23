library(tidyverse)
library(cowplot)
x <- tibble(x = 1:6, 
            y = 1:6, 
            colors = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"),
            labels = c(
                    "AI/AN | BA/BS | Large Central Metro",
                    "API | | Large Fringe Metro",
                    "Black | HS/GED | Medium Metro",
                    "White, All| | Micropolitan",
                    "White, Hisp| Some college | Non-core",
                    "NHW | | Small metro"
            )
)
p1 <- ggplot(x, aes(x, y, color = labels)) + 
        geom_point() +
        scale_color_manual(values = x$colors)
p1_legend <- cowplot::get_legend(p1)
ggdraw(p1_legend)
