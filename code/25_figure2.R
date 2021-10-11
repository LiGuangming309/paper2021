#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  #summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  scenarioI <- "A"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist()
rm(file_list)

theme_set(theme_classic())
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI)

## -- figure 3, attributable burden---
#TODO method di_gee/burnett
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "proportion of disparity to Black or African American attributable" & rural_urban_class == "All"
& method == "di_gee")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "proportion of disparity to lower educational attainment"  & rural_urban_class == "All"
& method == "di_gee")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education))


attr_burd$measure3 %>% unique
## --set range---
min1 <- min(c(attr_burd1$lower, attr_burd2$lower))
max1 <- max(c(attr_burd1$upper, attr_burd2$upper))

g1 <- g1 + ylim(min1, max1)
g2 <- g2 + ylim(min1, max1)

plots <- list(g1, g2)
rm(min1, max1)
rm(
  attr_burd1, attr_burd2, 
  g1, g2
)
#----formatting------
#group.colors <- c(hue_pal()(6), hue_pal()(3), hue_pal()(3))
group.colors <- hue_pal()(9)
group.colors <- RColorBrewer::brewer.pal(n = 9, name = "Paired")
names(group.colors) <- c("White, Not Hispanic or Latino",
                         "White, Hispanic or Latino",
                         "Black or African American",
                         "White, All Origins",
                         "Asian or Pacific Islander",
                         "American Indian or Alaska Native",
                         
                         "high school graduate or lower",
                         "some college education but no 4-year college degree",
                         "4-year college graduate or higher"
)

plots <- lapply(plots, function(g) {
  g +
    geom_line(size = 1.5) +
    xlab("Year") +
    geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
    scale_colour_manual(values=group.colors) +
    theme(legend.title = element_blank()) +
    guides(color=guide_legend(ncol=3,byrow=TRUE))
})

legend_plot <- get_legend(plots[[1]])
legend_plot <- as_ggplot(legend_plot)

plots <- lapply(plots, function(g) {
  g + theme(legend.position = "none", axis.title.y = element_blank()) 
  
})

## --- arrange plots----
lay <- rbind(
  c(4,NA,5),
  c(NA,NA,NA),
  c(1,NA,2),
  c(7,7,7)
)

t1 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("race", gp = gpar(fontsize = 10, fontface = "bold"))
)

t2 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", gp = gpar(fontsize = 10, fontface = "bold"))
)


gs <- append(plots, list(t1, t2, legend_plot))
#gs <- lapply(1:7, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(figure_width, blank_space, figure_width),
  heights = c(0.1, blank_space, figure_hight, 0.8),
  layout_matrix = lay
)

g_combined

#g_combined <- ggarrange(                                                
#          ggarrange(plotlist = plots,  ncol = 3, labels = c("A","B", "C")), 
#          legend_plot,
#          nrow = 2                                       
#)

# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, "figure2.png"), g_combined, height = 4, width = 8)
