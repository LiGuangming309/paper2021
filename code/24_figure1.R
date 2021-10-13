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
  "gridExtra", "grid", "lattice", "ggsci"
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
  scenarioI <- "A"
  
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  #summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  
  summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
  figuresDir <- "C:/Users/Daniel/Desktop/paper2021/data/15_figures"
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
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "value" & rural_urban_class == "All"
& method == "di_gee")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity))

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All"
                                   & method == "di_gee") #burnett 
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education))

attr_burd3 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value"
& rural_urban_class != "All" & Year >= 2000 & method == "di_gee")
g3 <- ggplot(attr_burd3, aes(x = Year, y = mean, color = rural_urban_class))

attr_burd4 <- attr_burd %>% filter(agr_by == "nation" & method == "di_gee" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "prop. of overall burden" & rural_urban_class == "All")
g4 <- ggplot(attr_burd4, aes(x = Year, y = mean, color = Ethnicity))

attr_burd5 <- attr_burd %>% filter(agr_by == "nation" & method == "di_gee" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "prop. of overall burden" & rural_urban_class == "All")
g5 <- ggplot(attr_burd5, aes(x = Year, y = mean, color = Education))

attr_burd6 <- attr_burd %>% filter(agr_by == "nation" & method == "di_gee" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "prop. of overall burden" 
                                   & rural_urban_class != "All" & Year >= 2000)
g6 <- ggplot(attr_burd6, aes(x = Year, y = mean, color = rural_urban_class))


## --set range---
min1 <- min(c(attr_burd1$lower, attr_burd2$lower, attr_burd3$lower))
min2 <- min(c(attr_burd4$lower, attr_burd5$lower, attr_burd6$lower))

max1 <- max(c(attr_burd1$upper, attr_burd2$upper, attr_burd3$upper))
max2 <- max(c(attr_burd4$upper, attr_burd5$upper, attr_burd6$upper))

  g1 <- g1 + ylim(min1, max1)
  g2 <- g2 + ylim(min1, max1)
  g3 <- g3 + ylim(min1, max1)
  g4 <- g4 + ylim(min2, max2)
  g5 <- g5 + ylim(min2, max2)
  g6 <- g6 + ylim(min2, max2)

#g6 <- g6 + scale_y_continuous(breaks= pretty_breaks())

plots <- list(g1, g2, g3, g4, g5, g6)
rm(min1, min2, max1, max2)
rm(
  attr_burd1, attr_burd2, attr_burd3, attr_burd4, attr_burd5, attr_burd6,
  g1, g2, g3, g4, g5, g6
)
#----formatting------
#group.colors <- c(hue_pal()(6), hue_pal()(3), hue_pal()(3))
group.colors <- hue_pal()(12)
group.colors <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
names(group.colors) <- c("White, Not Hispanic or Latino",
                         "White, Hispanic or Latino",
                         "Black or African American",
                         "White, All Origins",
                         "Asian or Pacific Islander",
                         "American Indian or Alaska Native",
                         
                         "high school graduate or lower",
                         "some college education but no 4-year college degree",
                         "4-year college graduate or higher",
                         
                         "non metro",
                         "large metro",
                         "small-medium metro"
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
## ----get legends ---
#own_get_legend <- function(p) {
#  g <- ggplot_build(p)
#  data.frame(
#    colour = unique(g$data[[1]]["colour"]),
#    column = g$plot$labels$colour,
#    label = unique(as.data.frame(g$plot$data)[, g$plot$labels$colour])
#  )
#}

#legend_df <- lapply(plots, own_get_legend) %>%
#  rbindlist() %>%
#  distinct()

#legend_df <- data.frame(
#  colour = group.colors,
#  label = names(group.colors),
#  column = c(rep("Ethnicity", 6), rep("Education", 3), rep("rural_urban_class", 3))
#)

#legend_df <- legend_df %>%
#  complete(colour, column, fill = list(label = "")) %>%
#  group_by(column) %>%
#  mutate(label = str_pad(label, max(nchar(label)), "right"))

#legend_df <- legend_df %>%
#  pivot_wider(
#    names_from = column,
#    values_from = label # ,
    # values_fill = ""
#  ) %>%
#  as.data.frame() %>%
#  unite("labels", c("Ethnicity", "Education", "rural_urban_class"), sep = " | ")

# color_legend <- legend_df$colour
# names(color_legend) <- legend_df$labels

#legend_plot <- get_legend(ggplot(
#  data = legend_df %>% mutate(test = 1),
#  aes(color = labels, x = test, y = test)
#) +
#  theme(
#    legend.title = element_blank(),
#    legend.key.size = unit(4, "mm"),
#    legend.text = element_text(family = "mono", size = 7)
#  ) +
  #  guides(color = guide_legend(label.position = "left"))+
#  geom_point() +
#  scale_colour_manual(values = color_legend))
#as_ggplot(legend_plot)

# legend_df <- legend_df %>% select(Ethnicity, Education, rural_urban_class)
# legend_theme <- gridExtra::ttheme_minimal(
#  core = list(padding=unit(c(2, 2), "mm"),
#              fg_params = list(#hjust=0, x=0.1,
#                               fontsize=9)
#              )
# )
# legend_grob_table <- tableGrob(legend_df, theme=legend_theme,  rows = NULL, cols = NULL)

# rm(color_legend)

## --- arrange plots----
lay <- rbind(
  c(NA, NA, 13, NA, NA, 14),
  c(10, 7, 1, NA, 8, 4),
  c(NA, 7, NA, NA, 8, NA),
  c(11, 7, 2, NA, 8, 5),
  c(NA, 7, NA, NA, 8, NA),
  c(12, 7, 3, NA, 8, 6),
  c(NA, NA, 9, 9, 9, 9)
)

t1 <- textGrob("age-adjusted death rate per 100,000", rot = 90, gp = gpar(fontsize = 10), vjust = 1)
t2 <- textGrob("%", rot = 90, gp = gpar(fontsize = 10), vjust = 1)

t3 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Ethnicity", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 1)
)
t4 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Education", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 1)
)
t5 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("Rural-Urban", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"), vjust = 1)
)

t6 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("burden attributable", gp = gpar(fontsize = 10, fontface = "bold"))
)
t7 <- grobTree(
  rectGrob(gp = gpar(fill = "grey")),
  textGrob("proportion of all-cause burden attributable", gp = gpar(fontsize = 10, fontface = "bold"))
)

gs <- append(plots, list(t1, t2, legend_plot, t3, t4, t5, t6, t7))
#gs <- lapply(1:14, function(ii) grobTree(rectGrob(gp = gpar(fill = ii, alpha = 0.5)), textGrob(ii)))

blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(0.1, 0.1, figure_width , blank_space, 0.1, figure_width),
  heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.6),
  layout_matrix = lay
)

g_combined
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, "figure1.png"), g_combined, height = 9, width = 8)
