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
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "grid", "cowplot",
  "dplyr"
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
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  scenarioI <- "A"
  methodI <- "burnett"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist
rm(file_list)

theme_set(theme_classic())
dir.create(file.path(figuresDir,methodI))
### ----- read stuff----
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))

# filter
all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & attr == "overall" & rural_urban_class == "All")

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & 
           method == methodI & attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & rural_urban_class == "All")


pm_summ <- pm_summ %>% filter(agr_by == "nation" & pm_metric == "mean" & Gender.Code == "All genders" & scenario == "A")

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
# https://rpubs.com/Koundy/71792
# http://rstudio-pubs-static.s3.amazonaws.com/9575_8a5dc0315e7d48ea94e0fd2546727041.html
## --- figure 1, population-weighted mean particular matter exposure---
pm_summ1 <- pm_summ %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All")
g1 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("population-weighted annual average PM2.5 in μg/m^3") +
  xlim(1990, 2016) +
  theme(
    legend.title = element_blank(),
    #legend.position = position_dodge(3),
    legend.position = c(0.72, 0.88),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = FALSE))
#ggsave(file.path(figuresDir, "figure1a.png"), g1)
#https://ggplot2.tidyverse.org/reference/position_dodge.html

pm_summ2 <- pm_summ %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")
g2 <- ggplot(pm_summ2, aes(x = Year, y = value, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("population-weighted annual average PM2.5 in μg/m^3") +
  theme(
    legend.title = element_blank(),
    #legend.position = c(0.4, 0.2), # plot.margin = unit(c(0,1,0,-1), "lines") plot.margin = unit(c(0,-.5,0,.5), "lines"),
    legend.position = c(0.43, 0.14),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure1b.png"), g2)

g3 <- ggarrange(g1, g2, ncol = 1, labels = "AUTO", align = "v")
ggsave(file.path(figuresDir,methodI, "app_figure1.png"), g3, height = 9, width = 8)

# https://stackoverflow.com/questions/64757410/shared-x-and-y-axis-labels-ggplot2-with-ggarrange
rm(g1, g2,g3, pm_summ1, pm_summ2)
## -- figure 2, all-cause burden ----
all_burden1 <- all_burden %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All")
g1 <- ggplot(all_burden1, aes(x = Year, y = overall_value, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("all-cause age-adjusted mortality per 100k") +
  xlim(1990, 2016) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7, 1.0),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(20,20,20,20),"points")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure2a.png"), g1)
#g1$layout$clip[g1$layout$name == "panel"] <- "off"

all_burden2 <- all_burden %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")
g2 <- ggplot(all_burden2, aes(x = Year, y = overall_value, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("all-cause age-adjusted mortality per 100k") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.09),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure2b.png"), g2)

g3 <- ggarrange(NULL, g1,NULL, g2, 
                ncol = 1, 
                heights = c(0.1,1, 0.1, 1),
                labels = c("","A", "", "B"),
                align = "v"
                )

ggsave(file.path(figuresDir,methodI, "app_figure2.png"), g3, height = 9, width = 8)

rm(all_burden1, all_burden2, g1, g2, g3)
## -- figure 3, attributable burden----
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins"& measure3 == "value" & rural_urban_class == "All")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("age-adjusted death rate per 100,000") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.85),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins"& measure3 == "value" & rural_urban_class == "All")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("age-adjusted death rate per 100,000") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.1),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) #0.1

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.2, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir,methodI, "main_figure1.png"), g3, height = 9, width = 8)
rm(attr_burd1, attr_burd2, g1, g2, g3)
## -- figure 4, prop. of overall burden----
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins"& measure3 == "prop. of overall burden" & rural_urban_class == "All")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins"& measure3 == "prop. of overall burden" & rural_urban_class == "All")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.1),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))  +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.2, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir,methodI, "figure4.png"), g3, height = 9, width = 8)

rm(attr_burd1, attr_burd2, g1, g2) 
##---figure 5, prop of difference----

attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All"&
                                     measure3 == "proportion of disparity to Black or African American attributable" #&
                                     #Ethnicity %in% c("Black or African American")
                                     )
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) 

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All"
                                   & measure3 == "proportion of disparity to lower educational attainment")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.685, 1.03),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) 

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.1, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir,methodI, "figure5.png"), g3, height = 9, width = 8)
rm(attr_burd1, attr_burd2, g1, g2) 