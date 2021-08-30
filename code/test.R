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

attr_burd <- rbind(
  fread(file.path(summaryDir, "attr_burd.csv")),
  fread(file.path(summaryDir, "attr_burd_prop.csv"))
)

theme_set(theme_classic())
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

all_burden1 <- all_burden %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins")
g1 <- ggplot(all_burden1, aes(x = Year, y = overall_value, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("all-cause age-adjusted mortality per 100k") +
  xlim(1990, 2016) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.7, 1),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(20,20,20,20),"points")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
g1
