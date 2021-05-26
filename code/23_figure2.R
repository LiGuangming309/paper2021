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

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
}
theme_set(theme_classic())
attr_burd <- rbind(
  fread(file.path(summaryDir, "attr_burd.csv")),
  fread(file.path(summaryDir, "attr_burd_prop.csv"))
)

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == "burnett" & attr == "attributable" &
    source == "National Vital Statistics System")

##---plot---
attr_burd1 <- attr_burd %>%
  filter(
    measure3 == "value" & Education == 666 &
      Region %in% c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina")
  )

g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("deaths per 100.000") +
  # ylab("age-adjusted death rate per 100,000") +
  theme(legend.position="bottom")+
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1) +
  #facet_grid(~Region,ncol = 3, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) 
  #scales='free_x', space='free_x', 
  facet_wrap(~Region)

ggsave(file.path(figuresDir, "app_figure1.png"), g1, height = 9)