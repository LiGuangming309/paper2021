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
  #summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
  #figuresDir <-  "C:/Users/Daniel/Desktop/paper2021/data/15_figures"
  scenarioI <- "A"
  methodI <- "burnett"
}
theme_set(theme_classic())
file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist
rm(file_list)

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == methodI & attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & rural_urban_class == "All")

##---plot---
attr_burd1 <- attr_burd %>%
  filter(
    measure3 == "value" & Education == 666 & Ethnicity != "All, All Origins"& 
      Region %in% c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina")
       )

g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1) +
  xlab("Year") +
  #ylab("Mortality per 100k") +
   ylab("age-adjusted mortality per 100k") +
  
  theme(legend.position="bottom",
        legend.box.margin = margin(1, 36, 1, 6))+
  #theme(legend.position=c(0.1, 0.1))+
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
  #facet_grid(~Region,ncol = 3, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) 
  #scales='free_x', space='free_x', 
  facet_wrap(~Region) 

ggsave(file.path(figuresDir, "app_figure1.svg"), g1, height = 9, width = 8)