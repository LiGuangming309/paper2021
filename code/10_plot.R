#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

agr_by <- args[1]
attrBurdenDir <- args[2]
plotsDir <- args[3]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
  plotsDir <- "/Users/default/Desktop/paper2021/data/10_plots"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
plotsDir <- file.path(plotsDir, agr_by)
dir.create(plotsDir, recursive = T, showWarnings = F)

### ---read attributable burden data-----
files <- list.files(attrBurdenDir)

attrBurden <- lapply(files, function(file) {
  attrBurden <- file.path(attrBurdenDir, file) %>% read.csv()
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

missing <- setdiff(2000:2016, attrBurden$Year)
if (length(missing) > 0) {
  print("Years missing in attributable burden data")
  print(missing)
}

## --- group-----
groups <- c("Year", "Race", "Hispanic.Origin")
attrBurden_gr <- attrBurden %>%
  group_by_at(vars(one_of(groups))) %>%
  summarise(
    Deaths = sum(Deaths),
    YLD = sum(YLD),
    attrDeaths = sum(attrDeaths),
    attrYLD = sum(attrYLD)
  ) %>%
  as.data.frame()

fwrite(attrBurden_gr, file.path(plotsDir, "attr_burd.csv"))

## ---plot ------
for (his_or in unique(attrBurden_gr$Hispanic.Origin)) {
  attrBurden_gr_his <- attrBurden_gr %>% filter(Hispanic.Origin == his_or)


  for (measure in c("Deaths", "YLD", "attrDeaths", "attrYLD")) {
    g <- attrBurden_gr_his %>%
      ggplot(aes(x = Year, y = measure, group = Race, color = Race)) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle(paste("hispanic origin:", his_or)) +
      theme_ipsum() +
      ylab(paste("burden measured in", measure)) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016)+
      geom_line() 
    
    ggsave(file.path(plotsDir, paste0(measure, "_", his_or, ".png")),
           plot = g 
    )
  }
}
