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
  "dplyr", "tigris","tmap" #, "maps", "mapdata", 
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
  methodI <- "di_gee"
}
theme_set(theme_classic())
file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist
rm(file_list)

attr_burd$method %>% unique
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == methodI 
         & attr == "attributable" & measure3 == "proportion of disparity to Black or African American attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & rural_urban_class == "All" & agr_by == "STATEFP" 
    & Year %in% 2000:2016 & Ethnicity == "White, Not Hispanic or Latino")

test <- attr_burd %>% 
  filter(mean >= 0) %>%
  group_by(Region) %>%
  summarise(years = list(Year),
            n = n()) 

attr_burd <- attr_burd %>%
  group_by(Region) %>%
  summarise(mean = mean(mean))
##---plot---

#state <- maps::map("state", boundary=FALSE, col="gray", add=TRUE)
#maps::map("state", boundary=FALSE, col="gray", add=TRUE)

states <- tigris::states()

states_join <- tigris::geo_join(states, attr_burd, "NAME", "Region", how = "inner")

tm_shape(states_join, projection = 26916) +
  tm_fill("mean") + 
  tm_legend(bg.color = "white", bg.alpha = 0.6)# + 
  #tm_style_gray()

tm <- tm_shape(states_join) + tm_polygons("mean", alpha = 0.6)
tm
