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
  "dplyr", "tigris","tmap" 
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
  methodI <- "di_gee"
}

states <- tigris::states()

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist
rm(file_list)

attr_burd$method %>% unique
attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == methodI 
         & attr == "attributable"  &
    source == "National Vital Statistics System" & scenario == scenarioI & rural_urban_class == "All" & agr_by == "STATEFP" 
    & Year %in% 2000:2016 #2000:2016
    & !Region %in% c("Alaska", "Hawaii"))

attr_burd1 <- attr_burd %>% filter(measure3 == "proportion of disparity to Black or African American attributable"& Ethnicity == "White, Not Hispanic or Latino")
attr_burd1 <- attr_burd1 %>%
  group_by(Region) %>%
  summarise(mean = mean(mean))

attr_burd2 <- attr_burd %>% 
  filter(measure3 == "value"& Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American")) %>%
  mutate(mean = case_when(Ethnicity == "White, Not Hispanic or Latino"~ mean,
                          Ethnicity == "Black or African American"~ -mean)) %>%
  group_by(Region, Year) %>%
  summarise(mean = sum(mean)) %>%
  group_by(Region) %>%
  summarise(mean = mean(mean))

##---plot---
states_join1 <- tigris::geo_join(states, attr_burd1, "NAME", "Region", how = "inner")
tm1 <- tm_shape(states_join1) + tm_polygons("mean", alpha = 0.6)

states_join2 <- tigris::geo_join(states, attr_burd2, "NAME", "Region", how = "inner")
tm2 <- tm_shape(states_join2) + tm_polygons("mean", alpha = 0.6)
