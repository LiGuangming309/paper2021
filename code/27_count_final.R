#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)

#load calculated data
#summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
#if not downloaded, load from github
if(!file.exists(summaryDir)) summaryDir <- 'https://raw.github.com/FridljDa/paper2021/master/data/14_summary'

attrBurden <- rbind(fread(file.path(summaryDir, "attr_burd.csv")), 
                    fread(file.path(summaryDir, "attr_burd_prop.csv")))

all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pm_summ <- pm_summ %>% filter(scenario == "A")
pop_summary <- fread(file.path(summaryDir, "pop_summary.csv"))

## count states, where whites less affected
attrBurden1 <- attrBurden %>% filter(Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American") &
                                      Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & Region != "United States"
                                     & Year == 2004 & method == "burnett" & measure3 == "value")

attrBurden1 <- attrBurden1 %>%
  group_by(Region, scenario) %>%
  slice(which.max(mean)) %>%
  group_by(scenario, Ethnicity) %>%
  summarise(n = n())

## count states, where disparity explained by PM2.5
attrBurden2 <- attrBurden %>% filter(Ethnicity %in% c("White, Not Hispanic or Latino") &
                                       Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & Region != "United States"
                                     & Year == 2004 & method == "burnett" & measure3 == "proportion of disparity to Black or African American attributable")
attrBurden2 <- attrBurden2 %>%
  group_by(scenario) %>%
  summarise(n = sum(mean >= 10))