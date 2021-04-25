#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenParsedDir <- args[13]
attrBurdenDir <- args[14]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2001
  agr_by <- "nation"
  
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/10_attr_burd"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv"))

#read some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
total_burden <- file.path(totalBurdenParsedDir,  agr_by, "total_burden.csv") %>% 
  read.csv %>% 
  filter(Year == year)
#http://web.stanford.edu/~mburke/papers/burke_et_al_wildfire_pnas_2021.pdf
#https://github.com/burke-lab/wildfire-map-public/blob/main/work/14_figure3.R