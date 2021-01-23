#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/23/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
censDir <- args[3]
attrBurdenDir <- args[4]
summaryDir <- args[5]
plotDir <- args[6]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/10_plots"
}
