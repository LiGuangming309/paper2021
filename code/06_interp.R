#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1] %>% as.numeric()
dataDir <- args[2]
tmpDir <- args[3]
censDir <- args[8]

if (rlang::is_empty(args)) {
  year <- 2003
  #dataDir <- "/Users/default/Desktop/paper2021/data"
  #tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  #censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
}