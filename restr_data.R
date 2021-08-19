#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 08/19/2021
# Purpose: calculate PAF
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyr","tidyverse", "tictoc", "testthat", "MALDIquant", "ggplot2")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

MULT1999.AllCnty <- read.csv("~/Google Drive/currentDocumants/mixed Data/2020/2020job/HIGH/Progress/R code/Transfer/raw_restricted_fake/MULT1999.AllCnty.gz")