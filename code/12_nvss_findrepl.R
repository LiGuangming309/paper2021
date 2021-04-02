#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table",  "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[3]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
}
findreplaceDir <- file.path(totalBurdenParsedDir, "findreplace.csv")
states <- file.path(tmpDir, "states.csv") %>% read.csv
if(!file.exists(findreplaceDir)){
  #causes
  findreplaces1 <- 
    lapply(c("", 0:9), function(end) {
      data.frame(
        replacecolumns = "label_cause",
        from = c(
          "I20", "I21", "I22", "I23", "I24", "I25",
          "G45", "G46", "I61", "I62", "I63", "I65", "I66", "I67", "I68", "I69",
          "C33", "C34", "D02", "D14", "D38",
          "J41", "J42", "J43", "J44", 
          "A48", "A70", "B97", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J20", "J21", "P23", "U04",
          "E11"
        ),
        to = c(rep("cvd_ihd", 6), rep("cvd_stroke", 10), rep("neo_lung", 5), rep("resp_copd", 4), rep("resp_copd", 15), "t2_dm")
      ) %>% mutate(from = paste0(from, end))
    }) %>% do.call(rbind, .)
  
  findreplaces1 <- merge(data.frame(Year = 2000:2016), findreplaces1)
  ####----- 2000-2002------
  findreplaces2 <-
    rbind(
      data.frame(
        replacecolumns = "Hispanic.Origin",
        from = c(0, 1, 2, 3, 4, 5, 99),
        to = c("Not Hispanic or Latino", rep("Hispanic or Latino", 5), "Unkown")
      ),
      data.frame(
        replacecolumns = "Gender.Code",
        from = c(1, 2),
        to = c("M", "F")
      ),
      data.frame(
        replacecolumns = "Race",
        from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
        to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Guama", rep("Asian or Pacific Islander", 2))
      )
    )
  findreplaces2 <- merge(data.frame(Year = 2000:2002), findreplaces2)
  ####----- 2003 -------
  findreplaces3 <-
    rbind(
      data.frame(
        replacecolumns = "Hispanic.Origin",
        from = c(1:5, 6:8,9),
        to = c(rep("Hispanic or Latino", 5), rep("Not Hispanic or Latino", 3), "Unkown")
      ),
      data.frame(
        replacecolumns = "STATEFP",
        from = states$STUSPS,
        to = states$STUSPS
      ),
      data.frame(
        replacecolumns = "Race",
        from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
        to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Guama", rep("Asian or Pacific Islander", 2))
      ),
      data.frame(
        replacecolumns = "min_age",
        from = c(1:52), #TODO
        to = c(rep(0, 22), 1,2,3,4,seq(5,120,5),125,"unknown") #TODO
      ),
      data.frame(
        replacecolumns = "max_age",
        from = c(1:52), #TODO
        to = c(rep(0, 22), 1,2,3,4,seq(9,124,5),150,"unknown") #TODO
      )
    )
  
  findreplaces3 <- merge(data.frame(Year = 2003:2016), findreplaces3)
  
  findreplaces <- rbind(findreplaces1,findreplaces2,findreplaces3)
  write.csv(findreplaces,findreplaceDir, row.names = FALSE)
}