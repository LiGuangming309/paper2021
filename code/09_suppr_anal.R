#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 02/01/2021
# Purpose: analysie suppression in data from CDC WONDER by ethnicity
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "xlsx")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[3]
agr_by <- args[10]
totalBurdenDir <- args[12]
unsTotalBurdenDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2016
  agr_by <- "nation"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  unsTotalBurdenDir <- "/Users/default/Desktop/paper2021/data/09_unsup_total_burden"
}

totalBurdenDir <- file.path(totalBurdenDir, agr_by)
ethn_supprDir <- file.path(tmpDir, "ethn_suppr.csv")

if (!file.exists(ethn_supprDir)) {

  ## ---- calc------------
  label_causes <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "cvd_stroke")
  ethn_suppr <- lapply(label_causes, function(label_cause) {
    lapply(c("", "_all"), function(end) {
      filename <- paste0(label_cause, end, ".txt")
      # https://wonder.cdc.gov/controller/saved/D77/D105F096
      total_burden <- read.delim(file.path(totalBurdenDir, filename))
      if (!"Hispanic.Origin" %in% colnames(total_burden)) total_burden[, "Hispanic.Origin"] <- "All Origins"
      # https://wonder.cdc.gov/controller/saved/D77/D107F342
      uns_total_burden <- read.delim(file.path(unsTotalBurdenDir, filename))
      if (!"Hispanic.Origin" %in% colnames(uns_total_burden)) uns_total_burden[, "Hispanic.Origin"] <- "All Origins"


      total_burden <- total_burden %>%
        filter(
          Deaths != "Suppressed"
        ) %>%
        mutate(Deaths = as.numeric(Deaths)) %>%
        group_by(Race, Hispanic.Origin) %>%
        summarise(Deaths = sum(Deaths)) %>%
        filter(!is.na(Deaths))

      uns_total_burden <- uns_total_burden %>%
        filter(Deaths != "Suppressed") %>%
        mutate(
          Deaths = as.numeric(Deaths),
          Population = as.numeric(Population),
          Crude.Rate = as.numeric(Crude.Rate)
        ) %>%
        group_by(Race, Hispanic.Origin) %>%
        summarise(
          Deaths = sum(Deaths),
          Population = sum(Population),
          Crude.Rate = sum(Crude.Rate)
        )

      ethn_suppr <- left_join(total_burden, uns_total_burden, by = c("Race", "Hispanic.Origin"))

      ethn_suppr$label_cause <- label_cause
      return(ethn_suppr)
    }) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

  ethn_suppr <- ethn_suppr %>%
    mutate(
      suppressed = 100 - 100 * Deaths.x / Deaths.y,
      factor = Deaths.y / Deaths.x
    )
  
  fwrite(ethn_suppr, ethn_supprDir)
}

if(TRUE){
  ethn_suppr <- fread(ethn_supprDir)
  ethn_suppr2 <- ethn_suppr %>%
    group_by(Race, Hispanic.Origin) %>%
    summarise(Deaths.x = sum(Deaths.x),
              Deaths.y = sum(Deaths.y)) %>%
    mutate(
      suppressed = 100 - 100 * Deaths.x / Deaths.y
    ) %>%
    arrange(suppressed) %>%
    as.data.frame
  
  ethn_suppr2_sub <- ethn_suppr2 %>% 
    filter(suppressed >= 10) %>%
    select(Race, Hispanic.Origin, suppressed)  %>%
    as.data.frame
  
  print("Following ethnicites should be ignored, because data suppression is too high:")
  print(ethn_suppr2_sub)
  
  write.xlsx(ethn_suppr2, file.path(tmpDir, "ethn_suppr2.xlsx"), sheetName = "Sheet1", 
             col.names = TRUE, row.names = FALSE, append = FALSE)
}
