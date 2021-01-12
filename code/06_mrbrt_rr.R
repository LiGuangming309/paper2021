#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/03/2020
# Purpose: calculate RR from MR-BRT
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))
set.seed(0)
# load packages, install if missing
packages <- c("magrittr", "MALDIquant", "ggplot2", "dplyr", "tictoc")

options(tidyverse.quiet = TRUE)
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)
tmpDir <- args[3]
exp_rrDir <- args[6]

plotsDir <- file.path(exp_rrDir, "plots")
dir.create(plotsDir, recursive = T, showWarnings = F)


## --------either load data or write it---------
# write useful overview over causes
causes_agesDir <- file.path(tmpDir, "causes_ages.csv")

if (file.exists(causes_agesDir)) {
  causes_ages <- read.csv(causes_agesDir)
} else {
  # Chronic obstructive pulmonary disease, ? ,lower respiratory infections, ?, type 2 diabetes
  causes_all_ages <- c("resp_copd", "lri", "neo_lung", "t2_dm")
  causes_age_specific <- c("cvd_ihd", "cvd_stroke")

  age_ids <- seq.int(25, 95, 5) # TODO assuming folks do not get older

  causes_ages <- data.frame(
    label_cause = rep(causes_age_specific, each = length(age_ids)),
    age_group_id = rep(age_ids, times = length(causes_age_specific))
  )

  causes_ages <- data.frame(
    label_cause = causes_all_ages,
    age_group_id = rep("all ages", each = length(causes_all_ages))
  ) %>%
    rbind(causes_ages)

  write.csv(causes_ages, causes_agesDir, row.names = FALSE)
}

# drar samples for tmrel
# tmrelsDir <- file.path(tmpDir, "tmrels.csv")
# if(file.exists(tmrelsDir)){
#  tmrels <- read.csv(tmrelsDir)[[1]]
# }else{
#  tmrels <-runif(1000, min= 2.4, max= 5.9) %>%
#                round(digits = 2)
#  write.csv(tmrels,tmrelsDir, row.names = FALSE)
# }

#tmrel <- mean(2.4, 5.9)
tmrel <- 1.3
## ----------calculation---------

tic("Calculated RR from MR-BRT for all causes")
apply(causes_ages, 1, function(cause_age) {
  label_cause <- cause_age[1]
  age_group_id <- cause_age[2]

  exp_rrDirX <- ifelse(age_group_id == "all ages",
    paste0(label_cause, ".csv"),
    paste0(label_cause, "_", age_group_id, ".csv")
  ) %>%
    file.path(exp_rrDir, .)

  exp_rr <- exp_rrDirX %>%
    read.csv() %>%
    filter(exposure_spline <= 50)

  if ("rr" %in% colnames(exp_rr)) {
    return()
  } # TODO entkommentieren

  getMRBRT <- function(pm) {
    match.closest(pm, exp_rr$exposure_spline) %>%
      exp_rr[., "mean"] %>%
      return(.)
  }

  tmrelMRBR <- getMRBRT(tmrel)

  getRR <- function(pm) {
    ifelse(pm <= tmrel,
      1,
      getMRBRT(pm) / tmrelMRBR
    ) %>%
      as.numeric %>%
      return(.)
  }

  # getRR2 <-function(pm){
  #  sapply(tmrels, function(tmrel){
  #    ifelse(pm<=tmrel,
  #           1,
  #           getMRBRT(pm)/getMRBRT(tmrel)) %>%
  #      return(.)
  #  }) %>% mean %>%
  #      return(.)
  #  }

  exp_rr <- exp_rr %>% mutate(
    lower = NULL,
    upper = NULL,
    rr = getRR(exposure_spline)
  )

  write.csv(exp_rr, exp_rrDirX, row.names = FALSE)

  plotDirX <- paste0(label_cause, "_", age_group_id, ".png") %>%
    file.path(plotsDir, .)

  if (FALSE && !file.exists(plotDirX)) {
    ggplot(data = exp_rr, aes(x = exposure_spline, y = rr)) +
      geom_point() +
      xlab("Exposure") +
      ylab("RR") +
      ggtitle(paste(label_cause, age_group_id))


    ggsave(plotDirX)
  }
})
toc()
""
