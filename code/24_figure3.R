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
  "dplyr"
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

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
}

#--- reading total burden data---
files <- list.files(file.path(totalBurdenDir, "nation", "nvss"))
total_burden <- lapply(files, function(file) {
  total_burden <- fread(file.path(totalBurdenDir, "nation", "nvss", file))
  total_burden <- total_burden %>%
    filter(Gender.Code == "A" & measure1 == "Deaths" & measure2 == "absolute number" & source == "nvss" &
      Education == 666) %>%
    mutate(
      age_group_id = seq(25, 95, 5)[
        findInterval(
          max_age,
          seq(25, 90, 5),
          left.open =  TRUE
        )
      ]
    ) %>%
    group_by(Year, label_cause, age_group_id) %>%
    dplyr::summarise(value = sum(value))
  return(total_burden)
}) %>% rbindlist()
rm(files)


## --- GBD estimate----
gbd <- function(pms) {
  example_exp_rr <- file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv()
  pm_levels <- example_exp_rr$exposure_spline
  causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()

  pafs <- apply(causes_ages, 1, function(cause_age) {
    # subset rows of total burden
    label_causeX <- cause_age[["label_cause"]]
    age_group_idX <- cause_age[["age_group_id"]]
    total_burden <- total_burden %>% filter(label_cause == label_causeX)
    if (age_group_idX != "all ages") {
      total_burden <- total_burden %>% filter(age_group_id == as.numeric(cause_age[["age_group_id"]]))
    }
    total_burden <- total_burden %>%
      group_by(Year, label_cause) %>%
      dplyr::summarise(value = sum(value))


    exp_rr <- ifelse(age_group_idX == "all ages",
      paste0(label_causeX, ".csv"),
      paste0(label_causeX, "_", age_group_idX, ".csv")
    ) %>%
      file.path(exp_rrDir, .) %>%
      fread()
  
    pm_matched <-sapply(pms, function(x) pm_levels[match.closest(x, pm_levels)])
    
    exp_rr <- as.matrix(exp_rr[, -1])
    rownames(exp_rr) <- pm_levels
    exp_rr <- exp_rr[as.character(pm_matched), ]
    exp_rr <- apply(exp_rr, 1:2, function(x) (x-1) /x)
    exp_rr <- data.frame(pm = row.names(exp_rr) %>% as.numeric(), exp_rr)
    exp_rr <- exp_rr %>%
      pivot_longer(cols)
    
    test <- merge(total_burden, exp_rr)
  })
}
gbd(1:5)
