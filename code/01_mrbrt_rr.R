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
packages <- c("magrittr", "MALDIquant", "ggplot2", "dplyr", "tictoc", "tidyr")

options(tidyverse.quiet = TRUE)
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)
tmpDir <- args[1]
exp_rrDir <- args[2]

# TODO delete
if (rlang::is_empty(args)) {
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
}

plotsDir <- file.path(exp_rrDir, "plots")
dir.create(plotsDir, recursive = T, showWarnings = F)

mrbrtDir <- file.path(exp_rrDir, "mrbrt_summary")
## --------either load data or write it---------
# write useful overview over causes
causes_agesDir <- file.path(tmpDir, "causes_ages.csv")

if (file.exists(causes_agesDir)) {
  causes_ages <- read.csv(causes_agesDir)
} else {
  # Chronic obstructive pulmonary disease, ? ,lower respiratory infections, ?, type 2 diabetes
  causes_all_ages <- c("resp_copd", "lri", "neo_lung", "t2_dm")
  causes_age_specific <- c("cvd_ihd", "cvd_stroke")

  age_ids <- seq.int(25, 95, 5)

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

#tmrels <- runif(1000, min = 2.4, max = 5.9)
tmrels <- file.path(mrbrtDir, "tmrel_draws.csv") %>% 
  read.csv %>% 
  unlist
## ----------calculation---------

tic("Calculated RR from MR-BRT for all causes")
apply(causes_ages, 1, function(cause_age) {
  label_cause <- cause_age[1]
  age_group_id <- cause_age[2]
  
  file_name <- ifelse(age_group_id == "all ages",
    paste0(label_cause, ".csv"),
    paste0(label_cause, "_", age_group_id, ".csv")
  )

  exp_rrDirX <- file.path(exp_rrDir, file_name)

  if (!file.exists(exp_rrDirX)) {
    tic(paste("Calculated RR from MR-BRT for", label_cause, "age group:", age_group_id))
    
    mrbrtDirX <- file.path(mrbrtDir, file_name)
    exp_mrbrt <- read.csv(mrbrtDirX)
    
    ## --interpolate mrbrt data
    # X values of points to interpolate from known data
    aim <- exp_mrbrt$exposure_spline 
    aim <- aim[aim < 10]
    aim <- c(aim, seq(10,40,0.1))
    
    interp <- approx(exp_mrbrt$exposure_spline,
                     exp_mrbrt$mean,
                     xout = aim)
    
    exp_mrbrt_interp <- data.frame(
      label = label_cause,
      exposure_spline = interp$x,
      mean = interp$y,
      row.names = NULL
    )
    
    ## --calculate RR
    getMRBRT <- function(pm) {
      match.closest(pm, exp_mrbrt_interp$exposure_spline) %>%
        exp_mrbrt_interp[., "mean"] %>%
        as.numeric() %>%
        return(.)
    }
    
    getRR_tmrel <- function(tmrel, pm) {
      rr <- ifelse(pm <= tmrel,
                   1,
                   getMRBRT(pm) / getMRBRT(tmrel)
      )
      return(rr)
    }
    
    getRR <- function(pm) {
      rrs <- sapply(tmrels, getRR_tmrel, pm = pm)
      return(mean(rrs))
    }
    
    exp_rr <- exp_mrbrt_interp %>% transmute(
      label = label,
      exposure_spline = exposure_spline,
      mrbrt = mean,
      rr = sapply(exposure_spline, getRR),
      interpolated = sapply(exposure_spline, function(exposure_spline){
        ifelse(exposure_spline %in% exp_mrbrt$exposure_spline,
               "not interpolated",
               "interpolated")
      })
    )
    
    write.csv(exp_rr, exp_rrDirX, row.names = FALSE)
    toc()
  }
  
  exp_rr <- read.csv(exp_rrDirX)
  plotDirX <-file.path(plotsDir, paste0(label_cause, "_", age_group_id, ".png"))

  if (!file.exists(plotDirX) && TRUE) {
    exp_rr2 <- exp_rr %>%
      pivot_longer(
        cols = !c("exposure_spline", "label", "interpolated"),
        names_to = "measure",
        values_to = "value"
      ) %>%
      filter(interpolated == "not interpolated") %>%
      as.data.frame()

    exp_rr2[exp_rr2 == "mrbrt"] <- "MR-BRT"
    exp_rr2[exp_rr2 == "rr"] <- "RR"

    ggplot(data = exp_rr2, aes(x = exposure_spline, y = value)) +
      #geom_point(aes(color = measure, shape = interpolated), size =2) +
      geom_point(color = "black", size =2, shape = 2) +
      geom_line(aes(color = measure), size =1) +
      xlab("Exposure") +
      ylab("RR") +
      ggtitle(paste0(label_cause, ", ", age_group_id))

    ggsave(plotDirX)
  }

  plotDirX <- paste0(label_cause, "_", age_group_id, ".png") %>%
    file.path(plotsDir, .)
  
})
toc()
""
