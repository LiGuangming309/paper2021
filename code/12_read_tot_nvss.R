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
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
totalBurdenDir <- args[12]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  year <- 2000
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
}

totalBurdenDir <- file.path(totalBurdenDir, "nvss", paste0("mort", year, ".csv"))
totalBurdenParsedDir <- file.path(totalBurdenParsedDir, agr_by, "nvss")
dir.create(totalBurdenParsedDir, recursive = T, showWarnings = F)
totalBurdenParsedDir <- file.path(
  totalBurdenParsedDir,
  "total_burden_nvss.csv"
)

if (!file.exists(totalBurdenParsedDir)) {
  lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))

  ## ----determine join variables


  ## ----- read total burden ---------
  tic(paste("read", year, "total burden data"))
  total_burden <- fread(totalBurdenDir)

  test <- total_burden$ucod %>%
    unique() %>%
    sort()

  selectcolumns <- c(
    "Year" = "year",
    "label_cause" = "ucod", # record_1/enum_1
    "Education" = "educ", # 52-53
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "Single.Year.Ages.Code" = "age", # 64
    "Hispanic.Origin" = "hispanic" # 80 - 81
  )

  # All public-use micro-data files from 2005-present contain individual-level vital event data at the national level only.
  # Specifically, these files contain no geographic identifiers at the state, county, or city level.
  # see https://www.cdc.gov/nchs/nvss/dvs_data_release.htm
  if (year <= 2004) {
    if (agr_by == "nation") {
      total_burden <- total_burden %>% tibble::add_column(nation = "us")
      selectcolumns <- c(selectcolumns, "nation")
    } else if (agr_by == "STATEFP") {

      # if staters==0 (foreign resident), take state of occurance
      total_burden <- total_burden %>% mutate(
        staters = if (staters == 0) stateoc
      )
      selectcolumns <- c(selectcolumns, "State.Code" = "staters") # residence, not occurance
    } else if (agr_by == "county") {
      # if staters==0 (foreign resident), take state of occurance
      total_burden <- total_burden %>% mutate(
        staters = if (staters == 0) stateoc,
        countyrs = if (countyrs == 0) countyoc
      )
      selectcolumns <- c(selectcolumns, "State.Code" = "staters", "County.Code" = "countyrs") # residence, not occurance
    } else {

    }
  } else {
    if (agr_by == "nation") {
      total_burden <- total_burden %>% tibble::add_column(nation = "us")
      selectcolumns <- c(selectcolumns, "nation")
    } else {
      # TODO
      print(paste("in", year, "no geopgraphic identifier available"))
    }
  }

  # https://data.nber.org/mortality/2000/interim2000p1.pdf
  total_burden <- total_burden %>% select(all_of(selectcolumns))

  #---------find and replace stuff--------
  replacecolumns <- c("Hispanic.Origin", "Gender.Code", "Race", "label_cause")

  findreplaces <- list(
    data.frame(
      from = c(0, 1, 2, 3, 4, 5, 99),
      to = c("Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Unkown")
    ),
    data.frame(
      from = c(1, 2),
      to = c("M", "F")
    ),
    data.frame(
      from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 78),
      to = c(
        "White", "Black or African American", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
        "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Guama",
        "Asian or Pacific Islander"
      )
    ), 
    lapply(c("", 0:9), function(end) {
      data.frame(
        from = c(
          "I20", "I21", "I22", "I23", "I24", "I25",
          "G45", "G46", "I61", "I62", "I63", "I65", "I66", "I67", "I68", "I69",
          "C33", "C34", "D02", "D14", "D38",
          "J41", "J42", "J43", "J44", # TODO Format
          "A48", "A70", "B97", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J20", "J21", "P23", "U04",
          "E11"
        ),
        to = c(
          "cvd_ihd", "cvd_ihd", "cvd_ihd", "cvd_ihd", "cvd_ihd", "cvd_ihd",
          "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke", "cvd_stroke",
          "neo_lung", "neo_lung", "neo_lung", "neo_lung", "neo_lung",
          "resp_copd", "resp_copd", "resp_copd", "resp_copd",
          "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri", "lri",
          "t2_dm"
        )
      ) %>% mutate(from = paste0(from, end))
    }) %>% do.call(rbind, .)
  )

  for (i in seq_along(replacecolumns)) {
    replacecolumn <- replacecolumns[i]
    findreplace <- findreplaces[[i]]

    total_burden[, replacecolumn] <- total_burden %>%
      select(all_of(replacecolumn)) %>%
      left_join(findreplace,
        by = setNames("from", replacecolumn),
        na.replace = "other"
      ) %>%
      select(to)
  }

  ### ----raplce age---
  total_burden <- total_burden %>% mutate(
    min_age = Single.Year.Ages.Code,
    max_age = Single.Year.Ages.Code,
    Single.Year.Ages.Code = NULL
  )

  ### --- calculate burden----
  total_burden <- total_burden %>%
    group_by_at(colnames(total_burden)) %>%
    summarise(value = n())

  total_burden <- total_burden %>% tibble::add_column(measure = "deaths")

  total_burden_yll <- total_burden %>%
    dplyr::mutate(
      Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(max_age, lifeExpectancy$Age)],
      value = value * Life.Expectancy,
      measure = "YLL",
      Life.Expectancy = NULL
    )

  total_burden <- rbind(total_burden, total_burden_yll)
  rm(total_burden_yll)


  # TODO add attr
  # TODO all-cause

  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
