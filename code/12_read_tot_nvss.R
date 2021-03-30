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

  year <- 2003
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
  paste0("total_burden_nvss_", year, ".csv")
)

if (!file.exists(totalBurdenParsedDir)) {
  lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
  ## ----- read total burden ---------
  tic(paste("read", year, "total burden data"))
  total_burden <- fread(totalBurdenDir)
  numberDeaths <- nrow(total_burden)

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
      selectcolumns <- c(selectcolumns, "Nation" = "nation")
    } else if (agr_by == "STATEFP") {
      # if staters==0 (foreign resident), take state of occurance
      total_burden$staters <- apply(total_burden[, c("staters", "stateoc")], 1, function(row) {
        ifelse(row["staters"] != 0, row["staters"], row["stateoc"])
      })
      selectcolumns <- c(selectcolumns, "State.Code" = "staters") # residence, not occurance
    } else if (agr_by == "county") {
      # if staters==0 (foreign resident), take state of occurance
      total_burden$staters <- apply(total_burden[, c("staters", "stateoc")], 1, function(row) ifelse(row["staters"] != 0, row["staters"], row["stateoc"]))
      total_burden$countyrs <- apply(total_burden[, c("countyrs", "countyoc")], 1, function(row) ifelse(row["countyrs"] != 0, row["countyrs"], row["countyoc"]))

      selectcolumns <- c(selectcolumns, "State.Code" = "staters", "County.Code" = "countyrs") # residence, not occurance
    } else {

    }
  } else {
    if (agr_by == "nation") {
      total_burden <- total_burden %>% tibble::add_column(nation = "us")
      selectcolumns <- c(selectcolumns, "Nation" = "nation")
    } else {
      # TODO
      print(paste("in", year, "no geopgraphic identifier available"))
    }
  }

  # https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
  total_burden <- total_burden %>% select(all_of(selectcolumns))

  #---------find and replace stuff--------
  replacecolumns <- c("Hispanic.Origin", "Gender.Code", "Race", "label_cause") # TODO education

  findreplaces <- list(
    data.frame(
      from = c(0, 1, 2, 3, 4, 5, 99),
      to = c("Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino", "Unkown")
    ),
    data.frame(
      from = c(1,"M", 2,"F"),
      to = c("M","M", "F","M")
    ),
    data.frame(
      from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
      to = c(
        "White", "Black or African American", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
        "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Guama",
        "Asian or Pacific Islander", "Asian or Pacific Islander"
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

    # TODO anti_join
    replacement <- total_burden %>%
      select(all_of(replacecolumn)) %>%
      left_join(findreplace,
        by = setNames("from", replacecolumn),
        na.replace = "oth"
      ) %>%
      mutate(to = replace_na(to, "oth"))

    if (replacecolumn != "label_cause") {
      missing <- replacement %>% filter(to == "oth")
      if (nrow(missing) > 0) {
        print(paste("no value assigned in", replacecolumn, "for"))
        print(missing[,1] %>% unique %>% sort)
      }
    }

    total_burden[, replacecolumn] <- replacement %>% select(to)
  }

  ### --- calculate burden in Deaths and YLL----
  # Deaths
  total_burden <- total_burden %>%
    group_by_at(colnames(total_burden)) %>%
    summarise(value = n()) %>%
    mutate(measure = "Deaths")

  # YLL
  total_burden_yll <- total_burden %>%
    dplyr::mutate(
      Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(Single.Year.Ages.Code, lifeExpectancy$Age)],
      value = value * Life.Expectancy,
      measure = "YLL",
      Life.Expectancy = NULL
    )

  total_burden <- rbind(total_burden, total_burden_yll) %>% distinct()
  rm(total_burden_yll)

  ## --- seperate stuff----

  inverse_selectcolumns <- c(names(selectcolumns), "measure")
  # setdiff(colnames(total_burden),"value")

  # seperate education, add "All Education"
  total_burden_race <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Education")) %>%
    summarise(value = sum(value)) %>%
    mutate(Education = 666) # TODO

  total_burden_educ <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, c("Hispanic.Origin", "Race"))) %>%
    summarise(value = sum(value)) %>%
    mutate(
      Hispanic.Origin = "All Origins",
      Race = "All"
    )

  total_burden <- rbind(total_burden_race, total_burden_educ) %>% distinct()
  rm(total_burden_race, total_burden_educ)

  # add Hispanic Origin All Origins
  total_burden_all_his <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
    summarise(value = sum(value)) %>%
    mutate(Hispanic.Origin = "All Origins") # TODO doppelt gezählt

  total_burden <- rbind(total_burden, total_burden_all_his) %>% distinct()
  rm(total_burden_all_his)

  #--- add all-cause rows---
  total_burden_all <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "label_cause")) %>%
    summarise(value = sum(value)) %>%
    mutate(
      label_cause = "all-cause",
      attr = "overall"
    )

  total_burden_cause <- total_burden %>%
    filter(label_cause != "oth") %>%
    mutate(attr = "total")

  total_burden <- rbind(total_burden_all, total_burden_cause) %>% distinct()
  rm(total_burden_all, total_burden_cause)

  #----test----
  total_burden <- total_burden %>% as.data.frame()
  test_that("numbers add up", {
    test1 <- total_burden %>%
      filter(
        measure == "Deaths",
        label_cause == "all-cause",
        attr == "overall",
        Race == "All",
        Hispanic.Origin == "All Origins",
        Education != 666
      )

    expect_equal(sum(test1$value), numberDeaths)

    test2 <- total_burden %>%
      filter(
        measure == "Deaths",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin == "All Origins",
        Education == 666
      )
    expect_equal(sum(test2$value), numberDeaths)

    test3 <- total_burden %>%
      filter(
        measure == "Deaths",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666
      )
    expect_equal(sum(test3$value), numberDeaths)
  })

  ### ----add columns---
  total_burden <- total_burden %>% dplyr::mutate(
    min_age = Single.Year.Ages.Code,
    max_age = Single.Year.Ages.Code,
    Single.Year.Ages.Code = NULL
  )
  total_burden <- total_burden %>% tibble::add_column(source = "nvss")
  #------filter ------
  #total_burden$Race %>% unique()
  total_burden <- total_burden %>%
    filter(Hispanic.Origin != "Unkown" & # TODO
      Race != "Guama")


  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
