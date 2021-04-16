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
findreplace <- read.csv(file.path(totalBurdenParsedDir, "findreplace.csv")) %>% filter(Year == year)

totalBurdenDir <- file.path(totalBurdenDir, "nvss", paste0("mort", year, ".csv"))
totalBurdenParsedDir <- file.path(totalBurdenParsedDir, agr_by, "nvss")
dir.create(totalBurdenParsedDir, recursive = T, showWarnings = F)
totalBurdenParsedDir <- file.path(
  totalBurdenParsedDir,
  paste0("total_burden_nvss_", year, ".csv")
)

if (!file.exists(totalBurdenParsedDir)) {

  ## ----- read total burden ---------
  tic(paste("read", year, "total burden data"))
  total_burden <- fread(totalBurdenDir)
  numberDeaths <- nrow(total_burden)

  if (2000 <= year & year <= 2002) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      "Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age", # 64
      "Hispanic.Origin" = "hispanic" # 80 - 81
    )
  } else if (2003 <= year & year <= 2005) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      "Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age",
      "Hispanic.Origin" = "hspanicr" # 80 - 81
    )
  } else if (2005 <= year & year <= 2016) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      "Education" = "educ2003", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age",
      "Hispanic.Origin" = "hspanicr" # 80 - 81
    )
  }

  if (agr_by == "nation") {
    total_burden <- total_burden %>% tibble::add_column(nation = "us")
    selectcolumns <- c(selectcolumns, "nation" = "nation")
  } else {
    # All public-use micro-data files from 2005-present contain individual-level vital event data at the national level only.
    # Specifically, these files contain no geographic identifiers at the state, county, or city level.
    # see https://www.cdc.gov/nchs/nvss/dvs_data_release.htm
    if (year <= 2004) {
      if (agr_by == "STATEFP") {
        # if staters==0 (foreign resident), take state of occurance
        total_burden$staters <- apply(total_burden[, c("staters", "stateoc")], 1, function(row) {
          ifelse(row["staters"] != 0, row["staters"], row["stateoc"])
        })
        selectcolumns <- c(selectcolumns, "STATEFP" = "staters") # residence, not occurance
      } else if (agr_by == "county") {
        # if staters==0 (foreign resident), take state of occurance
        total_burden$staters <- apply(total_burden[, c("staters", "stateoc")], 1, function(row) ifelse(row["staters"] != 0, row["staters"], row["stateoc"]))
        total_burden$countyrs <- apply(total_burden[, c("countyrs", "countyoc")], 1, function(row) ifelse(row["countyrs"] != 0, row["countyrs"], row["countyoc"]))
        selectcolumns <- c(selectcolumns, "STATEFP" = "staters", "County.Code" = "countyrs") # residence, not occurance
      }
    } else {
      print(paste("in", year, "no geopgraphic identifier available"))
      quit()
    }
  }

  # https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
  total_burden <- total_burden %>% select(all_of(selectcolumns))

  #---------find and replace stuff--------
  for (replacecolumnX in findreplace$replacecolumns %>% unique()) {
    if (replacecolumnX %in% colnames(total_burden)) {
      findreplace_sub <- findreplace %>% filter(replacecolumns == replacecolumnX)

      replacement <- total_burden %>%
        select(all_of(replacecolumnX)) %>%
        mutate(across(everything(), as.character)) %>%
        left_join(findreplace_sub,
          by = setNames("from", replacecolumnX),
          na.replace = "oth"
        ) %>%
        mutate(to = replace_na(to, "oth"))

      if (replacecolumnX != "label_cause") {
        missing <- replacement %>% filter(to == "oth")
        if (nrow(missing) > 0) {
          print(paste("no value assigned in", replacecolumnX, "for"))
          print(missing[, 1] %>% unique() %>% sort())
        }
      }
      total_burden[, replacecolumnX] <- replacement %>% select(to)
    }
  }
  rm(findreplace, findreplace_sub, missing, replacement, replacecolumnX)

  # Deaths
  total_burden <- total_burden %>%
    group_by_at(colnames(total_burden)) %>%
    summarise(Deaths = n())


  ## --- seperate stuff----
  inverse_selectcolumns <- c(names(selectcolumns))
  # setdiff(colnames(total_burden),"Deaths")

  # seperate education, add "All Education"
  total_burden_race <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Education")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Education = 666) # TODO

   total_burden_educ <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, c("Hispanic.Origin", "Race"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      Hispanic.Origin = "All Origins",
      Race = "All",
      Education = as.numeric(Education)) 

  total_burden <- rbind(total_burden_race , total_burden_educ) %>% distinct()
  rm(total_burden_race, total_burden_educ)

  # add Hispanic Origin All Origins
  total_burden_all_his <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Hispanic.Origin = "All Origins")

  total_burden <- rbind(total_burden, total_burden_all_his) %>% distinct()
  rm(total_burden_all_his)

  #--- add all-cause rows---
  total_burden_all <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "label_cause")) %>%
    summarise(Deaths = sum(Deaths)) %>%
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
    expect_false(any(is.na(total_burden)))

    total_burden_test <- total_burden %>% select(setdiff(colnames(total_burden), c("Deaths")))
    total_burden_test <- total_burden_test[duplicated(total_burden_test), ]
    expect_equal(nrow(total_burden_test), 0)

     test1 <- total_burden %>%
      filter(
        label_cause == "all-cause",
        #attr == "overall",
        Race == "All",
        Hispanic.Origin == "All Origins",
        Education != 666
      )

     expect_equal(sum(test1$Deaths), numberDeaths) 

    test2 <- total_burden %>%
      filter(
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin == "All Origins",
        Education == 666
      )
    expect_equal(sum(test2$Deaths), numberDeaths)

    test3 <- total_burden %>%
      filter(
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666
      )
    expect_equal(sum(test3$Deaths), numberDeaths)
  })

  ## --write to csv----

  total_burden <- total_burden %>% tibble::add_column(source = "nvss")
  #------filter ------
  # total_burden$Race %>% unique()
  total_burden <- total_burden %>%
    filter(Hispanic.Origin != "Unknown" & # TODO
      Race != "Guama" &
      min_age != "Unknown")%>% 
    mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))


  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
