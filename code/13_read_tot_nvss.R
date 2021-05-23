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

  year <- 2009
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"

  dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/08_total_burden"
  totalBurdenParsedDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_total_burden_parsed"
}
findreplace <- read.csv(file.path(totalBurdenParsedDir, "findreplace.csv")) %>% filter(Year == year)
causes <- read.csv(file.path(totalBurdenParsedDir, "causes.csv")) 

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
    #if(replacecolumnX == "STATEFP") browser()
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

        missing <- replacement %>% 
          filter(to == "oth") %>%
          distinct
        if (nrow(missing) > 0) {
          print(paste("no value assigned in", replacecolumnX, "for"))
          print(missing[, 1] %>% unique() %>% sort())
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

  # add Hispanic Origin All Origins
  total_burden_all_his <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Hispanic.Origin = "All Origins")

  total_burden <- rbind(total_burden, total_burden_all_his) %>% distinct()
  rm(total_burden_all_his)

  #add Gender A
  total_burden_all_gend <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Gender.Code")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Gender.Code = "A")
  total_burden <- rbind(total_burden, total_burden_all_gend) %>% distinct()
  rm(total_burden_all_gend)
  
  # seperate education, add "All Education"
  total_burden_race <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Education")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Education = 666) # TODO
  
  total_burden_educ <- total_burden %>%
    filter(Hispanic.Origin == "All Origins") %>%
    group_by_at(setdiff(inverse_selectcolumns, c("Race"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      Race = "All",
      Education = as.numeric(Education)) 
  
  total_burden <- rbind(total_burden_race , total_burden_educ) %>% distinct()
  rm(total_burden_race, total_burden_educ)
  
  #--- add all-cause rows---
  total_burden_all <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "label_cause")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      label_cause = "all-cause",
      attr = "overall"
    )

  causes <- causes %>%
    group_by(to) %>%
    summarise(from = list(from))
  
  total_burden_cause <- total_burden %>% mutate(label_cause = substring(label_cause, 1, 3))
  total_burden_cause <- apply(causes, 1, function(cause){
    label_cause1 <- cause$to
    icd10_cause <- cause$from %>% unlist
    total_burden_cause <- total_burden_cause %>%
      filter(label_cause %in% icd10_cause) %>%
      group_by_at(setdiff(inverse_selectcolumns, "label_cause")) %>%
      summarise(Deaths = sum(Deaths)) %>%
      mutate(label_cause = label_cause1,
             attr = "total")
  }) %>% rbindlist
  
    
  total_burden <- rbind(total_burden_all, total_burden_cause) %>% distinct()
  rm(total_burden_all, total_burden_cause)

  #----test----
  total_burden <- total_burden %>% as.data.frame()
  test_that("numbers add up", {
    expect_false(any(is.na(total_burden)))

    total_burden_test <- total_burden %>% select(setdiff(colnames(total_burden), c("Deaths", "attr")))
    total_burden_test <- total_burden_test[duplicated(total_burden_test), ]
    expect_equal(nrow(total_burden_test), 0)
    
     test1 <- total_burden %>%
      filter(
        Gender.Code == "A",
        label_cause == "all-cause",
        attr == "overall",
        Race == "All",
        Hispanic.Origin == "All Origins",
        Education != 666
      )
     expect_equal(sum(test1$Deaths), numberDeaths) 

    test2 <- total_burden %>%
      filter(
        Gender.Code == "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin == "All Origins",
        Education == 666
      )
    expect_equal(sum(test2$Deaths), numberDeaths)

    test3 <- total_burden %>%
      filter(
        Gender.Code == "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666
      )
    expect_equal(sum(test3$Deaths), numberDeaths)
    
    test4 <- total_burden %>%
      filter(
        Gender.Code != "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666
      )
    expect_equal(sum(test4$Deaths), numberDeaths)
  })
  
  #------filter ------
  # total_burden$Race %>% unique()
  total_burden <- total_burden %>%
    filter(Hispanic.Origin != "Unknown" & # TODO
      #Race != "Guama" &
      min_age != "Unknown")%>% 
    mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))

  #Only considering Education groups for above 25
  #total_burden <- total_burden %>%  filter(Education == 666 | min_age >= 25)
  total_burden <- total_burden %>%
    mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
    filter(Ethnicity %in% c(
      "White, Not Hispanic or Latino",
      "White, Hispanic or Latino",
      "Black or African American, All Origins",
      "Asian or Pacific Islander, All Origins",
      "American Indian or Alaska Native, All Origins",
      "All, All Origins"
    ))  %>%
    mutate(Ethnicity = NULL)
  
  if(agr_by == "STATEFP") total_burden <- total_burden %>% filter(STATEFP != 0)

  #Only considering age above 25
  total_burden <- total_burden %>% filter(min_age >= 25)
  
  #--deal with education ---
  #Some studies using vital statistics data have also restricted analysis to ages 25–64 because of concerns about the accuracy
  #of death certificate education information for persons who died at older ages (9).
  # https://www.cdc.gov/nchs/data/series/sr_02/sr02_151.pdf
  total_burden_race <- total_burden %>% filter(Education == 666)
  total_burden_educ <- total_burden %>% filter(Education != 666 & max_age <= 64)
  
  prop_education_unknown <- total_burden_educ %>%
    group_by_at(setdiff(inverse_selectcolumns, c("Education", "Deaths"))) %>%
    mutate(propDeaths = Deaths/sum(Deaths), Deaths = NULL) %>%
    filter(Education %in% 9:10) %>%
    group_by_at(setdiff(inverse_selectcolumns, c("Education","propDeaths"))) %>%
    summarise(propDeaths = sum(propDeaths))
  
  setdiff(inverse_selectcolumns, "Education")
  colnames(total_burden_educ)
  colnames(prop_education_unknown)
  
  total_burden_educ <- total_burden_educ %>% filter(Education %in% c(1:7))
  total_burden_educ <- left_join(total_burden_educ, prop_education_unknown,
                                  by = setdiff(inverse_selectcolumns, "Education")) %>%
    mutate(propDeaths = replace_na(propDeaths, 0),
           Deaths = Deaths/(1-propDeaths),
           propDeaths = NULL)
  
  #counter, above effect
  
  total_burden <- rbind(total_burden_race, total_burden_educ)
  #---write csv---
  test_that("no na end read tot nvss", expect_false(any(is.na(total_burden))))
  
  total_burden <- total_burden %>% tibble::add_column(source = "nvss")
  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
