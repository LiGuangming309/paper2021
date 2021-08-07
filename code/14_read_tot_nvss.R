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

  year <- 2016
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
causes <- read.csv(file.path(totalBurdenParsedDir, "causes.csv")) %>% filter(Year == year)

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

  if (year %in% 1990:1995) {
    selectcolumns <- c( #TODO year
      "Year" = "datayear",
      "label_cause" = "ucod", # record_1/enum_1
      #"Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age", # 64
      "Hispanic.Origin" = "hispanic" # 80 - 81
    )
  }else if (year %in% 1996:2002) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      #"Education" = "educ", # 52-53
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
     # "Education" = "educ", # 52-53
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
      "Education1989" = "educ1989", 
      "Education2003" = "educ2003", # 52-53
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
    # if(replacecolumnX == "STATEFP") browser()
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
        distinct()
      if (nrow(missing) > 0) {
        print(paste("no value assigned in", replacecolumnX, "for"))
        print(missing[, 1] %>% unique() %>% sort())
      }

      total_burden[, replacecolumnX] <- replacement %>% select(to)
    }
  }
  rm(findreplace, findreplace_sub, missing, replacement, replacecolumnX)
  #TODO Education
  if("Education2003" %in% colnames(total_burden)){
    total_burden <- total_burden %>%
      mutate(Education1989 = na_if(Education1989, "101"),
             Education2003 = na_if(Education2003, "101")) %>% 
      unite("Education", c("Education1989","Education2003"), na.rm = TRUE)
  }
  total_burden$Education %>% unique
  test <- total_burden %>% filter(#Education == "Unknown" |
                                    min_age == "Unknown" |
                                    Hispanic.Origin == "Unknown" |
                                    Race == "Unknown")
  100*nrow(test)/nrow(total_burden)
  # Deaths
  total_burden <- total_burden %>%
    group_by_at(colnames(total_burden)) %>%
    summarise(Deaths = n())


  ## --- seperate stuff----
  #inverse_selectcolumns <- c(names(selectcolumns)) #Education1989
  inverse_selectcolumns<- setdiff(colnames(total_burden),"Deaths")

  # add Hispanic Origin All Origins
  total_burden_all_his <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Hispanic.Origin = "All Origins")

  total_burden <- rbind(total_burden, total_burden_all_his) %>% distinct()
  rm(total_burden_all_his)

  # add Gender A
  total_burden_all_gend <- total_burden %>%
    group_by_at(setdiff(colnames(total_burden), c("Gender.Code","Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Gender.Code = "A")
  total_burden <- rbind(total_burden, total_burden_all_gend) %>% distinct()
  rm(total_burden_all_gend)

  #--- add all-cause rows---
  total_burden_all <- total_burden %>%
    group_by_at(setdiff(colnames(total_burden), c("label_cause","Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      label_cause = "all-cause",
      attr = "overall"
    )

  causes <- causes %>%
    group_by(to) %>%
    summarise(from = list(from))

  total_burden_cause <- total_burden %>% mutate(label_cause = substring(label_cause, 1, 3))
  total_burden_cause <- apply(causes, 1, function(cause) {
    label_cause1 <- cause$to
    icd10_cause <- cause$from %>% unlist()
    total_burden_cause <- total_burden_cause %>%
      filter(label_cause %in% icd10_cause) %>%
      group_by_at(setdiff(colnames(total_burden), c("label_cause","Deaths"))) %>%
      summarise(Deaths = sum(Deaths)) %>%
      mutate(
        label_cause = label_cause1,
        attr = "total"
      )
  }) %>% rbindlist()


  total_burden <- rbind(total_burden_all, total_burden_cause) %>% distinct()
  rm(total_burden_all, total_burden_cause)

  # seperate education, add "All Education"
  total_burden_race <- total_burden %>%
    group_by_at(setdiff(colnames(total_burden), c("Education","Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Education = as.factor(666)) 

  total_burden_all <- total_burden %>%
    filter(Hispanic.Origin == "All Origins") %>%
    group_by_at(setdiff(colnames(total_burden), c("Race","Education","Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Race = "All", Education = as.factor(666)) 
  
  if(year %in% 2009:2016){
    
    total_burden_educ <- total_burden %>%
      filter(Hispanic.Origin == "All Origins") %>%
      group_by_at(setdiff(colnames(total_burden), c("Race","Deaths"))) %>%
      summarise(Deaths = sum(Deaths)) %>%
      mutate(
        Race = "All",
        Education = Education %>% as.factor()
      )
    
    # deal with unknown, 1989
    #total_burden_educ <- total_burden_educ %>%
    #  group_by_at(setdiff(colnames(total_burden), c("Education","Deaths"))) %>%
    #  mutate(DeathsAllEduc = sum(Deaths)) %>%
    #  filter(Education %in% c(1:7)) %>%
    #  mutate(
    #    prop = DeathsAllEduc / sum(Deaths),
    #    Deaths = Deaths * prop,
    #    DeathsAllEduc = NULL, prop = NULL
    #  ) 
    total_burden <- rbind(total_burden_race, total_burden_educ, total_burden_all) %>% distinct()
    rm(total_burden_educ)
  }else{
    total_burden <- rbind(total_burden_race, total_burden_all) %>% distinct()
  }
  
  rm(total_burden_race, total_burden_all)
  #----test----
  total_burden <- total_burden %>% 
    as.data.frame() %>%
    ungroup()
  test_that("numbers add up", {
    expect_false(any(is.na(total_burden)))

    total_burden_test <- total_burden %>% select(setdiff(colnames(total_burden), c("Deaths", "attr")))
    total_burden_test <- total_burden_test[duplicated(total_burden_test), ]
    expect_equal(nrow(total_burden_test), 0)

    if(year %in% 2009:2016){
      test1 <- total_burden %>%
        dplyr::filter(
          Gender.Code == "A" &
            label_cause == "all-cause" &
            attr == "overall" &
            Race == "All"&
            Hispanic.Origin == "All Origins"&
            Education != 666
        )
      #some difference is tolerable due to rounding
      expect_equal(sum(test1$Deaths), numberDeaths, 
                   tolerance = 0.005, scale = numberDeaths) 
    }
    
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
  total_burden <- total_burden %>% distinct()
  # total_burden$Race %>% unique()
  
  100*sum(total_burden$Education == "Unknown")/nrow(total_burden)
  
  total_burden <- total_burden %>%
    filter(Hispanic.Origin != "Unknown" & # TODO
      # Race != "Guama" &
      min_age != "Unknown" &
      Education != "Unknown") %>% 
    mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))

  
  total_burden <- total_burden %>% unite("Ethnicity", c("Race", "Hispanic.Origin"), sep=", ", remove = F)
  
  interested_ethnicities <-c(
    "Black or African American, All Origins",
    "Asian or Pacific Islander, All Origins",
    "American Indian or Alaska Native, All Origins",
    "All, All Origins"
  )
  
  if(year %in% 1990:2000)interested_ethnicities <- c(interested_ethnicities, "White, All Origins")
  if(year %in% 2000:2016) interested_ethnicities <- c(interested_ethnicities, "White, Not Hispanic or Latino", "White, Hispanic or Latino")

  total_burden <- total_burden %>%
    filter(Ethnicity %in% interested_ethnicities)%>% 
    mutate(Ethnicity = NULL)

  total_burden <- total_burden %>% filter(Gender.Code == "A")
  if (agr_by != "nation") total_burden <- total_burden %>% filter(STATEFP != 0)

  # Only considering age above 25
  total_burden <- total_burden %>% filter(min_age >= 25)

  #--deal with education ---
  # Some studies using vital statistics data have also restricted analysis to ages 25–64 because of concerns about the accuracy
  # of death certificate education information for persons who died at older ages (9).
  # https://www.cdc.gov/nchs/data/series/sr_02/sr02_151.pdf

  #https://www.jstor.org/stable/pdf/3702065.pdf?refreqid=excelsior%3Aee6b8a77d645e5a84cb2e35b6ceabfd8 
  #total_burden <- total_burden %>% filter(Education == 666 | max_age <= 64)

  #---write csv---
  test_that("no na end read tot nvss", expect_false(any(is.na(total_burden))))

  total_burden <- total_burden %>% tibble::add_column(source = "nvss")
  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
