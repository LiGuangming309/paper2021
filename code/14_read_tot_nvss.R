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

agr_by <- args[10]
totalBurdenDir <- args[12]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  year <- 1990
  
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"

   #totalBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/08_total_burden"
   #totalBurdenParsedDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_total_burden_parsed"
}
findreplace <- read.csv(file.path(totalBurdenParsedDir, "findreplace.csv")) %>% filter(Year == year)
causes <- read.csv(file.path(totalBurdenParsedDir, "causes.csv")) %>% filter(Year == year)

totalBurdenDir <- file.path(totalBurdenDir, "nvss")
file_list <- list.files(totalBurdenDir)
totalBurdenDir <- file.path(
  totalBurdenDir,
  file_list[grepl(year, file_list)]
)
rm(file_list)


totalBurdenParsedDir <- file.path(totalBurdenParsedDir, agr_by, "nvss")
dir.create(totalBurdenParsedDir, recursive = T, showWarnings = F)
totalBurdenParsedDir <- file.path(
  totalBurdenParsedDir,
  paste0("total_burden_nvss_", year, ".csv")
)

if (!file.exists(totalBurdenParsedDir)) {
  tic(paste("read", year, "total burden data"), quiet = FALSE)
  
  total_burden <- fread(totalBurdenDir)
  #print(paste("read", year, "total burden data"))
  
  findreplaceX <- findreplace %>% filter(Year == year)
  causesX <- causes %>% filter(Year == year)
  numberDeaths <- nrow(total_burden)
  
  if (year %in% 1990:1995) {
    selectcolumns <- c( # TODO year
      "Year" = "datayear",
      "label_cause" = "ucod", # record_1/enum_1
      # "Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age", # 64
      "Hispanic.Origin" = "hispanic", # 80 - 81
      "interested_state" = "staters"
    )
  } else if (year %in% 1996:2002) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      # "Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age", # 64
      "Hispanic.Origin" = "hispanic", # 80 - 81
      "interested_state" = "staters"
    )
  } else if (year %in% 2003:2005) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      # "Education" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age",
      "Hispanic.Origin" = "hspanicr", # 80 - 81
      "interested_state" = "staters"
    )
  } else if (year %in% 2006:2008) {
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age",
      "Hispanic.Origin" = "hspanicr", # 80 - 81
      "interested_state" = "staters"
    )
  } else if (year %in% 2009:2016) {
    # For the years 2009:2016 we have population estimates by Education
    selectcolumns <- c(
      "Year" = "year",
      "label_cause" = "ucod", # record_1/enum_1
      "Education1989" = "educ89",
      "Education2003" = "educ", # 52-53
      "Gender.Code" = "sex", # 59
      "Race" = "race", # 60
      "min_age" = "age", # 64, Single Year
      "max_age" = "age",
      "Hispanic.Origin" = "hspanicr", # 80 - 81
      "interested_state" = "staters"
    )
  }
  
  #initialize staters column if missing
  if (!"staters" %in% colnames(total_burden)) total_burden$staters <- NA
  
  if ("fipsctyr" %in% colnames(total_burden)) {
    selectcolumns <- c(selectcolumns, "rural_urban_class" = "fipsctyr")
    
    # replace missing information from other columns as good as possible
    total_burden <- total_burden %>%
      mutate(
        #use place of occurrence if place of place of residence not available
        fipsctyr = na_if(fipsctyr, 0),
        fipsctyr = coalesce(fipsctyr, fipsctyo),
        countyrs = na_if(countyrs, 0),
        countyrs = coalesce(countyrs, as.character(countyoc)),
        staters = coalesce(
          staters,
          str_sub(countyrs, 1, -4)  %>% as.integer()
        )
      )
  } else if (!("fipsctyr" %in% colnames(total_burden)) & "countyrs" %in% colnames(total_burden)) {
    selectcolumns <- c(selectcolumns, "rural_urban_class" = "countyrs")
    
    # replace missing information from other columns as good as possible
    total_burden <- total_burden %>% 
      mutate(
        #use place of occurrence if place of place of residence not available
        countyrs = na_if(countyrs, 0),
        countyrs = coalesce(countyrs, countyoc),
        staters = coalesce(
          staters,
          str_sub(countyrs, 1, -4) 
        )
      )
  } else {
    selectcolumns <- c(selectcolumns, "rural_urban_class" = "rural_urban_class")
    total_burden$rural_urban_class <- NA
  }
  
  if (agr_by == "nation") {
    total_burden <- total_burden %>% tibble::add_column(nation = "us")
    selectcolumns <- c(selectcolumns, "nation" = "nation")
  } else if (agr_by == "STATEFP") {
    selectcolumns <- c(selectcolumns, "STATEFP" = "staters") # residence, not occurance
  }else if (agr_by == "county") {
    if ("fipsctyr" %in% colnames(total_burdenX)) {
      selectcolumns <- c(selectcolumns, "county" = "fipsctyr")
    }else if (!("fipsctyr" %in% colnames(total_burdenX)) & "countyrs" %in% colnames(total_burdenX)){
      selectcolumns <- c(selectcolumns, "county" = "countyrs")
    }
  }
  
  # https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
  total_burden <- total_burden %>% select(all_of(selectcolumns))
  
  ## Test print
  #print(total_burden, n = 5)
  
  #---------find and replace stuff--------
  for (replacecolumnX in findreplaceX$replacecolumns %>% unique()) {
    # if(replacecolumnX == "STATEFP") browser()
    if (replacecolumnX %in% colnames(total_burden)) {
      findreplace_sub <- findreplaceX %>% filter(replacecolumns == replacecolumnX)
      
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
        print(missing[, 1] %>% unique())
      }
      
      total_burden[, replacecolumnX] <- replacement %>% select(to)
    }
  }
  rm(findreplace_sub, missing, replacement, replacecolumnX)
  # TODO Education
  total_burden$rural_urban_class %>% unique()
  if ("Education2003" %in% colnames(total_burden)) {
    total_burden <- total_burden %>%
      mutate(
        Education1989 = na_if(Education1989, "101"),
        Education2003 = na_if(Education2003, "101"),
        Education = coalesce(Education2003, Education1989),
        Education = replace_na(Education, "Unknown"), # TODO
        Education1989 = NULL, Education2003 = NULL
      )
  }
  
  total_burden <- total_burden %>%
    group_by_at(colnames(total_burden)) %>%
    summarise(Deaths = n())
  
  #print(1 - sum(as.integer(total_burden$interested_state)) / nrow(total_burden))
  ## --- seperate stuff----
  # inverse_selectcolumns <- c(names(selectcolumns)) #Education1989
  inverse_selectcolumns <- setdiff(colnames(total_burden), "Deaths")
  
  # add all rural_urban_class
  total_burden_all_urb <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "rural_urban_class")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(rural_urban_class = as.factor(666))
  if(agr_by == "county") {
    total_burdenX <- total_burdenX_all_urb %>% distinct()
  }else{
    total_burdenX <- rbind(total_burdenX, total_burdenX_all_urb) %>% distinct()
  }
  rm(total_burden_all_urb)
  
  # add Hispanic Origin All Origins
  total_burden_all_his <- total_burden %>%
    group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Hispanic.Origin = "All Origins")
  
  total_burden <- rbind(total_burden, total_burden_all_his) %>% distinct()
  rm(total_burden_all_his)
  
  # add Gender A
  total_burden_all_gend <- total_burden %>%
    group_by_at(setdiff(colnames(total_burden), c("Gender.Code", "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Gender.Code = "A")
  total_burden <- rbind(total_burden, total_burden_all_gend) %>% distinct()
  rm(total_burden_all_gend)
  
  #--- add all-cause rows---
  total_burden_all <- total_burden %>%
    group_by_at(setdiff(colnames(total_burden), c("label_cause", "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      label_cause = "all-cause",
      attr = "overall"
    )
  
  causesX2 <- causesX %>%
    group_by(to) %>%
    summarise(from = list(from))
  
  total_burden_cause <- total_burden %>% mutate(label_cause = substring(label_cause, 1, 3))
  total_burden_cause <- apply(causesX2, 1, function(cause) {
    label_cause1 <- cause$to
    icd10_cause <- cause$from %>% unlist()
    total_burden_cause <- total_burden_cause %>%
      filter(label_cause %in% icd10_cause) %>%
      group_by_at(setdiff(colnames(total_burden), c("label_cause", "Deaths"))) %>%
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
    group_by_at(setdiff(colnames(total_burden), c("Education", "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Education = as.factor(666))
  
  total_burden_all <- total_burden %>%
    filter(Hispanic.Origin == "All Origins") %>%
    group_by_at(setdiff(colnames(total_burden), c("Race", "Education", "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(Race = "All", Education = as.factor(666))
  
  # For the years 2009:2016 we have population estimates by Education
  if ("Education" %in% colnames(total_burden)) {
    total_burden_educ <- total_burden %>%
      filter(Hispanic.Origin == "All Origins") %>%
      group_by_at(setdiff(colnames(total_burden), c("Race", "Deaths"))) %>%
      summarise(Deaths = sum(Deaths)) %>%
      mutate(
        Race = "All",
        Education = Education %>% as.factor()
      )
    
    total_burden <- rbind(total_burden_race, total_burden_educ, total_burden_all) %>% distinct()
    rm(total_burden_educ)
  } else {
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
    
    if (year %in% 2009:2016) {
      test1 <- total_burden %>%
        dplyr::filter(
          Gender.Code == "A" &
            label_cause == "all-cause" &
            attr == "overall" &
            Race == "All" &
            Hispanic.Origin == "All Origins" &
            Education != 666 &
            rural_urban_class == 666
        )
      # some difference is tolerable due to rounding
      expect_equal(sum(test1$Deaths), numberDeaths,
                   #tolerance = 0.005, scale = numberDeaths
      )
    }
    
    test2 <- total_burden %>%
      filter(
        Gender.Code == "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin == "All Origins",
        Education == 666,
        rural_urban_class == 666
      )
    expect_equal(sum(test2$Deaths), numberDeaths)
    
    test3 <- total_burden %>%
      filter(
        Gender.Code == "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666,
        rural_urban_class == 666
      )
    expect_equal(sum(test3$Deaths), numberDeaths)
    
    test4 <- total_burden %>%
      filter(
        Gender.Code != "A",
        label_cause == "all-cause",
        attr == "overall",
        Race != "All",
        Hispanic.Origin != "All Origins",
        Education == 666,
        rural_urban_class == 666
      )
    expect_equal(sum(test4$Deaths), numberDeaths)
    
    if(agr_by != "county"){
      test5 <- total_burdenX %>%
        filter(
          Gender.Code != "A",
          label_cause == "all-cause",
          attr == "overall",
          Race == "All",
          Hispanic.Origin == "All Origins",
          Education == 666,
          rural_urban_class != 666
        )
      expect_equal(sum(test5$Deaths), numberDeaths)
    }
  })
  
  #------filter ------
  total_burden <- total_burden %>% distinct()
  
  total_burden <- total_burden %>%
    filter(Hispanic.Origin != "Unknown" & # TODO
             # Race != "Guama" &
             min_age != "Unknown" &
             Education != "Unknown" &
             rural_urban_class != "oth") %>%
    mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))
  
  total_burden <- total_burden %>% unite("Ethnicity", c("Race", "Hispanic.Origin"), sep = ", ", remove = F)
  
  interested_ethnicities <- c(
    "Black or African American, All Origins",
    "Asian or Pacific Islander, All Origins",
    "American Indian or Alaska Native, All Origins",
    "All, All Origins"
  )
  
  if (year %in% 1990:1999) interested_ethnicities <- c(interested_ethnicities, "White, All Origins")
  if (year == 2000) interested_ethnicities <- c(interested_ethnicities, "White, All Origins", "White, Not Hispanic or Latino", "White, Hispanic or Latino")
  if (year %in% 2001:2016) interested_ethnicities <- c(interested_ethnicities, "White, Not Hispanic or Latino", "White, Hispanic or Latino")
  
  total_burden <- total_burden %>%
    filter(Ethnicity %in% interested_ethnicities) %>%
    mutate(Ethnicity = NULL)
  
  total_burden <- total_burden %>% filter(Gender.Code == "A")
  total_burden <- total_burden %>%
    filter(interested_state == 1) %>%
    mutate(interested_state = NULL)
  
  if("STATEFP" %in% colnames(total_burden)) total_burden <- total_burden %>% filter(STATEFP != "oth") #TODO
  
  # Only considering age above 25
  total_burden <- total_burden %>% filter(min_age >= 25)
  
  #---write csv---
  test_that("no na end read tot nvss", expect_false(any(is.na(total_burden))))
  
  total_burden <- total_burden %>% tibble::add_column(source = "nvss")
  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}