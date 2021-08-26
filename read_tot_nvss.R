#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 08/25/2021
# Purpose: read
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p, dependencies = TRUE)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

#### ----- Change paths here!---
totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden/nvss" # Where the files are stored
totalBurdenDir <- "~/Google Drive/currentDocumants/mixed Data/2020/2020job/HIGH/Progress/R code/Transfer/raw_restricted_fake" # Where the files are stored
totalBurdenParsedDir <- "~/Google Drive/currentDocumants/mixed Data/2020/2020job/HIGH/Progress/R code/Transfer" # Where the parsed files should be stored

#### ----- Change paths here!---
file_list <- list.files(totalBurdenDir)
agr_bys <- c("nation", "STATEFP")
years <- 1999:2016

findreplace <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv") 
causes <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv") 

for (agr_by in agr_bys) {
  for (year in years) {
    
    findreplaceX <- findreplace %>% filter(Year == year)
    causesX <- causes %>% filter(Year == year)
    totalBurdenDirX <- file.path(totalBurdenDir, file_list[grepl(year, file_list)])

    totalBurdenParsedDirX <- file.path(totalBurdenParsedDir, agr_by, "nvss")
    dir.create(totalBurdenParsedDirX, recursive = T, showWarnings = F)
    totalBurdenParsedDirX <- file.path(
      totalBurdenParsedDirX,
      paste0("total_burden_nvss_", year, ".csv")
    )

    if (!file.exists(totalBurdenParsedDirX)) {

      ## ----- read total burden ---------
      tic(paste("read", year, "total burden data"))
      #if (substr(totalBurdenDirX, nchar(totalBurdenDirX) - 3 + 1, nchar(totalBurdenDirX)) == "csv") {
        total_burden <- fread(totalBurdenDirX)
      #} else {
        # if ("narcan" %in% rownames(installed.packages()) == FALSE) {
        #  devtools::install_github("mkiang/narcan")
        # }
      #  total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year, fix_states = FALSE)
      #}

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
          "Hispanic.Origin" = "hispanic" # 80 - 81
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

      if("fipsctyr" %in% colnames(total_burden)){
        selectcolumns <- c(selectcolumns, "rural_urban_class"="fipsctyr")
      }else if(!("fipsctyr" %in% colnames(total_burden)) & "countyrs" %in% colnames(total_burden)){
        selectcolumns <- c(selectcolumns, "rural_urban_class"="countyrs")
        total_burden$countyrs %>% unique %>% sort
      }else{
        selectcolumns <- c(selectcolumns, "rural_urban_class"="rural_urban_class")
        total_burden$rural_urban_class <- NA
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
      rm(findreplaceX, findreplace_sub, missing, replacement, replacecolumnX)
      # TODO Education
      total_burden$rural_urban_class %>% unique()
      if ("Education2003" %in% colnames(total_burden)) {
        total_burden <- total_burden %>%
          mutate(
            Education1989 = na_if(Education1989, "101"),
            Education2003 = na_if(Education2003, "101")
          ) %>%
          unite("Education", c("Education1989", "Education2003"), na.rm = TRUE)
      }

      total_burden <- total_burden %>%
        group_by_at(colnames(total_burden)) %>%
        summarise(Deaths = n())


      ## --- seperate stuff----
      # inverse_selectcolumns <- c(names(selectcolumns)) #Education1989
      inverse_selectcolumns <- setdiff(colnames(total_burden), "Deaths")

      # add all rural_urban_class
      total_burden_all_urb <- total_burden %>%
        group_by_at(setdiff(inverse_selectcolumns, "rural_urban_class")) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(rural_urban_class = as.factor(666))
      total_burden <- rbind(total_burden, total_burden_all_urb) %>% distinct()
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

      causesX <- causesX %>%
        group_by(to) %>%
        summarise(from = list(from))

      total_burden_cause <- total_burden %>% mutate(label_cause = substring(label_cause, 1, 3))
      total_burden_cause <- apply(causesX, 1, function(cause) {
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

      if (year %in% 2009:2016) {
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
            tolerance = 0.005, scale = numberDeaths
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

        total_burden$rural_urban_class %>% unique()
        test5 <- total_burden %>%
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

      if (year %in% 1990:2000) interested_ethnicities <- c(interested_ethnicities, "White, All Origins")
      if (year %in% 2000:2016) interested_ethnicities <- c(interested_ethnicities, "White, Not Hispanic or Latino", "White, Hispanic or Latino")

      total_burden <- total_burden %>%
        filter(Ethnicity %in% interested_ethnicities) %>%
        mutate(Ethnicity = NULL)

      total_burden <- total_burden %>% filter(Gender.Code == "A")
      if (agr_by != "nation") total_burden <- total_burden %>% filter(STATEFP != 0)

      # Only considering age above 25
      total_burden <- total_burden %>% filter(min_age >= 25)

      #---write csv---
      test_that("no na end read tot nvss", expect_false(any(is.na(total_burden))))

      total_burden <- total_burden %>% tibble::add_column(source = "nvss")
      fwrite(total_burden, totalBurdenParsedDirX)
      toc()
    }
  }
}
