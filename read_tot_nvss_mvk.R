#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 08/25/2021
# Purpose: read mortality counts
#
#***************************************************************************
#*

# clear memory
# rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("magrittr", "data.table", "testthat", "tidyverse", "tictoc")

library(foreach)
library(doParallel)
NCORES <- 12

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p, dependencies = TRUE)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

#### ----- Change paths here!---
# Where the files are stored
totalBurdenDir <- "./raw_restricted_data"
# Where the parsed files should be stored
totalBurdenParsedDir <- "./Transfer_for_daniel"

#### ----- Change paths here!---
file_list <- list.files(totalBurdenDir)
agr_bys <- c("nation", "STATEFP")
years <- 2009:2016

findreplace <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv")
causes <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv")

doParallel::registerDoParallel(cores = NCORES)
foreach::foreach(year = years, .inorder = FALSE) %dopar% {
  findreplaceX <- findreplace %>% filter(Year == year)
  causesX <- causes %>% filter(Year == year)
  totalBurdenDirX <- file.path(totalBurdenDir, file_list[grepl(year, file_list)])

  ## ----- read total burden ---------
  total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year = year, fix_states = FALSE)

  numberDeaths <- nrow(total_burden)
  for (agr_by in agr_bys) {
    totalBurdenParsedDirX <- file.path(totalBurdenParsedDir, agr_by, "nvss")
    dir.create(totalBurdenParsedDirX, recursive = T, showWarnings = F)
    totalBurdenParsedDirX <- file.path(
      totalBurdenParsedDirX,
      paste0("total_burden_nvss_", year, ".csv")
    )

    if (!file.exists(totalBurdenParsedDirX)) {
      tic(paste("read", year, "total burden data"))

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
      } else if (year %in% 2003:2005) {
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
      } else if (year %in% 2006:2016) {
        selectcolumns <- c(
          "Year" = "year",
          "label_cause" = "ucod", # record_1/enum_1
          "Education1989" = "educ89",
          "Education2003" = "educ", # 52-53
          # "Education1989" = "educ1989",
          # "Education2003" = "educ2003", # 52-53
          "Gender.Code" = "sex", # 59
          "Race" = "race", # 60
          "min_age" = "age", # 64, Single Year
          "max_age" = "age",
          "Hispanic.Origin" = "hspanicr" # 80 - 81
        )
      }

      if ("fipsctyr" %in% colnames(total_burden)) {
        selectcolumns <- c(selectcolumns, "rural_urban_class" = "fipsctyr")
      } else if (!("fipsctyr" %in% colnames(total_burden)) & "countyrs" %in% colnames(total_burden)) {
        selectcolumns <- c(selectcolumns, "rural_urban_class" = "countyrs")
        total_burden$countyrs %>%
          unique() %>%
          sort()
      } else {
        selectcolumns <- c(selectcolumns, "rural_urban_class" = "rural_urban_class")
        total_burden$rural_urban_class <- NA
      }

      if (agr_by == "nation") {
        total_burden <- total_burden %>% tibble::add_column(nation = "us")
        selectcolumns <- c(selectcolumns, "nation" = "nation")
      } else if(agr_by == "STATEFP"){
        #total_burden$staters <- apply(total_burden[, c("staters", "stateoc")], 1, function(row) {
        #  ifelse(row["staters"] != 0, row["staters"], row["stateoc"])
      #  })
        if(!"staters" %in% colnames(total_burden)){
          names(total_burden)
          total_burden <- total_burden %>% mutate(staters = str_sub(countyrs, 1, -4) %>% as.integer())
        }
        selectcolumns <- c(selectcolumns, "STATEFP" = "staters") # residence, not occurance
      }

      # https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
      total_burdenX <- total_burden %>% select(all_of(selectcolumns))

      #---------find and replace stuff--------
      for (replacecolumnX in findreplaceX$replacecolumns %>% unique()) {
        # if(replacecolumnX == "STATEFP") browser()
        if (replacecolumnX %in% colnames(total_burdenX)) {
          findreplace_sub <- findreplaceX %>% filter(replacecolumns == replacecolumnX)

          replacement <- total_burdenX %>%
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

          total_burdenX[, replacecolumnX] <- replacement %>% select(to)
        }
      }
      rm(findreplace_sub, missing, replacement, replacecolumnX)
      # TODO Education
      total_burdenX$rural_urban_class %>% unique()
      if ("Education2003" %in% colnames(total_burdenX)) {
        total_burdenX <- total_burdenX %>%
          mutate(
            Education1989 = na_if(Education1989, "101"),
            Education2003 = na_if(Education2003, "101")
          ) %>%
          unite("Education", c("Education1989", "Education2003"), na.rm = TRUE)
      }

      total_burdenX <- total_burdenX %>%
        group_by_at(colnames(total_burdenX)) %>%
        summarise(Deaths = n())


      ## --- seperate stuff----
      # inverse_selectcolumns <- c(names(selectcolumns)) #Education1989
      inverse_selectcolumns <- setdiff(colnames(total_burdenX), "Deaths")

      # add all rural_urban_class
      total_burdenX_all_urb <- total_burdenX %>%
        group_by_at(setdiff(inverse_selectcolumns, "rural_urban_class")) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(rural_urban_class = as.factor(666))
      total_burdenX <- rbind(total_burdenX, total_burdenX_all_urb) %>% distinct()
      rm(total_burdenX_all_urb)

      # add Hispanic Origin All Origins
      total_burdenX_all_his <- total_burdenX %>%
        group_by_at(setdiff(inverse_selectcolumns, "Hispanic.Origin")) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(Hispanic.Origin = "All Origins")

      total_burdenX <- rbind(total_burdenX, total_burdenX_all_his) %>% distinct()
      rm(total_burdenX_all_his)

      # add Gender A
      total_burdenX_all_gend <- total_burdenX %>%
        group_by_at(setdiff(colnames(total_burdenX), c("Gender.Code", "Deaths"))) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(Gender.Code = "A")
      total_burdenX <- rbind(total_burdenX, total_burdenX_all_gend) %>% distinct()
      rm(total_burdenX_all_gend)

      #--- add all-cause rows---
      total_burdenX_all <- total_burdenX %>%
        group_by_at(setdiff(colnames(total_burdenX), c("label_cause", "Deaths"))) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(
          label_cause = "all-cause",
          attr = "overall"
        )

      causesX <- causesX %>%
        group_by(to) %>%
        summarise(from = list(from))

      total_burdenX_cause <- total_burdenX %>% mutate(label_cause = substring(label_cause, 1, 3))
      total_burdenX_cause <- apply(causesX, 1, function(cause) {
        label_cause1 <- cause$to
        icd10_cause <- cause$from %>% unlist()
        total_burdenX_cause <- total_burdenX_cause %>%
          filter(label_cause %in% icd10_cause) %>%
          group_by_at(setdiff(colnames(total_burdenX), c("label_cause", "Deaths"))) %>%
          summarise(Deaths = sum(Deaths)) %>%
          mutate(
            label_cause = label_cause1,
            attr = "total"
          )
      }) %>% rbindlist()


      total_burdenX <- rbind(total_burdenX_all, total_burdenX_cause) %>% distinct()
      rm(total_burdenX_all, total_burdenX_cause)

      # seperate education, add "All Education"
      total_burdenX_race <- total_burdenX %>%
        group_by_at(setdiff(colnames(total_burdenX), c("Education", "Deaths"))) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(Education = as.factor(666))

      total_burdenX_all <- total_burdenX %>%
        filter(Hispanic.Origin == "All Origins") %>%
        group_by_at(setdiff(colnames(total_burdenX), c("Race", "Education", "Deaths"))) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(Race = "All", Education = as.factor(666))

      if (year %in% 2009:2016) {
        total_burdenX_educ <- total_burdenX %>%
          filter(Hispanic.Origin == "All Origins") %>%
          group_by_at(setdiff(colnames(total_burdenX), c("Race", "Deaths"))) %>%
          summarise(Deaths = sum(Deaths)) %>%
          mutate(
            Race = "All",
            # Education = Education %>% as.factor()
          ) %>%
          mutate(Education = as.factor(666))

        total_burdenX <- rbind(total_burdenX_race, total_burdenX_educ, total_burdenX_all) %>% distinct()
        rm(total_burdenX_educ)
      } else {
        total_burdenX <- rbind(total_burdenX_race, total_burdenX_all) %>% distinct()
      }

      rm(total_burdenX_race, total_burdenX_all)
      #----test----
      total_burdenX <- total_burdenX %>%
        as.data.frame() %>%
        ungroup()
      test_that("numbers add up", {
        expect_false(any(is.na(total_burdenX)))

        total_burdenX_test <- total_burdenX %>% select(setdiff(colnames(total_burdenX), c("Deaths", "attr")))
        total_burdenX_test <- total_burdenX_test[duplicated(total_burdenX_test), ]
        expect_equal(nrow(total_burdenX_test), 0)

        if (year %in% 2009:2016) {
          test1 <- total_burdenX %>%
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

        test2 <- total_burdenX %>%
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

        test3 <- total_burdenX %>%
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

        test4 <- total_burdenX %>%
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

        total_burdenX$rural_urban_class %>% unique()
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
      })

      #------filter ------
      total_burdenX <- total_burdenX %>% distinct()

      total_burdenX <- total_burdenX %>%
        filter(Hispanic.Origin != "Unknown" & # TODO
          # Race != "Guama" &
          min_age != "Unknown" &
          Education != "Unknown" &
          rural_urban_class != "oth") %>%
        mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))


      total_burdenX <- total_burdenX %>% unite("Ethnicity", c("Race", "Hispanic.Origin"), sep = ", ", remove = F)

      interested_ethnicities <- c(
        "Black or African American, All Origins",
        "Asian or Pacific Islander, All Origins",
        "American Indian or Alaska Native, All Origins",
        "All, All Origins"
      )

      if (year %in% 1990:1999) interested_ethnicities <- c(interested_ethnicities, "White, All Origins")
      if (year == 2000) interested_ethnicities <- c(interested_ethnicities, "White, All Origins", "White, Not Hispanic or Latino", "White, Hispanic or Latino")
      if (year %in% 2001:2016) interested_ethnicities <- c(interested_ethnicities, "White, Not Hispanic or Latino", "White, Hispanic or Latino")
      
      total_burdenX <- total_burdenX %>%
        filter(Ethnicity %in% interested_ethnicities) %>%
        mutate(Ethnicity = NULL)

      total_burdenX <- total_burdenX %>% filter(Gender.Code == "A")
      if (agr_by != "nation") total_burdenX <- total_burdenX %>% filter(STATEFP != 0)

      # Only considering age above 25
      total_burdenX <- total_burdenX %>% filter(min_age >= 25)

      #---write csv---
      test_that("no na end read tot nvss", expect_false(any(is.na(total_burdenX))))

      total_burdenX <- total_burdenX %>% tibble::add_column(source = "nvss")
      fwrite(total_burdenX, totalBurdenParsedDirX)
      toc()
    }
  }
}
doParallel::stopImplicitCluster()
closeAllConnections()
