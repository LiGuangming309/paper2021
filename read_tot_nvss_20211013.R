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
NCORES <- 10

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

dir.create(sprintf("%s/logs", totalBurdenParsedDir), recursive = TRUE, showWarnings = FALSE)

#totalBurdenDir <- "/Users/default/Desktop/paper2021/raw_restricted_fake"
# Where the parsed files should be stored
#totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/raw_restricted_fake"

#### ----- ---
file_list <- list.files(totalBurdenDir)
#agr_bys <- c("nation", "STATEFP")#county
agr_bys <- c("county","nation", "STATEFP")
years <- 1999

findreplace <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/findreplace.csv")
causes <- read.csv("https://raw.github.com/FridljDa/paper2021/master/data/09_total_burden_parsed/causes.csv")

#### ----- loop over everything---
 doParallel::registerDoParallel(cores = NCORES)
 foreach::foreach(year = years, .inorder = FALSE) %dopar% {
#for (year in years) {
  findreplaceX <- findreplace %>% filter(Year == year)

  totalBurdenDirX <- file.path(totalBurdenDir, file_list[grepl(year, file_list)])
  ## ----- read total burden ---------
   total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year = year, fix_states = FALSE)
  #total_burden <- data.table::fread(cmd = paste("unzip -p", totalBurdenDirX))

  ## Open log -- assume file import went fine.
  sink(sprintf("%s/logs/log_%s.txt", totalBurdenParsedDir, Sys.getpid()), append = TRUE)
  print(sprintf("Starting %s: %s", year, basename(totalBurdenDirX)))

  causesX <- causes %>% filter(Year == year)
  numberDeaths <- nrow(total_burden)

  for (agr_by in agr_bys) {
    total_burdenX <- total_burden

    totalBurdenParsedDirX <- file.path(totalBurdenParsedDir, agr_by, "nvss")
    dir.create(totalBurdenParsedDirX, recursive = T, showWarnings = F)
    totalBurdenParsedDirX <- file.path(
      totalBurdenParsedDirX,
      paste0("total_burden_nvss_", year, ".csv")
    )

    if (!file.exists(totalBurdenParsedDirX)) {
      tic(paste("read", year, "total burden data"), quiet = FALSE)
      print(paste("read", year, "total burden data"))

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
      if (!"staters" %in% colnames(total_burdenX)) total_burdenX$staters <- NA
      
      if ("fipsctyr" %in% colnames(total_burdenX)) {
        selectcolumns <- c(selectcolumns, "rural_urban_class" = "fipsctyr")

        # replace missing information from other columns as good as possible
        total_burdenX <- total_burdenX %>%
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
      } else if (!("fipsctyr" %in% colnames(total_burdenX)) & "countyrs" %in% colnames(total_burdenX)) {
        selectcolumns <- c(selectcolumns, "rural_urban_class" = "countyrs")

        # replace missing information from other columns as good as possible
        total_burdenX <- total_burdenX %>% 
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
        total_burdenX$rural_urban_class <- NA
      }
      
      if (agr_by == "nation") {
        total_burdenX <- total_burdenX %>% tibble::add_column(nation = "us")
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
      total_burdenX <- total_burdenX %>% select(all_of(selectcolumns))
      
      ## Test print
      print(total_burdenX, n = 5)

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
            Education2003 = na_if(Education2003, "101"),
            Education = coalesce(Education2003, Education1989),
            Education = replace_na(Education, "Unknown"), # TODO
            Education1989 = NULL, Education2003 = NULL
          )
      }

      total_burdenX <- total_burdenX %>%
        group_by_at(colnames(total_burdenX)) %>%
        summarise(Deaths = n())

      #print(1 - sum(as.integer(total_burdenX$interested_state)) / nrow(total_burdenX))
      ## --- seperate stuff----
      # inverse_selectcolumns <- c(names(selectcolumns)) #Education1989
      inverse_selectcolumns <- setdiff(colnames(total_burdenX), "Deaths")

      # add all rural_urban_class
      total_burdenX_all_urb <- total_burdenX %>%
        group_by_at(setdiff(inverse_selectcolumns, "rural_urban_class")) %>%
        summarise(Deaths = sum(Deaths)) %>%
        mutate(rural_urban_class = as.factor(666))
      
      if(agr_by == "county") {
        total_burdenX <- total_burdenX_all_urb %>% distinct()
      }else{
        total_burdenX <- rbind(total_burdenX, total_burdenX_all_urb) %>% distinct()
      }
     
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

      causesX2 <- causesX %>%
        group_by(to) %>%
        summarise(from = list(from))

      total_burdenX_cause <- total_burdenX %>% mutate(label_cause = substring(label_cause, 1, 3))
      total_burdenX_cause <- apply(causesX2, 1, function(cause) {
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

      # For the years 2009:2016 we have population estimates by Education
      if ("Education" %in% colnames(total_burdenX)) {
        total_burdenX_educ <- total_burdenX %>%
          filter(Hispanic.Origin == "All Origins") %>%
          group_by_at(setdiff(colnames(total_burdenX), c("Race", "Deaths"))) %>%
          summarise(Deaths = sum(Deaths)) %>%
          mutate(
            Race = "All",
            Education = Education %>% as.factor()
          )

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
            #tolerance = 0.005, scale = numberDeaths
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
      
      total_burden <- total_burden %>% filter(Gender.Code == "A")
      total_burden <- total_burden %>%
        filter(interested_state == 1) %>%
        mutate(interested_state = NULL)
      
      if("STATEFP" %in% colnames(total_burden)) total_burden <- total_burden %>% filter(STATEFP != "oth") #TODO
      
      
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
      
      # Only considering age above 25
      total_burden <- total_burden %>% filter(min_age >= 25)
      
      #----measure suppression---
      numberDeaths_afterfiltering <- total_burden %>%
        filter(Gender.Code == "A" & Race == "All" & rural_urban_class == 666 & label_cause == "all-cause" & Education == 666) 
      numberDeaths_afterfiltering <- sum(numberDeaths_afterfiltering$Deaths)
      
      suppressed_deaths <- total_burden %>%
        filter(!(Hispanic.Origin != "Unknown" & 
                   min_age != "Unknown" &
                   Education != "Unknown" &
                   rural_urban_class != "Unknown")) %>%
        filter(label_cause == "all-cause") %>%
        group_by_at(setdiff(colnames(total_burden), c("min_age","max_age", "Deaths"))) %>%
        summarize(Deaths = sum(Deaths)) %>% 
        ungroup() %>%
        filter((Race == "All" &Hispanic.Origin == "All Origins" & rural_urban_class == 666 & Education == "Unknown") |
                 (Race == "All" &Hispanic.Origin == "All Origins"& Education == 666 & rural_urban_class == "Unknown")|
                 (rural_urban_class == "All"& Education == 666 & (Race == "Unknown"| Hispanic.Origin == "Unknown") ))
      
      suppressed_deaths <- suppressed_deaths %>% 
        mutate(prop = 100*Deaths/numberDeaths_afterfiltering) %>%
        select(Year, Race, Hispanic.Origin, rural_urban_class, Education, Deaths, prop) %>%
        as.data.frame()
      
      print(paste("In",year,"rows in the mortality counts were excluded due to missing entries:"))
      print(suppressed_deaths)
      print(paste0("(total number of deaths considered: ",numberDeaths_afterfiltering,")"))
      
      total_burden <- total_burden %>%
        filter(Hispanic.Origin != "Unknown" & 
                 # Race != "Guama" &
                 min_age != "Unknown" &
                 Education != "Unknown" &
                 rural_urban_class != "Unknown") %>%
        mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))

      #---write csv---
      test_that("no na end read tot nvss", expect_false(any(is.na(total_burdenX))))

      total_burdenX <- total_burdenX %>% tibble::add_column(source = "nvss")
      #fwrite(total_burdenX, totalBurdenParsedDirX)
      fwrite(suppressed_deaths, totalBurdenParsedDirX)
      toc()
      tic.log()
      ## Close log
      cat("\n\n")
       sink()
    }
  }
}
 doParallel::stopImplicitCluster()
 closeAllConnections()
