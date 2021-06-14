
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census meta data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "DataCombine", "data.table", "tidyverse",
  "tictoc", "testthat", "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

# TODO l?schen
if (rlang::is_empty(args)) {
  year <- 2000

  # censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
  # tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
}
metaDir <- file.path(censDir, "meta")
dir.create(metaDir, recursive = T, showWarnings = F)
cross_bridgeDir <- file.path(censDir, "cross_bridge")
dir.create(cross_bridgeDir, recursive = T, showWarnings = F)

## ----- determine which variables we want to have ----
aim_metaDir <- file.path(metaDir, paste0("cens_meta_", year, ".csv"))
if (!file.exists(aim_metaDir)) {
  aim_meta1 <- data.frame(
    Race = c("White", "White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American", "All"),
    Hispanic.Origin = c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins", "All Origins", "All Origins", "All Origins"),
    Education = 666
  )
  aim_meta1 <- merge(data.frame(Gender.Code = c("A")), aim_meta1) #, "M", "F"

  aim_meta1 <- rbind(
    merge(
      aim_meta1,
      merge(
        data.frame(Year = c(2000:2008, 2010)),
        data.frame(
          min_age = c(0, seq(25, 85, 5)),
          max_age = c(24, seq(29, 84, 5), 150)
        )
      )
    ),
    merge(
      aim_meta1,
      merge(
        data.frame(Year = c(2009, 2011:2016)),
        data.frame(
          min_age = c(0, 25, 30, 35, 45, 55, 65, 75, 85),
          max_age = c(24, 29, 34, 44, 54, 64, 74, 84, 150)
        )
      )
    )
  )

  aim_meta2 <- data.frame(Race = "All", Hispanic.Origin = "All Origins", Education = 1:7)
  aim_meta2 <- merge(data.frame(Year = 2009:2016), aim_meta2)
  aim_meta2 <- merge(data.frame(Gender.Code = c("A")), aim_meta2) #, "M", "F"
  # Ignoring 18-25 
  aim_meta2 <- merge(aim_meta2, data.frame(
    min_age = c(25,35,45, 65),
    max_age = c(34,44,64, 150)
  ))

  aim_meta <- rbind(aim_meta1, aim_meta2)
  rm(aim_meta1, aim_meta2)

  aim_meta$variable <- apply(aim_meta, 1, function(row) {
    Race <- switch(row[["Race"]],
      "White" = "W",
      "American Indian or Alaska Native" = "I",
      "Asian or Pacific Islander" = "A",
      "Black or African American" = "B",
      "All" = "U",
    )
    Hispanic.Origin <- substring(row[["Hispanic.Origin"]], 1, 1)
    Education <- min(row[["Education"]] %>% as.numeric(), 66)
    Gender.Code <- row[["Gender.Code"]]
    min_age <- sprintf("%02d", as.numeric(row[["min_age"]]))
    max_age <- sprintf("%03d", as.numeric(row[["max_age"]]))
    paste0(Race, Hispanic.Origin, Education, Gender.Code, min_age, max_age)
  })

  aim_meta <- aim_meta %>% filter(Year == year)
  test_that("dublicate variable names", {
    aim_meta_test <- aim_meta %>% select(Year, Race, Hispanic.Origin, Education, Gender.Code, min_age)
    aim_meta_test <- aim_meta_test[duplicated(aim_meta_test), ]
    expect_equal(nrow(aim_meta_test), 0)

    variable_names <- aim_meta$variable
    variable_names <- variable_names[duplicated(variable_names)]
    expect_true(rlang::is_empty(variable_names))
  })

  ## add corresponding age_group_id from causes ages
  aim_meta <- aim_meta %>%
    mutate(
      age_group_id = c(0,seq(25, 95, 5))[
        findInterval(
          max_age,
          c(0,seq(25, 95, 5)),
          left.open =  F
        ) 
      ]
    )
  write.csv(aim_meta, aim_metaDir, row.names = FALSE)
}
## ----- cross-bridge-----
cross_bridgeDir <- file.path(cross_bridgeDir, paste0("cross_meta_", year, ".csv"))
if (!file.exists(cross_bridgeDir)) {
  tic(paste("crossed census meta data for", year))
  aim_meta <- read.csv(aim_metaDir)
  downloaded_meta <- file.path(censDir, "meta_down", paste0("cens_meta_", toString(year), ".csv")) %>% read.csv()
  downloaded_meta <- downloaded_meta %>%
    mutate(Education = as.character(Education)) %>%
    rename(Race2 = Race, Hispanic.Origin2 = Hispanic.Origin, Education2 = Education, Gender.Code2 = Gender.Code)
  downloaded_meta[downloaded_meta == "High school graduate (includes equivalency)"] <- "High school graduate, GED, or alternative"

  replaces1 <- data.frame(
    Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Black or African American", "All"),
    Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN", "all")
  )
  replaces2 <- data.frame(
    Hispanic.Origin = c("Not Hispanic or Latino", "All Origins", "Hispanic or Latino", "Hispanic or Latino"),
    Hispanic.Origin2 = c("NOT HISPANIC OR LATINO", "all", "all", "NOT HISPANIC OR LATINO"),
    coeff = c(1, 1, 1, -1)
  )
  replaces3 <- data.frame(
    Education = c(1:7, 666),
    Education2 = c(
      "Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative",
      "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"
    )
  )

  replaces4 <- data.frame(
    Gender.Code = c("M", "F", "A", "A"),
    Gender.Code2 = c("M", "F", "M", "F")
  )

  replaces5<- data.frame(
    Race = c("All", "All", "All", "All", "All"),
    Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN")
  )
  
  cross_bridge <- aim_meta %>%
    left_join(replaces2, by = "Hispanic.Origin") %>%
    left_join(replaces3, by = "Education") %>%
    left_join(replaces4, by = "Gender.Code") 
  
  cross_bridge1 <- cross_bridge %>%
    filter(!(Race == "All" & Hispanic.Origin == "All Origins" & Education == "666")) %>%
    left_join(replaces1, by = "Race") 
  
  cross_bridge2 <- cross_bridge %>%
    filter((Race == "All" & Hispanic.Origin == "All Origins" & Education == "666")) %>%
    left_join(replaces5, by = "Race") 

  cross_bridge <- rbind(cross_bridge1, cross_bridge2) %>%
    left_join(downloaded_meta, by = c("Year", "Gender.Code2", "Race2", "Hispanic.Origin2", "Education2"))  
  
  rm(cross_bridge1, cross_bridge2)  
  test_that("basic checks", {
    new_DF <- cross_bridge[rowSums(is.na(cross_bridge)) > 0, c("Year", "Gender.Code2", "Race2", "Hispanic.Origin2", "Education2")] # TODO
    expect_equal(0, nrow(new_DF))

    cross_bridge_test <- cross_bridge %>% filter(min_age.y < min_age.x & max_age.x < max_age.y)
    expect_equal(0, nrow(cross_bridge_test))
  })

  cross_bridge <- cross_bridge %>% filter(min_age.x <= min_age.y & max_age.y <= max_age.x)
  rm(replaces1, replaces2, replaces3, replaces4)

  test_that("check ages", {
    missing <- downloaded_meta %>% 
      filter(!variable %in% cross_bridge$variable.y) %>%
      filter(!(min_age == 18 & Education2 != 666))
    expect_equal(0, nrow(missing))

    cross_bridge_test <- cross_bridge %>%
      group_by(variable.x) %>%
      summarize(
        min_age.x = min(min_age.x),
        min_age.y = min(min_age.y),
        max_age.x = max(max_age.x),
        max_age.y = max(max_age.y)
      ) %>%
      ungroup() %>%
      filter(min_age.x != min_age.y | max_age.x != max_age.y)

    expect_equal(0, nrow(cross_bridge_test))
  })

  cross_bridge <- cross_bridge %>% select(variable.x, coeff, variable.y)
  fwrite(cross_bridge, cross_bridgeDir)
  toc()
}
