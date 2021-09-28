#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign", "tigris")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1] %>% as.numeric()
dataDir <- args[2]
tmpDir <- args[3]
censDir <- args[8]

if (rlang::is_empty(args)) {
  year <- 2000
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
}

if (!year %in% c(1990, 2000)) {
  print(paste("can not crosswalk census tracts in", year))
  quit()
}

censDirFrom <- file.path(censDir, year)
censDirTo <- file.path(censDir, paste0(year, "_in_2010"))
dir.create(censDirTo, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()
possible_states <- states$STATEFP

crosswalk <- read.csv(file.path(dataDir, paste0("crosswalk_", year, "_2010.csv")))
if (year == 1990) {
  crosswalk <- crosswalk %>% select(trtidFrom = trtid90, trtidTo = trtid10, weight)
} else if (year == 2000) {
  crosswalk <- crosswalk %>% select(trtidFrom = trtid00, trtidTo = trtid10, weight)
}

crosswalk <- crosswalk %>%
  mutate(
    trtidFrom = as.character(trtidFrom),
    stateFrom = str_sub(trtidFrom, 1, -10),
    trtidFrom = case_when(
      str_sub(trtidFrom, -2, -1) == "00" ~ str_sub(trtidFrom, 1, -3),
      TRUE ~ trtidFrom
    ),
    trtidFrom = as.numeric(trtidFrom),

    trtidTo = as.character(trtidTo),
    stateTo = str_sub(trtidTo, 1, -10),
    countyTo = str_sub(trtidTo, -9, -6),
    tractTo = str_sub(trtidTo, -5, -1),
    tractTo = case_when(str_sub(tractTo,-2,-1) == "00"~ str_sub(tractTo,1,-3),
                        TRUE ~ tractTo),
    trtidTo = case_when(
      str_sub(trtidTo, -2, -1) == "00" ~ str_sub(trtidTo, 1, -3),
      TRUE ~ trtidTo
    ),
    trtidTo = as.numeric(trtidTo)
  )

crosswalk <- crosswalk %>% filter(stateFrom %in% possible_states)

# states, for which 2000 and 2010 still needs to be calculated
missing_statesDir <- file.path(censDirTo, "missing_states.csv")
if (!file.exists(missing_statesDir)) write.csv(states, missing_statesDir)
missing_states <- read.csv(missing_statesDir)

## -----calculate 2010 data in 2000 boundaries and meta data -----
apply(missing_states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  if (!is.na(STUSPS)) {
    tic(paste("calculated,", year, "demographic census data in 2010 boundaries in", name))
    # read demographic census data by tract,
    censDataFrom <- file.path(censDirFrom, paste0("census_", year, "_", STUSPS, ".csv")) %>%
      fread(colClasses = c(pop_size = "numeric")) %>%
      select(GEO_ID,state,county, tract, variable, pop_size) %>%
      
    mutate_at(c("state","county","tract"), as.character)

    if (year == 1990) {
      censDataFrom <- censDataFrom %>% 
        mutate(GEO_ID = as.character(GEO_ID),
               GEO_ID = case_when(str_sub(GEO_ID,-2,-1) == "00"~ str_sub(GEO_ID,1,-3), #,"99"
                                  str_sub(GEO_ID,-2,-1) == "99"~ paste0(str_sub(GEO_ID,1,-3)), #,"01"
                                   TRUE ~ GEO_ID)
               )
      
      censDataFrom <- censDataFrom %>% 
        group_by(GEO_ID, state, county, tract, variable) %>%
        summarise(pop_size = sum(pop_size))
    }
    censDataFrom <- censDataFrom %>% mutate(GEO_ID = as.numeric(GEO_ID)) 
    
    censDataFrom_old <- censDataFrom

    test_that("no geo_id missing", {
      expect_false(any(is.na(crosswalk)))
      expect_false(any(is.na(censDataFrom)))
      
      test1 <- censDataFrom %>%
        anti_join(crosswalk, by = c("GEO_ID" = "trtidFrom", "state"="stateFrom")) %>%
        filter(pop_size > 0)%>%
        group_by(GEO_ID, state, county, tract) %>%
        summarize(n = n(),
                  nchar = nchar(GEO_ID))
      
      test2 <- crosswalk %>%
        filter(stateFrom == STATEFP) %>%
        anti_join(censDataFrom, by = c("trtidFrom" = "GEO_ID", "stateFrom" = "state"))
      
      #test3 <- inner_join(
      #  crosswalk %>% mutate(str_sub=str_sub(trtidFrom,1,-3)),
      #  test1 %>% mutate(str_sub=str_sub(GEO_ID,1,-3)),
      #  by = "str_sub"
      #)

      expect_equal(nrow(test1), 0)
    })

    # translate tracts
    censDataFrom <- censDataFrom %>%
      #left_join(crosswalk, by = c("GEO_ID" = "trtidFrom", "state"="stateFrom")) %>%
      inner_join(crosswalk, by = c("GEO_ID" = "trtidFrom", "state"="stateFrom")) %>%
      mutate(
        #trtidTo = coalesce(trtidTo, GEO_ID), #TODO
        #weight = coalesce(weight, 1), #TODO
        #stateTo = coalesce(stateTo, state),
        #countyTo = coalesce(countyTo, county),
        #tractTo = coalesce(tractTo, county),
        pop_size = pop_size * weight
      ) %>%
      group_by(stateTo,countyTo,tractTo,trtidTo, variable) %>%
      summarise(pop_size = sum(pop_size)) %>%
      rename(GEO_ID = trtidTo, state = stateTo, county = countyTo, tract = tractTo) %>%
      as.data.frame()

    
    test <- lapply(unique(censDataFrom$state), function(statefp) {
      censDataFrom_sub <- censDataFrom %>%
        filter(state == statefp) 
      
      STUSPS_corresponding <- states[states[, "STATEFP"] == as.numeric(statefp), "STUSPS"]
      if (STUSPS_corresponding == "") browser()
      censDirToX <- file.path(censDirTo, paste0("census_", year, "_", STUSPS_corresponding, ".csv"))

      suppressWarnings(
        write.table(censDataFrom_sub,
          censDirToX,
          sep = ",",
          col.names = !file.exists(censDirToX),
          append = T,
          row.names = F
        )
      )

      return(censDataFrom_sub)
    }) %>% rbindlist()

    test_that("03_interp old censData10", {
      expect_false(any(is.na(censDataFrom)))
      expect_false(any(censDataFrom == ""))

      test1 <- test %>%
        group_by(variable) %>%
        summarise(pop_size = sum(pop_size))

      test2 <- censDataFrom_old %>%
        group_by(variable) %>%
        summarise(pop_size = sum(pop_size))

      test3 <- full_join(test1, test2, by = c("variable"))
      expect_equal(test3$pop_size.x, test3$pop_size.y,tolerance = 0.01,  scale = test3$pop_size.y)
    })

    # delete this state from missing_states
    STUSPS_copy <- STUSPS
    missing_statesDir %>%
      read.csv() %>%
      filter(STUSPS != STUSPS_copy) %>%
      write.csv(missing_statesDir)
  }
})
