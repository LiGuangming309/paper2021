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
  
  #tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  #dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  #censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
}

if (!year %in% c(1990,2000)) {
  print(paste("can not crosswalk census tracts in", year))
  quit()
}

censDirFrom <- file.path(censDir, year)
censDirTo <- file.path(censDir, paste0(year,"_in_2010"))
dir.create(censDirTo, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()
possible_states <- states$STATEFP %>%
  as.character() %>%
  str_pad(., 2, pad = "0")

crosswalk <- read.csv(file.path(dataDir, paste0("crosswalk_", year,"_2010.csv"))) 
if(year == 1990){
  crosswalk <- crosswalk %>% select(trtidFrom = trtid90, trtidTo = trtid10, weight)
}else if(year == 2000){
  crosswalk <- crosswalk %>% select(trtidFrom = trtid00, trtidTo = trtid10, weight)
}

test_that("check that correct trtid10",{
  counties <- tigris::tracts("TX", year = 2010)
})

crosswalk <- crosswalk %>%
  mutate(#trtidFrom = str_pad(trtidFrom, 11, pad = "0"),
         #trtidTo = str_pad(trtidTo, 11, pad = "0"),
        trtidFrom = as.numeric(trtidFrom),
        trtidTo = as.numeric(trtidTo),
        state = str_pad(trtidFrom, 11, pad = "0")%>% str_sub( 1, 2) 
         #state = str_sub(str_pad(trtidFrom, 11, pad = "0"), 1, 2) 
         ) %>%
  filter(state %in% possible_states)

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
    tic(paste("calculated," ,year, "demographic census data in 2010 boundaries in", name))
    # read demographic census data by tract,
    censDataFrom <- file.path(censDirFrom, paste0("census_",year,"_", STUSPS, ".csv")) %>%
      fread(colClasses = c(pop_size = "numeric")) %>%
      select(GEO_ID, variable, pop_size) %>%
      mutate(GEO_ID = as.numeric(GEO_ID))
    
    if(year == 1990){
      censDataFrom$GEO_ID <- substring(censDataFrom$GEO_ID, 1, 11) %>% as.numeric()
    }
    censDataFrom_old <- censDataFrom
    
    #TODO
    test1 <- censDataFrom %>%
      anti_join(crosswalk, by = c("GEO_ID" = "trtidFrom")) %>%
      filter(pop_size > 0)
    
    test2 <- anti_join(crosswalk %>% filter(as.numeric(state) == STATEFP),
                       censDataFrom, 
                       by = c("trtidFrom"="GEO_ID")) 
    
    if (nrow(test1) > 0 &nrow(test2) > 0  ){
      warning("03_interp missing GEO_IDs")
    } 
    

    # translate tracts
    censDataFrom <- censDataFrom %>%
      inner_join(crosswalk, by = c("GEO_ID" = "trtidFrom")) %>%
      mutate(pop_size = pop_size * weight) %>%
      group_by(trtidTo, variable) %>%
      summarise(pop_size = sum(pop_size)) %>%
      rename(GEO_ID = trtidTo) %>%
      as.data.frame()
    
    censDataFrom <- censDataFrom %>% mutate(state = str_pad(GEO_ID, 11, pad = "0") %>% str_sub(1, 2))
    
    test <- lapply(unique(censDataFrom$state), function(statefp) {
      censDataFrom_sub <- censDataFrom %>% 
        filter(state == statefp) %>%
        mutate(state = NULL)
      STUSPS_corresponding <- states[states[, "STATEFP"] == as.numeric(statefp), "STUSPS"]
      if(STUSPS_corresponding == "") browser()
      censDirToX <- file.path(censDirTo, paste0("census_",year,"_", STUSPS_corresponding, ".csv"))
      
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
      expect_false(any(censDataFrom== ""))
      
      test1 <- test %>%
        group_by(variable) %>%
        summarise(pop_size = sum(pop_size))
      
      test2 <- censDataFrom_old %>%
        group_by(variable) %>%
        summarise(pop_size = sum(pop_size))
      
      test3 <- full_join(test1, test2, by = c("variable"))
      expect_equal(test3$pop_size.x, test3$pop_size.y, tolerance = 0.05, scale = test3$pop_size.y)
    })
    
    # delete this state from missing_states
    STUSPS_copy <- STUSPS
    missing_statesDir %>%
      read.csv() %>%
      filter(STUSPS != STUSPS_copy) %>%
      write.csv(missing_statesDir)
  }
})