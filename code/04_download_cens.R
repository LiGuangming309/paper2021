
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tictoc", "cdcfluview", "testthat", "rlang" #"tigris", 
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

  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
}

# quits, if not downloadable year
if (!year %in% c(2000, 2009:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

#intense computation
if (Sys.info()["sysname"] == "Windows") memory.limit(size=500000)

## ----------read useful data to tmp-------------------------------------------------------------------------------

states <- read.csv(file.path(tmpDir, "states.csv"))

### ------------------------download demographic data-----------------------------------------------------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

census_meta <- read.csv(file.path(censDir, "meta_down", paste0("cens_meta_", toString(year), ".csv")))
census_metan_new <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")))
cross_bridge <- read.csv(file.path(censDir, "cross_bridge", paste0("cross_meta_", year, ".csv")))
# identify relevant variables
relevant_variables <- census_meta$variable %>% unique()
table_groups <- census_meta %>%
  select(group, tablename) %>%
  distinct

if(year %in% 2001:2009) table_groups <- table_groups %>% filter(tablename != "dec/sf1")

## ---------------- download sex by age for each race----------------------
censDir <- file.path(censDir, year)
dir.create(censDir, recursive = T, showWarnings = F)

tic(paste("Downloaded census data in year", toString(year)))
# loop over all states
apply(states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  dem.state.dir <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(censDir, .)

  # download if does not exist yet
  if (!file.exists(dem.state.dir)) { 
    tic(paste("Downloaded census data in year", toString(year), "in", name))
    
    # loop over all groups, download data
    dem.state.data <- apply(table_groups, 1, function(row) {
       group <- row[["group"]]
        tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
        dem.state.data <- getCensus(
          name = row[["tablename"]],
          vintage = year,
          vars = paste0("group(", group, ")"),
          region = "tract:*",
          regionin = sprintf("state:%02d", STATEFP)
        )
        toc()
        return(dem.state.data)
      }) %>%
      rbindlist(fill = TRUE) %>%
        as.data.frame()
    
    # subset relevant part of GEO_ID
    if("GEO_ID" %in% colnames(dem.state.data)){
      dem.state.data$GEO_ID <- dem.state.data$GEO_ID %>%
        str_sub(., -11, -1)
    }else{
      dem.state.data$GEO_ID <- paste0(
        dem.state.data$state , #%>% as.character %>% str_pad(width = 2, pad = "0")
        dem.state.data$county ,
        dem.state.data$tract 
      )
    }
    
    dem.state.data <- dem.state.data %>%
      select(all_of(c(relevant_variables, "state", "county", "tract", "GEO_ID"))) %>%
      pivot_longer(
        cols = !c("state", "county", "tract", "GEO_ID"),
        names_to = "variable",
        values_to = "pop_size"
      )
    dem.state.data$pop_size[is.na(dem.state.data$pop_size)] <- 0
    toc()
    
    dem.state.data.old <- dem.state.data 
    
    ## ---- make additional calculations-----
    tic(paste("Made additional calculations with census data in year", toString(year), "in", name))

    dem.state.data <- dem.state.data %>% right_join(cross_bridge, by =c("variable" ="variable.y"))
    
    dem.state.data <- dem.state.data %>% 
      mutate(pop_size = pop_size * coeff) %>%
      group_by(state, county, tract, GEO_ID, variable.x) %>%
      summarize(pop_size = sum(pop_size)) %>%
      rename(variable = variable.x) %>%
      ungroup()
      
    toc()

    #--- test file for download census------
    test_that("02_download basic checks", {
      expect_false(any(is.na(dem.state.data)))
      expect_true(all(dem.state.data$pop_size >= 0))
      
      #Test, that sums have not changed
      census_meta <- census_meta %>%
        mutate(Education = as.character(Education)) %>%
        rename(Race2 = Race, Hispanic.Origin2 = Hispanic.Origin)

      replaces1 <- data.frame(
        Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Black or African American", "All"),
        Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN", "all")
      )
      replaces2 <- data.frame(
        Hispanic.Origin = c("Not Hispanic or Latino", "All Origins", "Hispanic or Latino"),
        Hispanic.Origin2 = c("NOT HISPANIC OR LATINO", "all", "HISPANIC OR LATINO")
      )
      #TODO age 18-24 in Education is cut off
      
      census_meta <- census_meta %>%
        left_join(replaces1, by = "Race2") %>%
        left_join(replaces2, by = "Hispanic.Origin2") 
      
      join_variables <- c("Race","Hispanic.Origin", "Education", "Gender.Code", "Year") #TODO GEO_ID
      test_dem.state.data.old <- dem.state.data.old %>% 
        left_join(census_meta, by = "variable") %>%
        group_by_at(vars(all_of(join_variables))) %>%
        summarize(pop_size = sum(pop_size)) %>%
        filter(Education == "666") %>%
        mutate(Education = 666)
       
      test_dem.state.data <- dem.state.data %>% 
        left_join(census_metan_new, by = "variable") %>%
        group_by_at(vars(all_of(join_variables))) %>%
        summarize(pop_size = sum(pop_size)) 
      
      test <- inner_join(test_dem.state.data.old, test_dem.state.data, by = join_variables) %>%
        filter(pop_size.x != pop_size.y)
      expect_equal(test$pop_size.x, test$pop_size.y)
      
      #Test, that GEO_ID - variable is a primary key for the data.frame
      test_dem.state.data <- dem.state.data %>% select(GEO_ID,variable)
      test_dem.state.data <- test_dem.state.data[duplicated(test_dem.state.data), ]
      expect_equal(0,nrow(test_dem.state.data))
  
    })

    # save demographic data in seperate file for each state
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    toc()
  }
})
toc()
""
