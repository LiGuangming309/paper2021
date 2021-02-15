
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
  "tigris", "tictoc", "cdcfluview", "testthat", "rlang"
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
  year <- 2016

   censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
   tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"

  #tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  #censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
}

# quits, if not downloadable year
if (!year %in% c(2000, 2010:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

## ----------download useful data to tmp-------------------------------------------------------------------------------

states <- read.csv(file.path(tmpDir, "states.csv"))


### ------------------------download demographic data-----------------------------------------------------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

census_meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")))
# identify relevant variables
relevant_variables <- census_meta$variable %>% unique()

## ---------------- download sex by age for each race----------------------
censDir <- file.path(censDir, year)
dir.create(censDir, recursive = T, showWarnings = F)

# relevant groups for each year and table names
if (year == 2000) {
  # decennical census, sex by age for races
  groups <- c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I", "PCT012J", "PCT012K", "PCT012L", "PCT012M")
  tablename <- "dec/sf1"
} else if (year == 2010) {
  # decennical census, sex by age for races
  groups <- c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M")
  tablename <- "dec/sf1"
} else if (year %in% 2011:2016) {
  # american community survey, sex by age for races
  groups <- c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H")
  # "not hispanic or latino" only available for white
  tablename <- "acs/acs5"
}

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
     dem.state.data <- lapply(groups, function(group) {
        tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
        data <- getCensus(
          name = tablename,
          vintage = year,
          vars = paste0("group(", group, ")"),
          region = "tract:*",
          regionin = sprintf("state:%02d", STATEFP)
        )

    # subset relevant part of GEO_ID
        if("GEO_ID" %in% colnames(data)){
          data$GEO_ID <- data$GEO_ID %>%
            str_sub(., -11, -1)
        }else{
          data$GEO_ID <- paste0(
            data$state , #%>% as.character %>% str_pad(width = 2, pad = "0")
            data$county ,
            data$tract 
          )
        }

        data <- data %>%
          select(any_of(c(relevant_variables, "state", "county", "tract", "GEO_ID"))) %>%
          pivot_longer(
            cols = !c("state", "county", "tract", "GEO_ID"),
            names_to = "variable",
            values_to = "pop_size"
          )
        toc()
        return(data)
      }) %>%
        do.call(rbind, .) %>%
        as.data.frame()
    
    dem.state.data.old <- dem.state.data 
    
    ## ---- make additional calculations-----
    tic(paste("Made additional calculations with census data in year", toString(year), "in", name))

    # make wider
    dem.state.data <- dem.state.data %>% #spread(key = variable, value = pop_size, fill = 0)
      pivot_wider(
        names_from = variable,
        values_from = pop_size,
        values_fill = 0
      ) 

    # filter out census tracts, where no one is living
    totalPopulationTract <- dem.state.data %>%
      select(-c(state, county, tract, GEO_ID)) %>%
      apply(., 2, as.numeric) %>%
      rowSums()

    dem.state.data <- dem.state.data[totalPopulationTract > 0, ] #TODO

    # calculate new variables from old ones
    census_meta_sub <- census_meta %>% filter(downloaded == FALSE)

    for (i in 1:nrow(census_meta_sub)) {
      var <- census_meta_sub[i, "variable"]

      # parse String "A|B|C..." to vector c(A,B,C,...)
      tot_var <- census_meta_sub[i, "tot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()

      ntot_var <- census_meta_sub[i, "ntot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()

      # row sum all columnds
      if (rlang::is_empty(tot_var)) {
        data_tot <- 0
      } else {
        data_tot <- dem.state.data[, tot_var, drop = FALSE] %>%
          apply(., 2, as.numeric) %>%
          rowSums() %>%
          unlist
      }

      if (rlang::is_empty(ntot_var)) {
        data_ntot <- 0
      } else {
        data_ntot <- dem.state.data[, ntot_var, drop = FALSE] %>%
          apply(., 2, as.numeric) %>%
          rowSums() %>%
          unlist()
      }

      # calculate "Hispanic or latino" = "all" - "not hispanic or latino"
      dem.state.data[, var] <- data_tot - data_ntot

      # some estimates lead to negative results => max(x,0)
      dem.state.data[, var] <- (dem.state.data[, var] + abs(dem.state.data[, var])) / 2
    }

    # longer again
    dem.state.data <- dem.state.data %>%
      pivot_longer(
        cols = !c("state", "county", "tract", "GEO_ID"),
        names_to = "variable",
        values_to = "pop_size"
      )

    # filter relevant variables
    relevant_variables <- census_meta %>%
      filter(relevant == TRUE) %>%
      select(variable) %>%
      unlist()

    dem.state.data <- dem.state.data %>%
      filter(variable %in% relevant_variables)
    toc()

    #--- test file for download census------
    test_that("02_download basic checks", {
      expect_false(any(is.na(dem.state.data)))
      expect_true(all(dem.state.data$pop_size >= 0))
      
      #Test, that has not changed
      test_dem.state.data <- dem.state.data.old %>%
        inner_join(dem.state.data, by = c("state", "county", "tract", "GEO_ID", "variable"))
      expect_equal(test_dem.state.data$pop_size.x, test_dem.state.data$pop_size.y)
      
      #Test, that GEO_ID - variable is a primary key for the data.frame
      test_dem.state.data <- dem.state.data %>%
        select(GEO_ID,variable)%>% 
        distinct

      is_equal <-nrow(test_dem.state.data)== nrow(dem.state.data)
      expect_true(is_equal)
      if(!is_equal){
        test_dem.state.data <- dem.state.data %>%
          group_by(GEO_ID,variable) %>%
          summarise(appearances = n())
        browser()
      }
    })

    test_that("02_download sanity check total population", {
      test_dem.state.data <- dem.state.data %>%
        inner_join(census_meta, by = "variable") %>%
        group_by(hispanic_origin) %>%
        summarise(pop_size = sum(pop_size)) %>%
        as.data.frame()

      test_dem.state.data.old <- dem.state.data.old %>%
        inner_join(census_meta, by = "variable") %>%
        group_by(hispanic_origin) %>%
        summarise(pop_size = sum(pop_size)) %>%
        as.data.frame()

      print(as.data.frame(test_dem.state.data))

      pop_all <- test_dem.state.data[test_dem.state.data[, "hispanic_origin"] == "All Origins", "pop_size"] %>% unlist()
      pop_his <- test_dem.state.data[test_dem.state.data[, "hispanic_origin"] == "Hispanic or Latino", "pop_size"] %>% unlist()
      pop_nhis <- test_dem.state.data[test_dem.state.data[, "hispanic_origin"] == "Not Hispanic or Latino", "pop_size"] %>% unlist()
      pop_all_old <- test_dem.state.data.old[test_dem.state.data.old[, "hispanic_origin"] == "All Origins", "pop_size"] %>% unlist()
      pop_nhis_old <- test_dem.state.data.old[test_dem.state.data.old[, "hispanic_origin"] == "Not Hispanic or Latino", "pop_size"] %>% unlist()
      
      if(year %in% c(2000,2010)){
        expect_equal(pop_all, pop_his + pop_nhis)
      }else{
        pop_all_white <- dem.state.data %>%
          inner_join(census_meta, by = "variable")%>%
          filter(hispanic_origin == "All Origins"&
                 race == "White") %>%
          select(pop_size) %>%
          sum 
        expect_equal(pop_all_white, pop_his + pop_nhis)
      }
      
      expect_equal(pop_all, pop_all_old)
      expect_equal(pop_nhis, pop_nhis_old)
    })

    # save demographic data in seperate file for each state
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    toc()
  }
})
toc()
""
