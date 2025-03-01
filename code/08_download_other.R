
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download required data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "RCurl", "magrittr","tidycensus", "stringr", "data.table", "tidyverse", "tictoc","rhdf5") # "tigris", 

options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2000
  
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  expDir <- "/Users/default/Desktop/paper2021/data/01_exposure"
  tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  
  ##tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  #expDir <- "C:/Users/Daniel/Desktop/paper2021/data/01_exposure"
  #tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/02_tracts"
}
#-- load data---
states <- file.path(tmpDir,"states.csv") %>% read.csv

#----------------------download exposure data-----------------
filenameExp <- paste0(toString(year), ".h5")
filepathExp <- file.path(expDir, filenameExp)

if (!file.exists(filepathExp)) {
  url <- "ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  tic(paste("Successfully downloaded PM exposure data for", year))
  #https://community.rstudio.com/t/download-file-issue-corrupted-file/60844/4
  download.file(paste0(url, filenameExp), filepathExp, quiet = TRUE, mode = "wb")
  toc()
}

# save useful variable for estimations later on
filepathM <- paste0("m_exp_", toString(year), ".RData") %>%
  file.path(tmpDir, .)

# calculate useful estimates to work efficiently with exposure data
if (!file.exists(filepathM)) {
  exp_data <- H5Fopen(filepathExp)

  long_vec <- c(as.matrix(exp_data$longitude)) 
  lat_vec <- c(as.matrix(exp_data$latitude))
  
  #y = mx +c
  #slope of longtitude, max in
  n_long <- length(long_vec) - 1
  long_dif <- long_vec[2:(1 + n_long)] - long_vec[1:n_long]
  m_min_long <- min(long_dif)
  m_max_long <- max(long_dif)
  
  #slope of latitude 
  n_lat <- length(lat_vec) - 1
  lat_dif <- lat_vec[2:(1 + n_lat)] - lat_vec[1:n_lat]
  m_min_lat <- min(lat_dif)
  m_max_lat <- max(lat_dif)
  
  #save estimates
  save(m_min_long, m_max_long, m_min_lat, m_max_lat, file = filepathM)
}

rm(filenameExp, filepathExp, filepathM)


### ------------------download tract shape files--------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

filepathTr <- file.path(tracDir, toString(year))
dir.create(filepathTr, recursive = T, showWarnings = F)

    apply(states, 1, function(state) {
      STUSPS <- state["STUSPS"]
      name <- state["NAME"]
    
      filepathTrX <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
        file.path(filepathTr, .)
    
      if (!file.exists(filepathTrX)) {
        tic(paste("Downloaded census tracts shape files for", year, name))
        
        #harmonize data
        #if (year == 1990){
        #  tracts <- tracts(state = STUSPS, cb = TRUE, year = year)
        #  tracts <- tracts %>% mutate(
        #         #GEO_ID = paste0(sprintf("%02d", STATEFP), sprintf("%03d", COUNTYFP), sprintf("%04d", TRACTBASE), sprintf("%02d", TRACTSUF))
        #         GEO_ID = paste0(STATEFP, COUNTYFP, TRACTBASE, TRACTSUF)
        #  )
        #}else if(year %in% c(2000)){
        #  tracts <- tracts(state = STUSPS, cb = TRUE, year = 2000) 
        #  tracts$GEO_ID <-paste0(tracts$STATE,tracts$COUNTY,tracts$TRACT)
        #}else if(year %in% c(1991:1999,2001:2008,2009, 2010)){
        #  tracts <- tracts(state = STUSPS, cb = TRUE, year = 2010)
        #  tracts$GEO_ID<- tracts$GEO_ID %>% str_sub(.,-11,-1)
        #}else if(year %in% c(2011:2012)){
        #    tracts <- tracts(state = STUSPS, cb = FALSE, year = year) %>%
        #      rename(GEO_ID = GEOID)
        #    #TODO Geometry corrupted?
        #  
        #}else if(year %in% 2013:2016){
        #  tracts <- tracts(state = STUSPS, cb = TRUE, year = year)
        #  tracts$GEO_ID<- tracts$AFFGEOID %>% str_sub(.,-11,-1)
        #}
        if(year == 1990){
          #tracts<-get_decennial(geography = "tract", variables = "TODO", year = 1990, state = STUSPS, geometry = TRUE)
          tracts <- tigris::tracts(state = STUSPS, cb = TRUE, year = year)
          tracts <- tracts %>% mutate(GEO_ID = paste0(STATEFP, COUNTYFP, TRACTBASE, TRACTSUF))
        }else if(year %in% c(1991:1999,2001:2008, 2010)){
          tracts<-get_decennial(geography = "tract", variables = "PCT012A009", year = 2010, state = STUSPS, geometry = TRUE, key = key)%>% 
            rename(GEO_ID = GEOID)
        }else if(year == 2000){
          tracts<-get_decennial(geography = "tract", variables = "P012A005", year = 2000, state = STUSPS, geometry = TRUE, key = key)%>% 
            rename(GEO_ID = GEOID) #%>%
            #mutate(
            #  GEO_ID = case_when(str_sub(GEO_ID,-2,-1) == "00"~ str_sub(GEO_ID,1,-3),
            #                      TRUE ~ GEO_ID)
            #)

        }else if(year %in% c(2009,2011:2016)){
          tracts<-get_acs(geography = "tract", variables = "B01001A_003E", state = STUSPS, geometry = TRUE, year = year, key = key)%>% 
            rename(GEO_ID = GEOID)
        }
        #save only relevant data
        tracts <- tracts  %>%
                  select("GEO_ID","geometry") %>%
                  distinct
                  #filter(!is.na(AFFGEOID)) #some are entirely in water, e.g. tract 01003990000 => ignore those
        #12087980100
        #TODO
        
        saveRDS(tracts, filepathTrX)
        toc()
      }
    })


rm(filepathTr)
""