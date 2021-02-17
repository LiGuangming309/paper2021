#TODO
packages <- c("data.table", "plyr", "magrittr", "testthat", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc", "units")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

if (rlang::is_empty(args)) {
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  expDir <- "/Users/default/Desktop/paper2021/data/01_exposure"
  tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  openaq.script <- "/Users/default/Desktop/paper2021/code/07_openaq.R"
}

tracts_locationsDir <- file.path(exp_tracDir, "openaq_tmp")

years <- 2000:2016

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  filter(STUSPS %in% c("AK", "HI"))

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  tracts_locations<-lapply(years, function(year){
    tracts_locations <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>% read.csv
  }) %>% do.call(rbind,.)
  
  tracts_locations <- tracts_locations %>% filter(distance > 0)
  
  median_distance <-median(tracts_locations$distance) 
  print(paste("median distance in",name,":",median_distance,"km"))
})