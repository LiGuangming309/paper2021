#TODO
#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

packages <- c("data.table", "plyr", "magrittr", "testthat", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc", "units", "stats","matrixStats")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

#if (rlang::is_empty(args)) {
  #tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  #expDir <- "/Users/default/Desktop/paper2021/data/01_exposure"
  #tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  #exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  #openaq.script <- "/Users/default/Desktop/paper2021/code/07_openaq.R"
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  expDir <- "C:/Users/Daniel/Desktop/paper2021/data/01_exposure"
  exp_tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/03_exp_tracts"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
#}
  options(dplyr.summarise.inform = FALSE)
  options(dplyr.join.inform = FALSE)
  
tracts_locationsDir <- file.path(exp_tracDir, "epa_tmp")

years <- 1990:2016

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  filter(STUSPS %in% c("AK", "HI"))

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  tracts_locations<-lapply(years, function(year){
    tracts_locationsDir <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv"))
    if(!file.exists(tracts_locationsDir)){
      print(paste("no available in", state, year))
      return(NULL)
    } 
    tracts_locations <- tracts_locationsDir %>% read.csv
    tracts_locations <- tracts_locations %>% filter(distance > 0)
    
    censData <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% read.csv
    
    meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
    censData<- censData %>%
      left_join(meta, by = "variable" )%>%
      filter(Hispanic.Origin == "All Origins" & Race == "All" & Gender.Code == "A" & Education == "666") %>% 
      group_by(Year, GEO_ID) %>%
      summarize(pop_size = sum(pop_size))
    
    tracts_locations <- tracts_locations %>%
      left_join(censData, by = "GEO_ID")
  }) %>% do.call(rbind,.) %>%
    as.data.frame()
  
  any(is.na(tracts_locations))
  tracts_locations <- tracts_locations[rowSums(is.na(tracts_locations)) == 0,]
  
  median_distance <-median(tracts_locations$distance) %>% round(digits = 2)
  mean_distance <-mean(tracts_locations$distance) %>% round(digits = 2)
  weighted_mean_distance <- weighted.mean(tracts_locations$distance, tracts_locations$pop_size, na.rm = TRUE) %>% round(digits = 2)
  weighted_median_distance = matrixStats::weightedMedian(tracts_locations$distance, tracts_locations$pop_size, na.rm = TRUE) %>% round(digits = 2)
  
  print(paste("mean distance in",name,":",mean_distance,"km"))
  print(paste("median distance in",name,":",median_distance,"km"))
  print(paste("weighted mean distance in",name,":",weighted_mean_distance,"km"))
  print(paste("weighted median distance in",name,":",weighted_median_distance,"km"))
})