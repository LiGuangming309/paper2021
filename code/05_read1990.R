#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 06/14/2021
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign")

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
  year <- 1990
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  
  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
}

if(year == 1990){
  
  ##---- create meta file-----
  metaDir <- file.path(censDir, "meta_down")
  dir.create(metaDir, recursive = T, showWarnings = F)
  filepathCensMeta <- file.path(metaDir, "cens_meta_1990.csv") 
  if(!file.exists(filepathCensMeta)){
    #meta1990 <- data.frame(Race = c("White", "Black or African American", "American Indian or Alaska Native","Asian or Pacific Islander","Other race"))
    meta1990 <- data.frame(Race = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN AND ALASKA NATIVE","Asian or Pacific Islander","Other race"))
    meta1990 <- merge(data.frame(Gender.Code = c("M","F")),
                      meta1990)
    meta1990 <- merge(data.frame(min_age = c(0,1,3,5,6,7,10,12,14,15,16,17,18,19,20,21,22,25,30,35,40,45,50,55,60,62,65,70,75,80,85),
                                 max_age = c(0,2,4,5,6,9,11,13,14,15,16,17,18,19,20,21,24,29,34,39,44,49,54,59,61,64,69,74,79,84,150)),
                      meta1990)
    meta1990$variable <- sprintf("ET4%03d", seq.int(nrow(meta1990)))
    meta1990$Education <- 666
    meta1990$Year <- 1990
    meta1990$group <- "NP12"
    #meta1990$Hispanic.Origin <- "All Origins"
    meta1990$Hispanic.Origin <- "all"
    write.csv(meta1990, filepathCensMeta, row.names = F)
  }
  
  ##---- read census data ----
  
  cross_bridge <- read.csv(file.path(censDir, "cross_bridge", paste0("cross_meta_", year, ".csv")))
  cens1990 <- fread(file.path(dataDir, "nhgis0002_ds120_1990_tract.csv"))
  cols <- colnames(cens1990)[startsWith(colnames(cens1990) , "ET")]
  cols <- c("GEO_ID"="GISJOIN","state"="STATEA","county"="COUNTYA","tract"="TRACTA",cols)
  cens1990 <- cens1990 %>% select(all_of(cols))
  cens1990 <- cens1990 %>% 
    pivot_longer(cols = -c("GEO_ID","state","county","tract"),
                 names_to = "variable",
                 values_to = "pop_size")
  
  cens1990 <- cens1990 %>% right_join(cross_bridge, by =c("variable" ="variable.y"))
  cens1990 <- cens1990 %>% 
    mutate(pop_size = pop_size * coeff) %>%
    group_by(state, county, tract, GEO_ID, variable.x) %>%
    summarize(pop_size = sum(pop_size)) %>%
    rename(variable = variable.x) %>%
    ungroup()
  
  print("test")
  states <- read.csv(file.path(tmpDir, "states.csv"))
  apply(states,1,function(row){
    
  })
}



