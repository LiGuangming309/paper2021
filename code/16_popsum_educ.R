#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "readxl")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
dataDir <- args[2]
agr_by <- args[10]
tmpDir <- args[3]
censDir <- args[8]
pop.summary.dir <- args[16]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 1991
  agr_by <- "county"

  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
  
  #dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  #tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  #censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  #pop.summary.dir <- "C:/Users/Daniel/Desktop/paper2021/data/11_population_summary"
}

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv()

corresponding_year <- setNames(c(1990, rep(2010,9),2000,rep(2010,8),2000,rep(2010,7)), 1990:2016)
rural_urban_class <- read.csv(file.path(dataDir, "rural_urban_class.csv")) %>%
  filter(fromYear == corresponding_year[[as.character(year)]])
rm(corresponding_year)

plotDir <- file.path(pop.summary.dir, "plot", agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)
# load meta data

pop.summary.dir <- file.path(pop.summary.dir, agr_by)
dir.create(pop.summary.dir, recursive = T, showWarnings = F)
pop.summary.dirX <- file.path(pop.summary.dir, paste0("pop_sum_", year, ".csv"))

if (!file.exists(pop.summary.dirX)) {
  census_meta <- file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")) %>% fread()
  
  # loop over all states
  tic(paste("summarized population data in", year, "by", agr_by))
  pop.summary <- apply(states, 1, function(state) {
    STUSPS <- state[["STUSPS"]]
    name <- state[["NAME"]]

    # read demographic census data by tract
    pop.summary <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% fread()
    
    pop.summary <- pop.summary %>% 
      mutate(FIPS.code = paste0(state, str_pad(county, 3, pad = "0")) %>% as.double) %>%
      mutate(county = NULL)
      
    if(agr_by == "county"){
      pop.summary <- pop.summary %>%
        rename(county = FIPS.code) %>%
        group_by(county, variable) %>%
        summarize(Population = sum(pop_size)) %>%
        mutate(rural_urban_class = as.factor(666))
    }else{
      #TODO delete
      anti_joined <- anti_join(pop.summary, rural_urban_class, by = "FIPS.code")
      missing_FIPS <- anti_joined$FIPS.code %>% unique()
      if(length(missing_FIPS) > 0){
        print(paste("16_popsum_educ;",length(missing_FIPS) ,"FIPS missing in",year,"in",name,":"))
        print(missing_FIPS)
      }
      
      pop.summary <- pop.summary%>%
        left_join(rural_urban_class, by = "FIPS.code") %>%
        mutate(FIPS.code = NULL)
      
      pop.summary1 <- pop.summary %>%
        group_by(state, variable) %>%
        summarize(Population = sum(pop_size)) %>%
        mutate(rural_urban_class = as.factor(666))
      
      pop.summary2 <- pop.summary %>%
        group_by(state, variable, rural_urban_class) %>%
        summarize(Population = sum(pop_size)) %>%
        filter(!is.na(rural_urban_class)) %>% #TODO
        mutate(rural_urban_class = as.factor(rural_urban_class))
      rm(pop.summary1, pop.summary2)
      
    }
    
    return(pop.summary)
  }) %>% rbindlist()

  if(agr_by != "county"){
    pop.summary <- states %>%
      right_join(pop.summary, by = c("STATEFP" = "state")) %>%
      dplyr::group_by_at(vars(all_of(c(agr_by, "variable", "rural_urban_class")))) %>%
      summarize(Population = sum(Population)) %>%
      as.data.frame()
  }
  
  pop.summary <- pop.summary %>%
    left_join(census_meta, by = "variable") %>%
    select(-c(variable))
  
  pop.summary <- pop.summary %>% tibble::add_column(source2 = "Census")
  pop.summary <- pop.summary %>% filter(min_age >= 25)
  write.csv(pop.summary, pop.summary.dirX, row.names = FALSE)
  toc()
}
