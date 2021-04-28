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
packages <- c("dplyr","DataCombine" ,"magrittr", "data.table", "testthat", "tidyverse", "tictoc", "stats","matrixStats")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
censDir <- args[2]
dem_agrDir <- args[3]
pop.summary.dir <- args[4]
summaryDir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  #tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  #censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog/"
  #pm_summDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr/"
  #summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
}

states <- file.path(tmpDir, "states.csv") %>% read.csv() %>% select(NAME, STATEFP)

##--- read pm data ----
pm_summDir <- file.path(summaryDir, "pm_summary.csv")
if(!file.exists(pm_summDir)){
  
  tic(paste("summarized pm data"))
  agr_bys <- setdiff(list.files(dem_agrDir),"county")
  pm_summ <- lapply(agr_bys, function(agr_by){
    tic(paste("summarized pm data by", agr_by))
    years <- list.files(file.path(dem_agrDir, agr_by))
    pm_summ <- lapply(years, function(year){
      meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
      files <- list.files(file.path(dem_agrDir, agr_by, year))
      pm_summ<-lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist
      pm_summ<- pm_summ %>% left_join(meta, by = "variable")
      return(pm_summ)
    }) %>% rbindlist
    #make compatible
    pm_summ <- pm_summ %>% rename("Region":=!!agr_by)
    pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)
    
    toc()
    return(pm_summ)
  }) %>% rbindlist
  
  pm_summ <- pm_summ %>%
    group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, Education, pm) %>%
    summarise(pop_size = sum(pop_size))
  
  pm_summ <- pm_summ %>%
    group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, Education) %>%
    summarise(mean = weighted.mean(pm, pop_size),
              median = matrixStats::weightedMedian(pm, pop_size))
  
  pm_summ <- pm_summ %>%
    pivot_longer(c(mean,median),
                 names_to = "pm_metric")
  
  ##--- find and replcase---
  rindreplace1 <- setNames(c(states$NAME, "United States"), c(states$STATEFP,"us"))
  pm_summ$Region <- sapply(pm_summ$Region , function(x) rindreplace1[[x]])
  
  rindreplace2 <- setNames(c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"), 
                           c(1:7, 666))
  pm_summ$Education <- sapply(pm_summ$Education %>% as.character, function(x) rindreplace2[[x]])
  
  rindreplace3 <- setNames(c("All genders", "Male","Female"), c("A","M","F"))
  pm_summ$Gender.Code <- sapply(pm_summ$Gender.Code , function(x) rindreplace3[[x]])
  
  pm_summ <- pm_summ%>% mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) 
  pm_summ$Hispanic.Origin <- NULL 
  pm_summ$Race <- NULL
  
  fwrite(pm_summ, pm_summDir)
  rm(rindreplace1, rindreplace2, rindreplace3)
  toc()
}

### --- read population data-----
pop_summaryDir <- file.path(summaryDir, "pop_summary.csv")
if(!file.exists(pop_summaryDir)){
  tic("summarized population data")
  files <- setdiff(list.files(pop.summary.dir),"plot")
  files1 <- files[endsWith(files,".csv")]
  pop_summary1 <- lapply(files1, function(file){
    pop_summary1 <- fread(file.path(pop.summary.dir, file))
    #make compatible
    agr_by <- str_sub(file,5,-5)
    pop_summary1 <- pop_summary1 %>% rename("Region":=!!agr_by)
    pop_summary1 <- pop_summary1 %>% tibble::add_column(agr_by = agr_by)
  })%>% rbindlist()
  
  agr_bys <- files[!endsWith(files,".csv")]
  pop_summary2 <- lapply(agr_bys, function(agr_by){
    files2 <- list.files(file.path(pop.summary.dir, agr_by))
    pop_summary2<-lapply(files2, function(file) fread(file.path(pop.summary.dir, agr_by, file))) %>% rbindlist()
    #make compatible
    pop_summary2 <- pop_summary2 %>% rename("Region":=!!agr_by)
    pop_summary2 <- pop_summary2 %>% tibble::add_column(agr_by = agr_by)
  }) %>% rbindlist()
  #pop_summary2 <- pop_summary2 %>% filter(Education != 666) #TODO
  
  pop_summary <- rbind(pop_summary1, pop_summary2)
  rm(pop_summary1, pop_summary2)
  
  pop_summary <- pop_summary %>%
    group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, Education, source2) %>%
    summarise(Population = sum(Population))
  ###---find and replace----
  rindreplace1 <- setNames(c(states$NAME, "United States"), c(states$STATEFP,"us"))
  pop_summary$Region <- sapply(pop_summary$Region , function(x) rindreplace1[[x]])
  
  rindreplace2 <- setNames(c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"), 
                           c(1:7, 666))
  pop_summary$Education <- sapply(pop_summary$Education %>% as.character, function(x) rindreplace2[[x]])
  
  rindreplace3 <- setNames(c("All genders", "Male","Female"), c("A","M","F"))
  pop_summary$Gender.Code <- sapply(pop_summary$Gender.Code , function(x) rindreplace3[[x]])
  
  rindreplace4 <- setNames(c("Official Bridged-Race Population Estimates", "Own Interpolation"), c("CDC","Census"))
  pop_summary$source2 <- sapply(pop_summary$source2 , function(x) rindreplace4[[x]])
  
  pop_summary <- pop_summary%>% mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) 
  pop_summary$Hispanic.Origin <- NULL 
  pop_summary$Race <- NULL
  
  fwrite(pop_summary, pop_summaryDir)
  rm(rindreplace1, rindreplace2, rindreplace3)
  toc()
}