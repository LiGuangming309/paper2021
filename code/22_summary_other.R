#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory 
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
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog/"
  dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr/"
  pop.summary.dir <- "C:/Users/Daniel/Desktop/paper2021/data/11_population_summary/"
  summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
  
 # tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
#  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
#  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
#  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
#  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
}

#intense computation
if (Sys.info()["sysname"] == "Windows") memory.limit(size=500000)

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
      if(nrow(pm_summ) >0){
        pm_summ<- pm_summ %>% left_join(meta, by = "variable")
        return(pm_summ)
      }else{
        return(NULL)
      }
      
    }) %>% rbindlist
    #make compatible
    pm_summ <- pm_summ %>% rename("Region":=!!agr_by)
    pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)
    
    pm_summ <- pm_summ %>%
      group_by_at(vars(all_of(setdiff(colnames(pm_summ),c("variable","pop_size","prop","min_age", "max_age"))))) %>%
      summarise(pop_size = sum(pop_size))
    toc()
    return(pm_summ)
  }) %>% rbindlist
  
  pm_summ <- pm_summ %>%
    group_by_at(vars(all_of(setdiff(colnames(pm_summ),c("variable","pop_size","prop","min_age", "max_age","pm"))))) %>%
    #group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, scenario, Education) %>%
    summarise(mean = weighted.mean(pm, pop_size),
              median = matrixStats::weightedMedian(pm, pop_size))
  
  pm_summ <- pm_summ %>%
    pivot_longer(c(mean,median),
                 names_to = "pm_metric")

  pm_summ <- pm_summ %>% filter(!is.na(Gender.Code)) #TODO
  ##--- find and replcase---
  pm_summ <- pm_summ %>% mutate_at(setdiff(colnames(pm_summ), c("value")),
                                         as.factor)
  
  rindreplace1 <- c(states$STATEFP, "us") %>% as.list
  names(rindreplace1) <- c(states$NAME, "United States")
  levels(pm_summ$Region) <- rindreplace1
  
  rindreplace2 <- list("high school graduate or lower" = "lower", 
                       "some college education but no 4-year college degree" = "middle", 
                       "4-year college graduate or higher" ="higher",
                       "666" = "666")
  levels(pm_summ$Education) <- rindreplace2

  rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
  levels(pm_summ$Gender.Code) <- rindreplace3
  
  pm_summ <- pm_summ %>% 
    unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
    mutate(Ethnicity = as.factor(Ethnicity))
  rindreplace7 <- list(
    "Black or African American" = "Black or African American, All Origins",
    "American Indian or Alaska Native" = "American Indian or Alaska Native, All Origins",
    "Asian or Pacific Islander" = "Asian or Pacific Islander, All Origins" ,
    "White, Hispanic or Latino" = "White, Hispanic or Latino",
    "White, Not Hispanic or Latino" = "White, Not Hispanic or Latino",
    "White, All Origins" = "White, All Origins",
    "All, All Origins" = "All, All Origins"
  )
  levels(pm_summ$Ethnicity) <- rindreplace7
  
  #rindreplace8 <- list("large central metro" = 1, "large fringe metro" = 2, "medium metro" = 3, "small metro" = 4, "micropolitan" = 5, "non-core" = 6,"All" = 666,"Unknown" = "Unknown")
  rindreplace8 <- list("large metro" = 1, "small-medium metro" = 2,  "non metro" = 3, "All" = 666,"Unknown" = "Unknown")
  levels(pm_summ$rural_urban_class) <- rindreplace8
  
  fwrite(pm_summ, pm_summDir)
  rm(rindreplace1, rindreplace2, rindreplace3,rindreplace7,rindreplace8)
  toc()
}

### --- read population data-----
pop_summaryDir <- file.path(summaryDir, "pop_summary.csv")
if(!file.exists(pop_summaryDir)){
  tic("summarized population data")
  files <- setdiff(list.files(pop.summary.dir),c("plot","county", "pop_county.csv"))
  files1 <- files[endsWith(files,".csv")]
  pop_summary1 <- lapply(files1, function(file){
    pop_summary1 <- fread(file.path(pop.summary.dir, file))
    #make compatible
    agr_by <- str_sub(file,5,-5)
    pop_summary1 <- pop_summary1 %>% rename("Region":=!!agr_by)
    pop_summary1 <- pop_summary1 %>% tibble::add_column(agr_by = agr_by)
  })%>% rbindlist()
  
  agr_bys <- files[!endsWith(files,".csv")]
  agr_bys <- setdiff(agr_bys,"county")
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
    group_by_at(vars(all_of(setdiff(colnames(pop_summary),c("Population","min_age", "max_age"))))) %>%
    summarise(Population = sum(Population))
  ###---find and replace----
  pop_summary <- pop_summary %>% mutate_at(setdiff(colnames(pop_summary), c("Population")),
                                   as.factor)
  rindreplace1 <- c(states$STATEFP, "us") %>% as.list
  names(rindreplace1) <- c(states$NAME, "United States")
  levels(pop_summary$Region) <- rindreplace1
  
  rindreplace2 <- list("high school graduate or lower" = "lower", 
                       "some college education but no 4-year college degree" = "middle", 
                       "4-year college graduate or higher" ="higher",
                       "666" = "666")
  levels(pop_summary$Education) <- rindreplace2
  
  rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
  levels(pop_summary$Gender.Code) <- rindreplace3

  rindreplace4 <- list("Official Bridged-Race Population Estimates" = "CDC", "Own Interpolation" = "Census")
  levels(pop_summary$source2) <- rindreplace4
  
  pop_summary <- pop_summary %>% 
    unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
    mutate(Ethnicity = as.factor(Ethnicity))
  rindreplace7 <- list(
    "Black or African American" = "Black or African American, All Origins",
    "American Indian or Alaska Native" = "American Indian or Alaska Native, All Origins",
    "Asian or Pacific Islander" = "Asian or Pacific Islander, All Origins" ,
    "White, Hispanic or Latino" = "White, Hispanic or Latino",
    "White, Not Hispanic or Latino" = "White, Not Hispanic or Latino",
    "White, All Origins" = "White, All Origins",
    "All, All Origins" = "All, All Origins"
  )
  levels(pop_summary$Ethnicity) <- rindreplace7
  
  #test <- pop_summary %>% filter(!is.na(rural_urban_class)) 
  
  #rindreplace8 <- c("large central metro" = 1, "large fringe metro" = 2, "medium metro" = 3, "small metro" = 4, "micropolitan" = 5, "non-core" = 6,"All" = 666,"Unknown" = "Unknown")
  rindreplace8 <- c("large metro" = 1, "small-medium metro" = 2,  "non metro" = 3, "All" = 666,"Unknown" = "Unknown")
  pop_summary <- pop_summary %>% 
    mutate(rural_urban_class = rural_urban_class %>%
             as.character() %>%
             replace_na(.,"Unknown"))

  #TODO
  pop_summary$rural_urban_class <- sapply(pop_summary$rural_urban_class %>% as.character, function(x){
    x <- replace_na(x,"Unknown")
    rindreplace8[[x]]
  } )

  
  fwrite(pop_summary, pop_summaryDir)
  rm(rindreplace1, rindreplace2, rindreplace3)
  toc()
}