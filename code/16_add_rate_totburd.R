#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: read total burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "readxl")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenParsedDir <- args[13]
source <- args[14]
pop.summary.dir  <- args[16]
totalBurdenParsed2Dir <-args[17]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2003
  agr_by <- "STATEFP"
  source <- "nvss"
  
  dataDir <- "/Users/default/Desktop/paper2021/data"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
}

totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir,agr_by,source)
dir.create(totalBurdenParsed2Dir, recursive = T, showWarnings = F)
totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir,paste0("total_burden_",year,".csv"))

if(agr_by != "nation" & source == "nvss" & year >2004){
  print(paste("in", year, "no geopgraphic identifier for nvss available"))
  quit()
}


##----calculations----
if(!file.exists(totalBurdenParsed2Dir)){
  if(source == "wonder"){
    total_burden <- file.path(totalBurdenParsedDir,  agr_by, "total_burden_wond.csv") %>% 
      read.csv %>% 
      filter(Year == year)
  }else if(source == "nvss"){
    total_burden <- file.path(totalBurdenParsedDir,  agr_by,"nvss", paste0("total_burden_nvss_",year,".csv")) %>% 
      read.csv 
  }
  
  ## --- measure 1: Deaths and YLL-----
  tic(paste("added YLL and age-adjusted rate to total burden in",year,agr_by))
  # Deaths
  total_burden$measure1 <- "Deaths"
  total_burden <- total_burden %>% dplyr::rename(value = Deaths)
  
  # YLL
  lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
  total_burden_yll <- total_burden %>%
    dplyr::mutate(
      Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(max_age, lifeExpectancy$Age)], # TODO max_age?
      value = value * Life.Expectancy,
      measure1 = "YLL",
      Life.Expectancy = NULL
    )
  
  total_burden <- rbind(total_burden, total_burden_yll)
  rm(lifeExpectancy, total_burden_yll)
  #---read population data----
  pop_summary1 <- file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv")) %>% 
    read.csv() %>%
    filter(Year == year)
  
  pop_summary2 <- file.path(pop.summary.dir, agr_by, paste0("pop_sum_",year,  ".csv")) %>% 
    read.csv() %>%
    filter(Year == year & Education != 666)
  pop_summary <- rbind(pop_summary1, pop_summary2)
  rm(pop_summary1, pop_summary2)
  #------measure 2: absolute number, crude rate and age-standartised rates----- 
  # absolute number
  total_burden$measure2 <- "absolute number"
  
  # crude rate
  pop_summary_agr <- pop_summary %>%
    group_by_at(vars(all_of(setdiff(colnames(pop_summary),c("min_age","max_age", "Population"))))) %>%
    summarise(Population = sum(Population))
  
  test_that("add_rate anti join total burden with population",{
    #expect_false(any(is.na(total_burden_crude)))
    test_anti_join <- total_burden %>% anti_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), "Population"))
    if(year <= 2008) test_anti_join <- test_anti_join %>% filter(Education == 666)
    
    test_anti_join <- test_anti_join %>%
      select(all_of(c("Year", "Education", "Gender.Code", "Race", "Hispanic.Origin", agr_by))) %>%
      distinct
    expect_equal(0, nrow(test_anti_join)) #TODO
  })
  
  total_burden_crude <- total_burden %>%
    left_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), "Population")) %>%
    mutate(
      value = value * (100000/ Population),
      measure2 = "crude rate",
      Population = NULL
    )
  total_burden_crude <- total_burden_crude %>% filter(!is.na(value)) #TODO
  rm(pop_summary_agr)
  
  # age-standartised rates
  # see https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf, page 125 for more information
  standartpopulation <- read_excel(file.path(dataDir, "standartpopulation.xlsx"))
  full_stand_popsize <- sum(standartpopulation$popsize)
  
  pop_summary_agr <- pop_summary %>% 
    group_by_at(vars(all_of(setdiff(colnames(pop_summary),c("Population"))))) %>% 
    summarise(Population = sum(Population))

  total_burden_age_adj <- total_burden %>% left_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), c("min_age","max_age","Population")))
  
  #group by new age
  #case 1 age group population inside age group total burden
  total_burden_age_adj1 <- total_burden_age_adj %>%
    filter(min_age.x <= min_age.y & max_age.y <= max_age.x) %>% 
    mutate(
      min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
      min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL
    ) 
  
  total_burden_age_adj1 <- total_burden_age_adj1 %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj1),"Population")))) %>% #Population
    summarise(Population = sum(Population)) %>%
    ungroup()
  
  #case 2 age group total burden inside age group population 
  total_burden_age_adj2 <- total_burden_age_adj %>%
    filter(min_age.y < min_age.x & max_age.x < max_age.y) %>%
    mutate(min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
      min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL) 
  
  total_burden_age_adj2 <- total_burden_age_adj2%>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj2),"value")))) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  
  #test_that("population min_age and max_age compatible with total burden",expect_equal(0,nrow(total_burden_age_adj2)))
  #TODO entcomment
  
  #combine cases
  total_burden_age_adj <- rbind(total_burden_age_adj1, total_burden_age_adj2) %>% distinct
  rm(total_burden_age_adj1, total_burden_age_adj2)
  
  #calculate age-adjusted rate
  total_burden_age_adj <- total_burden_age_adj %>%
    dplyr::mutate(
      value = value * (standartpopulation$popsize[findInterval(max_age, standartpopulation$max_age)] / Population) * (100000 / full_stand_popsize), # TODO max_age?
      measure2 = "age-adjusted rate",
      Population = NULL
    )
  
  total_burden <- rbind(total_burden, total_burden_crude, total_burden_age_adj)
  rm(total_burden_crude, total_burden_age_adj, standartpopulation,  full_stand_popsize,pop_summary_agr,pop_summary)
  ##---aggregate total burden by ages available in PAF file------
  pafDir <- file.path(pafDir, agr_by, year)
  paf <- file.path(pafDir, list.files(pafDir)[[1]]) %>% read.csv
  paf <- paf %>% 
    select(Hispanic.Origin, Race, Education, min_age, max_age, Year) %>% 
    distinct() %>%
    arrange(min_age, max_age)
  
  total_burden <- total_burden %>% left_join(paf, by=c("Hispanic.Origin", "Race", "Education", "Year")) 
  
  test_that("paf min_age and max_age compatible with total burden",{
    total_burden_test <- total_burden %>% filter(min_age.x < min_age.y & max_age.y < max_age.x)
    #test <- total_burden %>% filter(Education != "666")
    expect_equal(0,nrow(total_burden_test))
  })
  
  #case 1: total_burden inside pad
  #good
  total_burden <- total_burden %>% 
   filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
   mutate(min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
           min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL)
  
  total_burden <- total_burden %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden),"value")))) %>% 
    summarise(value = sum(value)) %>%
    ungroup()
  
  rm(paf, pafDir)
  ##----finish------
  test_that("basic check",{
    expect_false(any(is.na(total_burden)))
    
    total_burden_test <- total_burden %>% select(setdiff(colnames(total_burden),c("value","attr")))
    total_burden_test <- total_burden_test[duplicated(total_burden_test), ]
    expect_equal(nrow(total_burden_test), 0) #TODO
  })
  
  total_burden <- total_burden %>% 
    filter((measure1 == "Deaths" & measure2 == "age-adjusted rate") | 
           (measure1 == "YLL" & measure2 == "crude rate"))
  
  fwrite(total_burden,totalBurdenParsed2Dir)
  toc()
}
#compare with https://wonder.cdc.gov/controller/saved/D77/D144F454
#https://wonder.cdc.gov/controller/saved/D77/D144F456
