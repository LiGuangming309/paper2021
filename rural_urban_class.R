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
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  
  #dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  #tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  #censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  #pop.summary.dir <- "C:/Users/Daniel/Desktop/paper2021/data/11_population_summary"
}

###----
crosswalk90 <- read.csv(file.path(dataDir, paste0("crosswalk_", 1990,"_2010.csv"))) %>% 
  select(trtidFrom = trtid90, trtidTo = trtid10, weight) %>%
  mutate(fromYear = 1990)
crosswalk00 <- read.csv(file.path(dataDir, paste0("crosswalk_", 2000,"_2010.csv"))) %>% 
  select(trtidFrom = trtid00, trtidTo = trtid10, weight)%>%
  mutate(fromYear = 2000)

crosswalk <- rbind(crosswalk00, crosswalk90)
rm(crosswalk00, crosswalk90)
crosswalk <- crosswalk %>%
  mutate(countyFrom = trtidFrom %>%
           str_pad(., 11, pad = "0") %>%
           substr(., 0, 5) %>%
           as.integer(),
         countyTo = trtidTo %>%
           str_pad(., 11, pad = "0") %>%
           substr(., 0, 5) %>%
           as.integer())

crosswalk <- crosswalk %>%
  group_by(fromYear, countyFrom, countyTo) %>%
  summarise(weight = sum(weight)) %>%
  filter(weight == max(weight)) %>%
  mutate(weight = NULL) #TODO other method, use population size

crosswalk <- rbind(crosswalk,
                   data.frame(fromYear = 2010,
                              countyFrom = crosswalk$countyTo %>% unique,
                              countyTo = crosswalk$countyTo %>% unique))
###----
suppressMessages(
  rural_urban_class <- read_excel(file.path(dataDir, "NCHSURCodes2013.xlsx"), .name_repair = "universal") %>%
    rename(rural_urban_class= ..2013.code) %>%
    select(FIPS.code, rural_urban_class)
)
rural_urban_class <- rural_urban_class %>%
  left_join(crosswalk, by = c("FIPS.code" = "countyTo"))

#rural_urban_class$countyFrom <- apply(rural_urban_class, 1, function(row){
#  replace_na(row[["countyFrom"]],row[["FIPS.code"]])
#})
#rural_urban_class <- rural_urban_class %>%
#  mutate(countyFrom = replace_na(FIPS.code))

#test <- rural_urban_class %>%
#  group_by(FIPS.code) %>%
#  summarise(n = n(), Years = list(fromYear))

rural_urban_class <- rural_urban_class%>%
  mutate(FIPS.code = countyFrom, countyFrom = NULL)

write.csv(rural_urban_class,
          file = file.path(dataDir, "rural_urban_class.csv"))
