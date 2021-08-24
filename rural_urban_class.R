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
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"

  # dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  # tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # pop.summary.dir <- "C:/Users/Daniel/Desktop/paper2021/data/11_population_summary"
}
suppressMessages(
  rural_urban_class_or <- read_excel(file.path(dataDir, "NCHSURCodes2013.xlsx"), .name_repair = "universal") %>%
    rename(rural_urban_class = ..2013.code) %>%
    select(FIPS.code, rural_urban_class)
)

## ---
years <- c(1990, 2000)
pop.sum <- lapply(years, function(year) {
  meta <- file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")) %>%
    fread() %>%
    filter(Gender.Code == "A" & Race == "All" & Hispanic.Origin == "All Origins" &
      Education == 666)

  pop.sum <- lapply(list.files(file.path(censDir, year)), function(file) {
    fread(file.path(censDir, year, file))
  }) %>%
    rbindlist() %>%
    filter(variable %in% meta$variable)

  pop.sum <- pop.sum %>%
    mutate(FIPS.code = paste0(state, str_pad(county, 3, pad = "0"))
    %>% as.integer()) %>%
    group_by(FIPS.code) %>%
    summarise(pop_size = sum(pop_size)) %>%
    mutate(fromYear = year)
}) %>% rbindlist()


### ----
crosswalk90 <- read.csv(file.path(dataDir, paste0("crosswalk_", 1990, "_2010.csv"))) %>%
  select(trtidFrom = trtid90, trtidTo = trtid10) %>%
  mutate(fromYear = 1990)
crosswalk00 <- read.csv(file.path(dataDir, paste0("crosswalk_", 2000, "_2010.csv"))) %>%
  select(trtidFrom = trtid00, trtidTo = trtid10) %>%
  mutate(fromYear = 2000)

crosswalk <- rbind(crosswalk00, crosswalk90)
rm(crosswalk00, crosswalk90)
nrow(crosswalk)
crosswalk <- crosswalk %>%
  transmute(
    countyFrom = trtidFrom %>%
      str_pad(., 11, pad = "0") %>%
      substr(., 0, 5) %>%
      as.integer(),
    countyTo = trtidTo %>%
      str_pad(., 11, pad = "0") %>%
      substr(., 0, 5) %>%
      as.integer(),
    fromYear
  ) %>%
  distinct()

crosswalk <- crosswalk %>%
  left_join(pop.sum, by = c("countyFrom" = "FIPS.code", "fromYear" = "fromYear")) %>%
  group_by(fromYear, countyTo) %>%
  filter(pop_size == max(pop_size)) %>%
  mutate(pop_size = NULL)

filler <- merge(
  data.frame(countyFrom = setdiff(rural_urban_class_or$FIPS.code, crosswalk$countyTo),
             countyTo = setdiff(rural_urban_class_or$FIPS.code, crosswalk$countyTo)),
  data.frame(fromYear = c(1990, 2000, 2010))
)

crosswalk <- rbind(
  crosswalk,
  data.frame(
    fromYear = 2010,
    countyFrom = crosswalk$countyTo %>% unique(),
    countyTo = crosswalk$countyTo %>% unique()
  ),
  filler
)

test_that("rural urban class", {
  expect_false(any(is.na(crosswalk)))
  test <- crosswalk %>%
    group_by(countyTo) %>%
    summarise(n = n(), Years = list(fromYear)) %>%
    filter(n != 3)
  expect_equal(nrow(test), 0)
})
### ----

test_that("rural urban class", {
  expect_false(any(is.na(rural_urban_class_or)))
  test <- rural_urban_class_or %>%
    group_by(FIPS.code) %>%
    summarise(n = n()) %>%
    filter(n != 1)
  expect_equal(nrow(test), 0)
  
  anti_test <- rural_urban_class_or %>%
    anti_join(crosswalk, by = c("FIPS.code" = "countyTo"))
  expect_equal(nrow(anti_test), 0)
})

rural_urban_class <- rural_urban_class_or %>%
  left_join(crosswalk, by = c("FIPS.code" = "countyTo")) %>% 
  distinct

test_that("rural urban class", {
  expect_false(any(is.na(rural_urban_class)))
  test <- rural_urban_class %>%
    group_by(FIPS.code) %>%
    summarise(n = n(), Years = list(fromYear)) %>%
    filter(n != 3)
  expect_equal(nrow(test), 0)
})

rural_urban_class <- rural_urban_class %>%
  mutate(FIPS.code = countyFrom, countyFrom = NULL)

write.csv(rural_urban_class,
  file = file.path(dataDir, "rural_urban_class.csv"),
  row.names = F
)
