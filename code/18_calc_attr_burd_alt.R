#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
censDir <- args[8]
dem_agrDir <- args[9]
agr_by <- args[10]
source <- args[14]
totalBurdenParsed2Dir <- args[17]
attrBurdenDir <- args[18]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2001
  agr_by <- "nation"
  source <- "nvss"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
}

if(agr_by != "nation" & source == "nvss" & year >2004){
  print(paste("in", year, "no geopgraphic identifier for nvss available"))
  quit()
}

dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, agr_by, source, paste0("attr_burd_alt_", toString(year), ".csv"))
# http://web.stanford.edu/~mburke/papers/burke_et_al_wildfire_pnas_2021.pdf

# 32 https://pubmed.ncbi.nlm.nih.gov/29962895/
# https://github.com/burke-lab/wildfire-map-public/blob/main/work/14_figure3.R

#----read some data-----
group_variables <- c("Year", "Race", "Hispanic.Origin", "Education", "Gender.Code", agr_by)
total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
  fread() %>%
  filter(label_cause == "all-cause")
#total_burden <- total_burden %>% rename("Region" := !!agr_by)
#total_burden <- total_burden %>% tibble::add_column(agr_by = agr_by)
total_burden <- total_burden %>%
  dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "source", "measure1", "measure2"))) %>%
  #group_by(Year, Region, agr_by, Race, Hispanic.Origin, Gender.Code, Education, source, measure1, measure2) %>%
  summarise(value = sum(value))

meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
files <- list.files(file.path(dem_agrDir, agr_by, year))
pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
pm_summ <- pm_summ %>% left_join(meta, by = "variable")

# make compatible
#pm_summ <- pm_summ %>% rename("Region" := !!agr_by)
#pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)
pm_summ <- pm_summ %>%
  dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "pm"))) %>%
  #group_by(Year, Region, agr_by, Race, Hispanic.Origin, Gender.Code, Education, pm) %>%
  summarise(pop_size = sum(pop_size))

pm_summ <- pm_summ %>%
  dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education"))) %>%
  summarise(pm_mean = weighted.mean(pm, pop_size))
rm(meta)

group_variables <- c("Year", "Race", "Hispanic.Origin", "Education", "Gender.Code", agr_by)
## ---calculations----
# 29 https://www.nejm.org/doi/full/10.1056/nejmoa1702747
# Increases of 10 μg per cubic meter in PM2.5 and of 10 ppb in ozone were associated with increases in all-cause mortality of 7.3%
# DI 2017

if(!file.exists(attrBurdenDir)){
  attrBurden <- inner_join(total_burden, pm_summ, by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education")) %>%
    mutate(
      lower = value * 0.0071 *pmax(0,pm_mean-5),
      mean = value * 0.0073 *pmax(0,pm_mean-5),
      upper = value * 0.0075 *pmax(0,pm_mean-5),
      attr = "attributable",
      method = "DI",
      pm_mean = NULL,
      value = NULL
    )
  fwrite(attrBurden, attrBurdenDir)
}
