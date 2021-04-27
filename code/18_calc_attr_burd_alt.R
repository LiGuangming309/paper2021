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
cens_agrDir <- args[9]
agr_by <- args[10]
source <- args[14]
totalBurdenParsed2Dir <-args[17]
attrBurdenDir <- args[18]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2001
  agr_by <- "nation"
  source <- "nvss"
  
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
  
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv"))
#http://web.stanford.edu/~mburke/papers/burke_et_al_wildfire_pnas_2021.pdf

#32 https://pubmed.ncbi.nlm.nih.gov/29962895/
#https://github.com/burke-lab/wildfire-map-public/blob/main/work/14_figure3.R

#----read some data-----
total_burden <- file.path(totalBurdenParsed2Dir,agr_by,source, paste0("total_burden_",year,".csv")) %>% 
  fread%>% 
  filter(label_cause == "all-cause")

meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
files <- list.files(file.path(dem_agrDir, agr_by, year))
pm_summ<-lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist
pm_summ<- pm_summ %>% left_join(meta, by = "variable")

#make compatible
pm_summ <- pm_summ %>% rename("Region":=!!agr_by)
pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)
pm_summ <- pm_summ %>%
  group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, Education) %>%
  summarize(pop_size = sum(pop_size))
rm(meta)

##---calculations----
#29 https://www.nejm.org/doi/full/10.1056/nejmoa1702747 
# Increases of 10 μg per cubic meter in PM2.5 and of 10 ppb in ozone were associated with increases in all-cause mortality of 7.3% 
# DI 2017
rate = 0.0073
di = function(X, r=rate) {Y=1+(r*(X-5)); Y[X<5]=1; return((Y-1)*all_bmr)}  #APPROXIMATES LINE FROM DI