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
packages <- c(
  "dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "truncnorm", "triangle",
  "matrixStats"
)

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
  year <- 2000
  agr_by <- "nation"
  source <- "nvss"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"

  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "C:/Users/Daniel/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/13_attr_burd"
}

# if (agr_by != "nation" & source == "nvss" & year > 2004) {
#  print(paste("in", year, "no geopgraphic identifier for nvss available"))
#  quit()
# }

attrBurdenDir <- file.path(attrBurdenDir, agr_by, source)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd3_", toString(year), ".csv"))

if (!file.exists(attrBurdenDir)) {
  tic(paste("calculated 3rd burden alternative way", year, agr_by, source))
  #----read some data-----
  total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
    fread()

  total_burden <- total_burden %>%
    dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "rural_urban_class", "source", "measure1", "measure2", "label_cause"))) %>%
    summarise(
      value = sum(value),
      min_age = min(min_age),
      max_age = max(max_age)
    )
  test_that("basic check pm summ", {
    total_burden_dupl <- total_burden %>% select(setdiff(colnames(total_burden), c("value")))
    total_burden_dupl <- total_burden_dupl[duplicated(total_burden_dupl), ]
    expect_equal(nrow(total_burden_dupl), 0)
  })
  
  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  files <- list.files(file.path(dem_agrDir, agr_by, year))
  pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
  pm_summ <- pm_summ %>% left_join(meta, by = "variable")
  pm_summ <- pm_summ %>% filter(min_age >= 25)
  #pm_summ <- pm_summ %>% mutate(min_age = min(min_age), max_age = max(max_age))

  pm_summ <- pm_summ %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
  total_burden <- total_burden %>% mutate_at(c("rural_urban_class", "Education"), as.factor)

  pm_summ <- pm_summ %>%
    dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "rural_urban_class", "scenario", "min_age", "max_age"))) %>%
    summarise(pop_weight_pm_exp = weighted.mean(pm, pop_size))

  rm(meta, files)
  test_that("basic check pm summ", {
    pm_summ_dupl <- pm_summ %>% select(setdiff(colnames(pm_summ), c("pop_weight_pm_exp")))
    pm_summ_dupl <- pm_summ_dupl[duplicated(pm_summ_dupl), ]
    expect_equal(nrow(pm_summ_dupl), 0)
  })
  ## ---paf calculations----

  # DI 2017, SI, Table S3
  # hazard ratio
  # Risk of Death associated with increases #TODO
  # Increases of 10 μg per cubic meter in PM2.5 were associated with increases in all-cause mortality of 7.3%
  
  hr <- data.frame(
    method = c(rep("di_gee", 4), rep("di_coxme", 4)),
    Race = c("White", "Black or African American", "Asian or Pacific Islander", "White"), # TODO
    Hispanic.Origin = rep(c("All Origins", "All Origins", "All Origins", "Hispanic or Latino"), 2), # TODO
    hr_mean = c(1.063, 1.208, 1.096, 1.116, 1.068, 1.216, 1.140, 1.127),
    hr_lower = c(1.06, 1.199, 1.075, 1.1, 1.065, 1.206, 1.116, 1.109),
    hr_upper = c(1.065, 1.217, 1.117, 1.133, 1.07, 1.225, 1.164, 1.144)
  )

  paf_di <- inner_join(pm_summ, hr, by = c("Race", "Hispanic.Origin"))
  rm(pm_summ, hr)
  paf_di <- paf_di %>%
    mutate(
      # PAF = 1-1/HR #TODO
      paf_mean = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_mean - 1) / 10
      ),
      paf_lower = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_lower - 1) / 10
      ),
      paf_upper = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_upper - 1) / 10
      ),
      pop_weight_pm_exp = NULL, hr_upper = NULL, hr_mean = NULL, hr_lower = NULL
    )

  attr_burden_di <- inner_join(
    total_burden %>% dplyr::filter(label_cause == "all-cause"),
    paf_di,
    by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education","rural_urban_class")
  ) 
  attr_burden_di <- attr_burden_di%>%
    mutate(mean = value * paf_mean,
           lower = value * paf_lower,
           upper = value * paf_upper,
           paf_mean = NULL, paf_lower = NULL, paf_upper = NULL,
           value = NULL, label_cause = NULL,
           attr = "attributable",
           min_age = min(min_age.x, min_age.y),
           max_age = max(max_age.x, max_age.y),
           min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL)
 
  test_that("basic check attr burden", {
    attr_burden_dupl <- attr_burden_di %>%
      dplyr::group_by_at(vars(one_of(setdiff(colnames(attr_burden_di), c("lower", "mean", "upper"))))) %>%
      summarise(n = n())
    
    attr_burden_dupl <- attr_burden_dupl %>% filter(n != 1)
    test_that(nrow(attr_burden_dupl),0)
  })
  
  fwrite(attr_burden_di, attrBurdenDir)
  toc()
}
