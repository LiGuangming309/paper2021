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
agr_by <- args[10]
pafDir <- args[11]
source <- args[14]
totalBurdenParsed2Dir <-args[17]
attrBurdenDir <- args[18]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2000
  agr_by <- "STATEFP"
  source <- "nvss"

  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by, source)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv"))

if(agr_by != "nation" & source == "nvss" & year >2004){
  print(paste("in", year, "no geopgraphic identifier for nvss available"))
  quit()
}

#read some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
total_burden <- file.path(totalBurdenParsed2Dir,agr_by,source, paste0("total_burden_",year,".csv")) %>% 
  fread%>% 
  filter(label_cause != "all-cause")

#intense computation
if (Sys.info()["sysname"] == "Windows") memory.limit(size=500000)

## ----calculations-----
if (!file.exists(attrBurdenDir)) {
  ## ----determine join variables
  join_variables <- c("Year", "Race", "Hispanic.Origin","Education","Gender.Code", "label_cause","min_age","max_age", agr_by)
  group_variables <- c("Year","Race","Hispanic.Origin","Education", "Gender.Code", agr_by)

  ## ----- read paf------
  regions <- states[, agr_by] %>% unique()

  tic("calc_attr_burd: 1 read all PAFs")
  pafs <- lapply(regions, function(region) {
    file.path(pafDir, agr_by, year, paste0("paf_", toString(year), "_", region, ".csv")) %>%
      fread()
  }) %>%
    rbindlist %>%
    as.data.frame()

  if (agr_by == "STATEFP") {
    possible_regions <- c(1, 4:6, 8:13, 16:42, 44:51, 53:56)
  } else if (agr_by == "Census_Region") {
    pafs$Census_Region <- paste("CENS-R", pafs$Census_Region)
    possible_regions <- paste("CENS-R", 1:4)
  } else if (agr_by == "nation") {
    possible_regions <- "us"
  } else if (agr_by == "Census_division") {
    pafs$Census_division <- paste("CENS-D", pafs$Census_division)
    possible_regions <- paste("CENS-D", 1:9)
  } else if (agr_by == "hhs_region_number") {
    pafs$hhs_region_number <- paste("HHS", pafs$hhs_region_number)
    possible_regions <- paste("HHS", 1:10)
  } else if (agr_by == "county") {
    # pafs$county<-sprintif(%02d,03d,pafs$state,pafs$county)
    # TODO
    pafs$state <- NULL
    possible_regions <- c() # TODO too difficult
  }

  # missing regions
  missing <- setdiff(possible_regions, pafs[, agr_by])
  if (length(missing) > 0) {
    print("Regions in paf data missing:")
    print(missing)
  }
  # give some feedback on what is still missing from total burden
  # one side
  test_variables <- setdiff(join_variables,c("min_age","max_age",agr_by))
  total_burden_test <- total_burden %>%
    select(all_of(test_variables)) %>%
    distinct()
  
  pafs_test <- pafs %>%
    select(all_of(test_variables)) %>%
    distinct()
  
  missing <- anti_join(total_burden_test , pafs_test, by = test_variables) 
  if (nrow(missing) > 0) {
    print(paste(nrow(missing), "rows are still missing in pafs data for", agr_by, ":"))
    print(head(missing))
  }
  
  # other side
  missing <- anti_join(pafs_test, total_burden_test, by = test_variables) 
  if (nrow(missing) > 0) {
    print(paste(nrow(missing), "rows are still missing in total burden data for", agr_by, ":"))
    print(head(missing))
  }
  rm(missing, total_burden_test, pafs_test, test_variables)
  toc()
  ## ----- join total_burden and pafs-----
  tic("calc_attr_burd: 2 joined PAFs and total burden data")
  
  burden_paf <- inner_join(total_burden, pafs, by = join_variables)
  rm(pafs)
  toc()
  #tic("calc_attr_burd: 3 filtered wrong age combinations")
  #test_that("paf min_age and max_age compatible with total burden",{
  #  burden_paf_test <- burden_paf %>% filter(min_age.x < min_age.y & max_age.y < max_age.x)
  #  expect_equal(0,nrow(burden_paf_test))
  #})
  
  #burden_paf <- burden_paf %>% 
  #   filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
  #   mutate(min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
  #           min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL)
  #toc()
  
  ## ----- calculate attributable burden------
  tic("calc_attr_burd: 3 pivot_longer")
  test_that("09_calc distinct rows", {
    burden_paf_sub1 <- burden_paf %>% select(c(join_variables,"measure1","measure2"))
    burden_paf_sub1 <- burden_paf_sub1[duplicated(burden_paf_sub1), ]
    expect_equal(nrow(burden_paf_sub1), 0)
  })

  burden_paf <- pivot_longer(burden_paf,
                             cols = colnames(burden_paf) %>% grep('draw', ., value=TRUE),
                             names_to = "draw", 
                             values_to = "paf") 
  toc()
  
  tic("calc_attr_burd: 4 calculated attributable burden")
  attrBurden <- burden_paf %>%
    mutate(
      value = value * paf,
      attr = "attributable",
      paf = NULL
    )
  rm(burden_paf)
  toc()

  # group "out" ages
  tic("calc_attr_burd: 5 grouped by group_variables and draw")
  columns <- c(group_variables, "draw", "measure1","measure2", "attr")
  attrBurden <- attrBurden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(value = sum(value),
                     min_age = pmin(min_age),
                     max_age = pmax(max_age)
    )
  toc()
  
  #group "out" draw, mean and confidence interval
  tic("calc_attr_burd: 6 grouped out draws, calculate mean, lower, upper")
  columns <- c(group_variables, "measure1","measure2", "attr")
  attrBurden <- attrBurden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(mean = mean(value),
                     lower = quantile(value,p=.025),
                     upper = quantile(value,p=.975) 
              )
  attrBurden <- attrBurden %>% tibble::add_column(source = source)
  attrBurden <- attrBurden %>% tibble::add_column(method = "GBD")
  toc()
  fwrite(attrBurden, attrBurdenDir)
}
