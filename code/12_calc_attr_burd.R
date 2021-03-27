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
dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenParsedDir <- args[13]
attrBurdenDir <- args[14]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2011
  agr_by <- "nation"

  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/10_attr_burd"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv"))

#read some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
total_burden <- file.path(totalBurdenParsedDir,  agr_by, "total_burden.csv") %>% read.csv

#ethn_suppr <- file.path(tmpDir, "ethn_suppr.csv") %>% #TODO delete
#  read.csv() %>%
#  select(Race, Hispanic.Origin, label_cause, factor)
## ----calculations-----
if (!file.exists(attrBurdenDir)) {
  print("test2")
  ## ----determine join variables
  join_variables <- c(
    "Year" = "year",
    "Race" = "race",
    "Hispanic.Origin" = "hispanic_origin",
    "label_cause" = "label_cause"
  )
  
  if(agr_by == "nation"){
    join_variables <- c(join_variables, "Gender.Code" = "gender_label")
  }
  
  group_variables <- c(
    "Year" = "year",
    "Race" = "race",
    "Hispanic.Origin" = "hispanic_origin"
  )

  agr_by_replace <- c(
    "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
    "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county" = "County.Code"
  )
  agr_by_new <- agr_by_replace[[agr_by]]
  join_variables[agr_by_new] <- agr_by
  group_variables[agr_by_new] <- agr_by

  inverse_join_variables <- setNames(names(join_variables), join_variables)
  inverse_group_variables <- setNames(names(group_variables), group_variables)
  ## ----- read paf------
  regions <- states[, agr_by] %>% unique()

  pafs <- lapply(regions, function(region) {
    file.path(pafDir, agr_by, year, paste0("paf_", toString(year), "_", region, ".csv")) %>%
      fread()
  }) %>%
    do.call(rbind, .) %>%
    # TODO Asian, Pacific Islander immer noch dabei
    filter(!(race %in% c("ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER"))) %>%
    # TODO old people still included
    filter(!(100 <= min_age & max_age < 150 | 100 < min_age)) %>%
    as.data.frame()
  print("test3")
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
  missing_rows <- anti_join(filter(total_burden, label_cause != "all-cause") , pafs, by = join_variables)
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in pafs data for", agr_by, ":"))
    print(head(missing_rows))
  }
  
  # other side
  missing_rows <- anti_join(pafs, total_burden, by = inverse_join_variables) %>%
    select(gender, race, hispanic_origin) %>%
    distinct()
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in total burden data for", agr_by, ":"))
    print(head(missing_rows))
  }
  
  ## ----- join total_burden and pafs-----
  print("test")
  total_burden_cause <- total_burden %>% filter(label_cause != "all-cause")
  tic("1")
  burden_paf <- inner_join(total_burden_cause, pafs, by = join_variables)
  toc()
  
  tic("2")
  # filter those, where age in correct interval
  burden_paf <- burden_paf %>%
    filter(
      (min_age.x <= min_age.y & max_age.y <= max_age.x) |
      (min_age.y <= min_age.x & max_age.x <= max_age.y)
    )
  toc()
  ## ----- calculate attributable burden------
  tic("3")
  test_that("09_calc distinct rows", {
    burden_paf_sub <- burden_paf %>%
      select(min_age.x, max_age.x, measure, Gender.Code, Race, Hispanic.Origin, label_cause)

    dub_ind <- duplicated(burden_paf_sub) | duplicated(burden_paf_sub, fromLast = TRUE)
    burden_paf_sub <- burden_paf[dub_ind, ]

    expect_equal(nrow(burden_paf_sub), 0)
  })
  toc()
  tic(4)
  burden_paf <- pivot_longer(burden_paf,
                             cols = colnames(burden_paf) %>% grep('draw', ., value=TRUE),
                             names_to = "draw", 
                             values_to = "paf") 
  toc()
  
  tic(5)
  attrBurden <- burden_paf %>%
    mutate(
      value = value * paf,
      attr = "attributable",
      paf = NULL
    )
  toc()

  # group "out" ages
  tic(6)
  columns <- c(unname(inverse_group_variables), "draw", "measure", "attr")
  attrBurden <- attrBurden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(value = sum(value)
    )
  toc()
  
  #group "out" draw, mean and confidence interval
  tic(7)
  columns <- c(unname(inverse_group_variables), "measure", "attr")
  attrBurden <- attrBurden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(mean = mean(value),
                     lower = quantile(value,p=.025),
                     upper = quantile(value,p=.975) 
              )
  toc()
  # some basic tests
  test_that("09_read burden join2", {
    expect_false(any(is.na(attrBurden)))

    # test that total number of deaths/YLL has not changed
    #columns <- unname(inverse_join_variables)
    #comp1 <- total_burden %>%
    #  group_by_at(vars(one_of(columns))) %>%
    #  summarize(Deaths = sum(Deaths), YLL = sum(YLL))

    #comp2 <- attrBurden %>%
    #  group_by_at(vars(one_of(columns))) %>%
    #  summarize(Deaths = sum(Deaths), YLL = sum(YLL))

    #comp3 <- inner_join(comp1, comp2, by = columns)

    #expect_equal(comp3$Deaths.x, comp3$Deaths.x)
    #expect_equal(comp3$YLL.x, comp3$YLL.x)
  })

  fwrite(attrBurden, attrBurdenDir)
}
