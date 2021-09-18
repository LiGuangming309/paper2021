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
packages <- c("dplyr", "DataCombine", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
totalBurdenParsed2Dir <- args[5]
attrBurdenDir <- args[6]
summaryDir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"

   #tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
   #totalBurdenParsed2Dir <-"C:/Users/Daniel/Desktop/paper2021/data/12_total_burden_parsed2"
   # attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/13_attr_burd"
  #
  #summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
}

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  select(NAME, STATEFP)

tic("summarized all burden and attributable burden data")
## --- read attr burden----
agr_bys <- list.files(attrBurdenDir)
attrBurden <- lapply(agr_bys, function(agr_by) {
  sources <- list.files(file.path(attrBurdenDir, agr_by))
  attrBurden <- lapply(sources, function(source) {
    files <- list.files(file.path(attrBurdenDir, agr_by, source))
    attrBurden <- lapply(files, function(file) fread(file.path(attrBurdenDir, agr_by, source, file))) %>% rbindlist(use.names = TRUE)
    #print("TEST")#54
    #names(attrBurden[[54]] )
    #setdiff(names(attrBurden[[55]] ), names(attrBurden[[54]] ))
  }) %>% rbindlist(use.names = TRUE)

  # make compatible
  attrBurden <- attrBurden %>% rename("Region" := !!agr_by)
  attrBurden <- attrBurden %>% tibble::add_column(agr_by = agr_by)
  return(attrBurden)
}) %>%
  rbindlist(use.names = TRUE) %>%
  as.data.frame()

test_that("basic check attr burden", {
  attrBurden_dupl <- attrBurden %>% select(setdiff(colnames(attrBurden), c("lower", "mean", "upper")))
  attrBurden_dupl <- attrBurden_dupl[duplicated(attrBurden_dupl), ]
  expect_equal(nrow(attrBurden_dupl), 0)
})
## --- read all burden----
agr_bys <- list.files(totalBurdenParsed2Dir)
all_burden <- lapply(agr_bys, function(agr_by) {
  sources <- list.files(file.path(totalBurdenParsed2Dir, agr_by))
  all_burden <- lapply(sources, function(source) {
    files <- list.files(file.path(totalBurdenParsed2Dir, agr_by, source))
    all_burden <- lapply(files, function(file) fread(file.path(totalBurdenParsed2Dir, agr_by, source, file))) %>% do.call(rbind, .)
  }) %>% rbindlist()

  # make compatible
  all_burden <- all_burden %>% rename("Region" := !!agr_by)
  all_burden <- all_burden %>% tibble::add_column(agr_by = agr_by)
  return(all_burden)
}) %>%
  rbindlist() %>%
  as.data.frame()

all_burden <- all_burden %>% filter(label_cause == "all-cause")
test_that("basic check attr burden", {
  all_burden_dupl <- all_burden %>% select(setdiff(colnames(all_burden), c("value", "label_cause")))
  all_burden_dupl <- all_burden_dupl[duplicated(all_burden_dupl), ]
  expect_equal(nrow(all_burden_dupl), 0)
})

group_variables <- setdiff(colnames(attrBurden), c("lower", "mean", "upper", "method", "min_age", "max_age", "scenario"))
# group_variables <- setdiff(colnames(all_burden), c("min_age", "max_age", "value"))
# setdiff(group_variables2, group_variables)

all_burden <- all_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    overall_value = sum(value) # ,
    # min_age = min(min_age), max_age = max(max_age)
  ) %>%
  mutate(label_cause = NULL)

attrBurden <- attrBurden %>% mutate_at(
  setdiff(colnames(attrBurden), c("mean", "lower", "upper")),
  as.factor
)

all_burden <- all_burden %>% mutate_at(
  setdiff(colnames(all_burden), c("overall_value")),
  as.factor
)
nrow(attrBurden) / nrow(all_burden)
### ----- add proportion ---

# add "prop. of overall burden", "prop. of total burden"
# join everything
attrBurden_prop <- attrBurden %>% left_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
# attrBurden_prop <- attrBurden %>% inner_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))

# calculations
attrBurden_prop <- attrBurden_prop %>%
  mutate(
    mean = coalesce(100 * mean / overall_value, 0),
    lower = coalesce(100 * lower / overall_value, 0),
    upper = coalesce(100 * upper / overall_value, 0),
    measure3 = case_when(attr.y == "overall" ~ "prop. of overall burden", TRUE ~ "prop. of total burden"),
    overall_value = NULL, attr.x = NULL, attr.y = NULL,
    attr = "attributable"
  )

test_that(" basic checks", {
  test1 <- attrBurden %>% anti_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
  expect_equal(0, nrow(test1))

  test <- attrBurden[rowSums(is.na(attrBurden)) > 0, ]
  expect_false(any(is.na(attrBurden)))
  new_DF <- attrBurden_prop[rowSums(is.na(attrBurden_prop)) > 0, ]

  expect_false(any(is.na(attrBurden_prop))) # TODO
  expect_false(any(is.na(all_burden)))
  # TODO
})

# add proportion of disparity#"rural_urban_class", TODO
attrBurden_disp1 <- inner_join(
  all_burden %>% filter(attr == "overall" & !(Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  all_burden %>% filter(attr == "overall" & (Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  by = setdiff(colnames(all_burden), c("Race", "Hispanic.Origin", "overall_value"))
)

attrBurden_disp2 <- inner_join(
  attrBurden %>% filter(!(Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  attrBurden %>% filter((Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  by = setdiff(colnames(attrBurden), c("Race", "Hispanic.Origin", "lower", "mean", "upper"))
)

attrBurden_disp3 <- inner_join(
  attrBurden_disp1,
  attrBurden_disp2,
  by = setdiff(colnames(attrBurden_disp1), c("attr", "overall_value.x", "overall_value.y"))
)

attrBurden_disp3 <- attrBurden_disp3 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to Black or African American attributable", # "prop. of disp.",#
  Race = Race.x, Hispanic.Origin = Hispanic.Origin.x,
  Race.x = NULL, Race.y = NULL, Hispanic.Origin.x = NULL, Hispanic.Origin.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

attrBurden_disp4 <- inner_join(all_burden %>% filter(attr == "overall" & Education != "lower"),
  all_burden %>% filter(attr == "overall" & Education == "lower"),
  by = setdiff(colnames(all_burden), c("Education", "overall_value"))
)

attrBurden_disp5 <- inner_join(attrBurden %>% filter(Education != "lower"),
  attrBurden %>% filter(Education == "lower"),
  by = setdiff(colnames(attrBurden), c("Education", "lower", "mean", "upper"))
)

attrBurden_disp6 <- inner_join(
  attrBurden_disp4,
  attrBurden_disp5,
  by = setdiff(colnames(attrBurden_disp4), c("attr", "overall_value.x", "overall_value.y"))
  # by = c("Education.x", "Education.y", "Race", "Hispanic.Origin","Year", "Gender.Code", "Region", "measure1", "measure2", "source", "agr_by")
)

attrBurden_disp6 <- attrBurden_disp6 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to lower educational attainment",
  Education = Education.x,
  Education.x = NULL, Education.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

attrBurden$measure3 <- "value"
attrBurden <- rbind(attrBurden, attrBurden_prop, attrBurden_disp3, attrBurden_disp6)
rm(attrBurden_prop, attrBurden_disp1, attrBurden_disp2, attrBurden_disp3, attrBurden_disp4, attrBurden_disp5, attrBurden_disp6)

# TODO
# paf_epa <- cbind(pm_summ_var,
#                 lower = matrixStats::rowQuantiles(paf_epa, probs = .025),
#                 mean = rowMeans(paf_epa),
#                 upper = matrixStats::rowQuantiles(paf_epa, probs = .975),
#                 method = "EPA"
# )


## --Find replace----

rindreplace1 <- c(states$STATEFP, "us") %>% as.list()
names(rindreplace1) <- c(states$NAME, "United States")
levels(all_burden$Region) <- rindreplace1
levels(attrBurden$Region) <- rindreplace1

# c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"),
# c(1:7, 666)

rindreplace2 <- list(
  "high school graduate or lower" = "lower",
  "some college education but no 4-year college degree" = "middle",
  "4-year college graduate or higher" = "higher",
  "666" = "666"
)
levels(all_burden$Education) <- rindreplace2
levels(attrBurden$Education) <- rindreplace2

rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
levels(all_burden$Gender.Code) <- rindreplace3
levels(attrBurden$Gender.Code) <- rindreplace3

rindreplace4 <- list("National Vital Statistics System" = "nvss", "Mortality Data from CDC WONDER" = "wonder")
levels(all_burden$source) <- rindreplace4
levels(attrBurden$source) <- rindreplace4

# rindreplace5 <- setNames(c("Years of Life Lost (YLL)", "Deaths"), c("YLL","Deaths"))
# all_burden$measure1 <- sapply(all_burden$measure1 , function(x) rindreplace5[[x]])
# attrBurden$measure1 <- sapply(attrBurden$measure1 , function(x) rindreplace5[[x]])

rindreplace6 <- list("crude rate per 100,000" = "crude rate", "age-adjusted rate per 100,000" = "age-adjusted rate", "absolute number" = "absolute number")
levels(all_burden$measure2) <- rindreplace6
levels(attrBurden$measure2) <- rindreplace6

all_burden <- all_burden %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))
attrBurden <- attrBurden %>%
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
levels(all_burden$Ethnicity) <- rindreplace7
levels(attrBurden$Ethnicity) <- rindreplace7

rindreplace8 <- list(
  "large central metro" = 1,
  "large fringe metro" = 2,
  "medium metro" = 3,
  "small metro" = 4,
  "micropolitan" = 5,
  "non-core" = 6,
  "All" = 666,
  "Unknown" = "Unknown"
)
levels(all_burden$rural_urban_class) <- rindreplace8
levels(attrBurden$rural_urban_class) <- rindreplace8

rm(rindreplace1, rindreplace2, rindreplace3, rindreplace4, rindreplace6, rindreplace7, rindreplace8)
## --- test final---
test_that("basic check attr burden", {
  all_burden_dupl <- all_burden %>% select(setdiff(colnames(all_burden), c("overall_value")))
  all_burden_dupl <- all_burden_dupl[duplicated(all_burden_dupl), ]
  expect_equal(nrow(all_burden_dupl), 0)

  attrBurden_dupl <- attrBurden %>% select(setdiff(colnames(attrBurden), c("lower", "mean", "upper")))
  attrBurden_dupl <- attrBurden_dupl[duplicated(attrBurden_dupl), ]
  expect_equal(nrow(attrBurden_dupl), 0)
})

#--write---
# attrBurden<- attrBurden %>% ungroup %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, measure3, Region, scenario, mean, lower, upper, method)
# all_burden<- all_burden %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, Region, overall_value)

measure3_all <- attrBurden$measure3 %>% unique()
for (i in seq_along(measure3_all)) {
  fwrite(
    attrBurden %>% filter(measure3 == measure3_all[[i]]),
    file.path(summaryDir, paste0("attr_burd_", i, ".csv"))
  )
}
# fwrite(attrBurden %>% filter(measure3 %in% c("value", "prop. of overall burden")), file.path(summaryDir, "attr_burd.csv"))
# fwrite(attrBurden %>% filter(!measure3 %in% c("value", "prop. of overall burden")), file.path(summaryDir, "attr_burd_prop.csv"))
fwrite(all_burden, file.path(summaryDir, "all_burd.csv"))
toc()
