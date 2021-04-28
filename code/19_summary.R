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
    attrBurden <- lapply(files, function(file) fread(file.path(attrBurdenDir, agr_by, source, file))) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

  # make compatible
  attrBurden <- attrBurden %>% rename("Region" := !!agr_by)
  attrBurden <- attrBurden %>% tibble::add_column(agr_by = agr_by)
  return(attrBurden)
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

## --- read all burden----
agr_bys <- list.files(totalBurdenParsed2Dir)
all_burden <- lapply(agr_bys, function(agr_by) {
  sources <- list.files(file.path(totalBurdenParsed2Dir, agr_by))
  all_burden <- lapply(sources, function(source) {
    files <- list.files(file.path(totalBurdenParsed2Dir, agr_by, source))
    all_burden <- lapply(files, function(file) fread(file.path(totalBurdenParsed2Dir, agr_by, source, file))) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)

  # make compatible
  all_burden <- all_burden %>% rename("Region" := !!agr_by)
  all_burden <- all_burden %>% tibble::add_column(agr_by = agr_by)
  return(all_burden)
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

group_variables <- setdiff(colnames(attrBurden), c("lower", "mean", "upper", "method"))

all_burden <- all_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(overall_value = sum(value))
### ----- add proportion ---

# add "prop. of overall burden", "prop. of total burden"
# join everything
attrBurden_prop <- attrBurden %>% left_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))

# calculations
attrBurden_prop <- attrBurden_prop %>%
  mutate(
    mean = 100 * mean / overall_value,
    lower = 100 * lower / overall_value,
    upper = 100 * upper / overall_value,
    measure3 = sapply(attr.y, function(att) ifelse(att == "overall", "prop. of overall burden", "prop. of total burden")),
    overall_value = NULL, attr.x = NULL, attr.y = NULL,
    attr = "attributable"
  )

test_that(" basic chackes", {
  test1 <- attrBurden %>% anti_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))

  test <- attrBurden[rowSums(is.na(attrBurden)) > 0, ]
  expect_false(any(is.na(attrBurden)))
  expect_false(any(is.na(attrBurden_prop)))
  expect_false(any(is.na(all_burden)))
  # TODO
})

# add proportion of disparity
attrBurden_disp1 <- inner_join(all_burden %>% filter(attr == "overall" & !(Race == "White" & Hispanic.Origin == "Not Hispanic or Latino")),
  all_burden %>% filter(attr == "overall" & (Race == "White" & Hispanic.Origin == "Not Hispanic or Latino")),
  by = c("Year", "Gender.Code", "Education", "Region", "measure1", "measure2", "attr", "source", "agr_by")
)

attrBurden_disp2 <- inner_join(attrBurden %>% filter(!(Race == "White" & Hispanic.Origin == "Not Hispanic or Latino")),
  attrBurden %>% filter((Race == "White" & Hispanic.Origin == "Not Hispanic or Latino")),
  by = c("Year", "Gender.Code", "Education", "Region", "measure1", "measure2", "attr", "source", "agr_by", "method")
)

attrBurden_disp3 <- inner_join(attrBurden_disp1, attrBurden_disp2, by = c("Race.x", "Hispanic.Origin.x", "Race.y", "Hispanic.Origin.y", "Year", "Gender.Code", "Education", "Region", "measure1", "measure2", "source", "agr_by"))
attrBurden_disp3 <- attrBurden_disp3 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to White, Not Hispanic attributable",#"prop. of disp.",#
  Race = Race.x, Hispanic.Origin = Hispanic.Origin.x,
  Race.x = NULL, Race.y = NULL, Hispanic.Origin.x = NULL, Hispanic.Origin.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

attrBurden_disp4 <- inner_join(all_burden %>% filter(attr == "overall" & Education != 7),
  all_burden %>% filter(attr == "overall" & Education == 7),
  by = c("Year", "Gender.Code", "Race", "Hispanic.Origin", "Region", "measure1", "measure2", "attr", "source", "agr_by")
)

attrBurden_disp5 <- inner_join(attrBurden %>% filter(Education != 7), 
  attrBurden %>% filter(Education == 7), 
  by = c("Year", "Gender.Code", "Race", "Hispanic.Origin", "Region", "measure1", "measure2", "attr", "source", "agr_by", "method")
)

attrBurden_disp6 <- inner_join(attrBurden_disp4, attrBurden_disp5, by = c("Education.x", "Education.y", "Race", "Hispanic.Origin","Year", "Gender.Code", "Region", "measure1", "measure2", "source", "agr_by"))
attrBurden_disp6 <- attrBurden_disp6 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to Graduate or professional degree attributable",#"prop. of disp.",#
  Education = Education.x, 
  Education.x = NULL, Education.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

attrBurden$measure3 <- "value"
attrBurden <- rbind(attrBurden, attrBurden_prop, attrBurden_disp3, attrBurden_disp6)
rm(attrBurden_prop, attrBurden_disp1, attrBurden_disp2, attrBurden_disp3, attrBurden_disp4, attrBurden_disp5, attrBurden_disp6)
## --- sort ----
all_burden <- rbind(
  all_burden %>% filter(Region == "us"),
  all_burden %>%
    filter(Region != "us") %>%
    arrange(Region)
)
## --Find replace----
rindreplace1 <- setNames(c(states$NAME, "United States"), c(states$STATEFP, "us"))
all_burden$Region <- sapply(all_burden$Region, function(x) rindreplace1[[x]])
attrBurden$Region <- sapply(attrBurden$Region, function(x) rindreplace1[[x]])

rindreplace2 <- setNames(
  c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"),
  c(1:7, 666)
)
all_burden$Education <- sapply(all_burden$Education %>% as.character(), function(x) rindreplace2[[x]])
attrBurden$Education <- sapply(attrBurden$Education %>% as.character(), function(x) rindreplace2[[x]])

rindreplace3 <- setNames(c("All genders", "Male", "Female"), c("A", "M", "F"))
all_burden$Gender.Code <- sapply(all_burden$Gender.Code, function(x) rindreplace3[[x]])
attrBurden$Gender.Code <- sapply(attrBurden$Gender.Code, function(x) rindreplace3[[x]])

rindreplace4 <- setNames(c("National Vital Statistics System", "Mortality Data from CDC WONDER"), c("nvss", "wonder"))
all_burden$source <- sapply(all_burden$source, function(x) rindreplace4[[x]])
attrBurden$source <- sapply(attrBurden$source, function(x) rindreplace4[[x]])

 rindreplace5 <- setNames(c("Years of Life Lost (YLL)", "Deaths"), c("YLL","Deaths"))
# all_burden$measure1 <- sapply(all_burden$measure1 , function(x) rindreplace5[[x]])
# attrBurden$measure1 <- sapply(attrBurden$measure1 , function(x) rindreplace5[[x]])

rindreplace6 <- setNames(c("crude rate per 100,000", "age-adjusted rate per 100,000", "absolute number"), c("crude rate", "age-adjusted rate", "absolute number"))
all_burden$measure2 <- sapply(all_burden$measure2, function(x) rindreplace6[[x]])
attrBurden$measure2 <- sapply(attrBurden$measure2, function(x) rindreplace6[[x]])
# Years of Life lost;

rm(rindreplace1, rindreplace2, rindreplace3, rindreplace4, rindreplace5, rindreplace6)
all_burden <- all_burden %>% mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin))
all_burden$Hispanic.Origin <- NULL
all_burden$Race <- NULL

attrBurden <- attrBurden %>% mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin))
attrBurden$Hispanic.Origin <- NULL
attrBurden$Race <- NULL

#--write---
fwrite(attrBurden %>% filter(measure3 %in% c("value", "prop. of overall burden")), file.path(summaryDir, "attr_burd.csv"))
fwrite(attrBurden %>% filter(!measure3 %in% c("value", "prop. of overall burden")), file.path(summaryDir, "attr_burd_prop.csv"))
fwrite(all_burden, file.path(summaryDir, "all_burd.csv"))
toc()
