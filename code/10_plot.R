#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
censDir <- args[3]
attrBurdenDir <- args[4]
plotsDir <- args[5]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
  plotsDir <- "/Users/default/Desktop/paper2021/data/10_plots"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
plotsDir <- file.path(plotsDir, agr_by)
dir.create(plotsDir, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()

group_variables <- c(
  "Year" = "year",
  # "Gender" = "gender",
  # "Gender.Code" = "gender_label",
  "Race" = "race",
  "Hispanic.Origin" = "hispanic_origin"
)
inverse_group_variables <- setNames(names(group_variables), group_variables)
### ---read attributable burden data-----
files <- list.files(attrBurdenDir)

attrBurden <- lapply(files, function(file) {
  attrBurden <- file.path(attrBurdenDir, file) %>% read.csv()
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

missing <- setdiff(2000:2016, attrBurden$Year)
if (length(missing) > 0) {
  print("Years missing in attributable burden data:")
  print(missing)
}

attrBurden_gr <- attrBurden %>%
  group_by_at(vars(one_of(inverse_group_variables))) %>%
  summarise(
    Deaths = sum(Deaths),
    YLL = sum(YLL),
    attrDeaths = sum(attrDeaths),
    attrYLL = sum(attrYLL)
  ) %>%
  as.data.frame %>%
  mutate(effPaf = attrDeaths /Deaths)
## --- read demographic census data ----
tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ', ')))
censData <- lapply(unique(attrBurden$Year), function(year) {
  tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ', '), "in", year))

  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  censData <- apply(states, 1, function(state) {
    STUSPS <- state["STUSPS"]
    name <- state["NAME"]
    tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ', '), "in", year, "in", name))
    censData <- file.path(censDir, year, paste0("census_2010_", STUSPS, ".csv")) %>% read.csv()

    censData <- censData %>%
      left_join(meta, by = "variable") %>%
      group_by_at(vars(one_of(group_variables))) %>%
      summarise(pop_size = sum(pop_size))

    toc()
    return(censData)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()

  toc()
  return(censData)
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

censData <- censData %>%
  group_by_at(vars(one_of(group_variables))) %>%
  summarise(pop_size = sum(pop_size))
toc()
## --- join/write -----

attrBurden_gr <- left_join(attrBurden_gr, censData, by = group_variables) %>%
  mutate(
    crudeDeaths = Deaths *100000/pop_size,
    crudeYLL = YLL *100000/pop_size,
    crudeAttrDeaths = attrDeaths *100000/pop_size, #Crude Rate Per 100,000
    crudeAttrYLL = attrYLL *100000/pop_size,
  )

test_that("10 plot basic chackes",{
  expect_false(any(is.na(attrBurden_gr)))
  #TODO
})

fwrite(attrBurden_gr, file.path(plotsDir, "attr_burd.csv"))

## ---plot ------
for (his_or in unique(attrBurden_gr$Hispanic.Origin)) {
  attrBurden_gr_his <- attrBurden_gr %>% filter(Hispanic.Origin == his_or)

  for (measure in c("Deaths", "YLL", "attrDeaths", "attrYLL")) {
    g <- attrBurden_gr_his %>%
      ggplot(aes(x = Year, y = measure, group = Race, color = Race)) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle(paste("hispanic origin:", his_or)) +
      theme_ipsum() +
      ylab(paste("burden measured in", measure)) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      geom_line()

    ggsave(file.path(plotsDir, paste0(measure, "_", his_or, ".png")),
      plot = g
    )
  }
}
