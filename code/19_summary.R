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
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
totalBurdenParsed2Dir <- args[4]
attrBurdenDir <- args[5]
summaryDir <- args[6]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
}

totalBurdenParsed2Dir<- file.path(totalBurdenParsed2Dir, agr_by)
attrBurdenDir <- file.path(attrBurdenDir, agr_by)
summaryDir <- file.path(summaryDir, agr_by)
dir.create(summaryDir, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()

group_variables <- c("Year","Race","Hispanic.Origin","Education", "Gender.Code", agr_by)

if (!file.exists(file.path(summaryDir, "attr_burd.csv"))) {
  ### --------read attributable burden data----------
  sources <- c("wonder","nvss")
  attrBurden<-lapply(sources, function(source){
    attrBurdenDir <- file.path(attrBurdenDir, source)
    if(!file.exists(attrBurdenDir)) return(NA)
    files <- list.files(attrBurdenDir)
    attrBurden <- lapply(files, function(file) {
      attrBurden <- file.path(attrBurdenDir, file) %>% read.csv()
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  

  missing <- setdiff(2000:2016, attrBurden$Year)
  if (length(missing) > 0) {
    print("Years missing in attributable burden data:")
    print(missing)
  }
  rm(missing)
  
  ## --- read overall burden ---
  #TODO
  #all_burden <- file.path(totalBurdenParsedDir, agr_by, "total_burden.csv") %>%
  #  read.csv() %>%
  #  filter(attr == "overall") %>%
  #  group_by_at(vars(one_of(c(group_variables, "measure")))) %>%
  #  summarise(overall_value = sum(value))
  all_burden<-lapply(sources, function(source){
    totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir, source)
    if(!file.exists(totalBurdenParsed2Dir)) return(NA)
    files <- list.files(totalBurdenParsed2Dir)
    all_burden <- lapply(files, function(file) {
      all_burden <- file.path(totalBurdenParsed2Dir, file) %>% read.csv()
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  all_burden <- all_burden  %>%
    filter(attr == "overall") %>%
    group_by_at(vars(all_of(c(group_variables, "source","measure1","measure2")))) %>%
    summarise(overall_value = sum(value))
  
  ## ---------------- join/write --------------------
  # join everything
  attrBurden_prop <- attrBurden %>% left_join(all_burden, by = setdiff(colnames(all_burden),"overall_value")) 

  # calculations
  attrBurden_prop <- attrBurden_prop %>%
    mutate(
      mean = 100*mean/overall_value, 
      lower = 100*lower/overall_value,
      upper = 100* upper/overall_value,
      overall_value = NULL,
      measure2 = "prop. of overall burden"
    )

  attrBurden <- rbind(attrBurden, attrBurden_prop)
  test_that("10 plot basic chackes", {
    expect_false(any(is.na(attrBurden)))
    expect_false(any(is.na(all_burden)))
  })
  rm(attrBurden_prop)

  fwrite(attrBurden, file.path(summaryDir, "attr_burd.csv"))
  fwrite(all_burden, file.path(summaryDir, "all_burd.csv"))
}

## ---plot ------
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
attrBurden <- fread(file.path(summaryDir, "attr_burd.csv"))
if (FALSE) {
  
  attrBurden_gr_sub <- attrBurden_gr %>%
    mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
    filter(Ethnicity %in% c(
      "White, Not Hispanic or Latino",
      "White, Hispanic or Latino",
      "Black or African American, All Origins",
      "Asian or Pacific Islander, All Origins",
      "American Indian or Alaska Native, All Origins"
    ))

  i <- 1
  # TODO population
  for (location in attrBurden_gr_sub[, get(agr_by_new)] %>% unique()) {
    attrBurden_gr_sub2 <- attrBurden_gr_sub %>%
      filter(measure == "Deaths" &
        attr == "total")

    g <- attrBurden_gr_sub2 %>%
      ggplot(aes_string(x = "Year", y = "Population", color = "Ethnicity")) +
      # scale_color_viridis(discrete = TRUE) +
      # theme_ipsum() +
      ylab(paste("Population")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      geom_line() +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("Population", location, sep = ", "))

    ggsave(file.path(summaryDir, paste0("plot", i, ".png")),
      plot = g
    )
    i <- i + 1
  }

  for (measure2 in attrBurden_gr_sub$measure %>% unique()) {
    for (location in attrBurden_gr_sub[, get(agr_by_new)] %>% unique()) {
      for (attr2 in attrBurden_gr_sub$attr %>% unique()) {
        for (column in c("crude_rate", "prop")) { # TODO mean, lower, upper
          attrBurden_gr_sub2 <- attrBurden_gr_sub %>%
            filter(measure == measure2 &
              attr == attr2 &
              get(agr_by_new) == location)

          g <- attrBurden_gr_sub2 %>%
            ggplot(aes_string(x = "Year", y = column, color = "Ethnicity")) +
            # scale_color_viridis(discrete = TRUE) +
            # theme_ipsum() +
            ylab(paste("burden measured in", measure2, column)) +
            xlab("Year") +
            ylim(0, NA) +
            xlim(2000, 2016) +
            geom_line() +
            theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
            guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
            ggtitle(paste(measure2, location, attr2, column, sep = ", "))

          ggsave(file.path(summaryDir, paste0("plot", i, ".png")),
            plot = g
          )
          i <- i + 1
        }
      }
    }
  }
}
