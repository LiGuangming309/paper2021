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

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
censDir <- args[3]
attrBurdenDir <- args[4]
summaryDir <- args[5]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/10_plots"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
summaryDir <- file.path(summaryDir, agr_by)
dir.create(summaryDir, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()

group_variables <- c(
  "Year" = "year",
  # "Gender" = "gender",
  # "Gender.Code" = "gender_label",
  "Race" = "race",
  "Hispanic.Origin" = "hispanic_origin"
)
inverse_group_variables <- setNames(names(group_variables), group_variables)

if (!file.exists(file.path(summaryDir, "attr_burd.csv"))) {
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
    as.data.frame() %>%
    mutate(effPaf = attrDeaths / Deaths)
  ## --- read demographic census data ----
  tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", ")))
  censData_agr <- lapply(unique(attrBurden$Year), function(year) {
    tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", "), "in", year))
    
    censData_agr <- apply(states, 1, function(state) {
      STUSPS <- state["STUSPS"]
      name <- state["NAME"]
      
      censData_agrDir<-file.path(censDir, year, paste0("agr_census_", toString(year), "_", STUSPS, ".csv"))
      if(!file.exists(censData_agrDir)){
        tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", "), "in", year, "in", name))
        censData <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% read.csv
        
        censData_agr <- censData %>%
          group_by(variable) %>%
          summarise(pop_size = sum(pop_size))
        
        fwrite(censData_agr,censData_agrDir)
        toc()
      }
      censData_agr <-fread(censData_agrDir)
      return(censData_agr)
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame
    
    meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
    censData_agr<- censData_agr %>%
      left_join(meta, by = "variable")
    
    toc()
    return(censData_agr)
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
      crudeDeaths = Deaths * 100000 / pop_size,
      crudeYLL = YLL * 100000 / pop_size,
      crudeAttrDeaths = attrDeaths * 100000 / pop_size, # Crude Rate Per 100,000
      crudeAttrYLL = attrYLL * 100000 / pop_size,
    )

  test_that("10 plot basic chackes", {
    expect_false(any(is.na(attrBurden_gr)))
  })

  fwrite(attrBurden_gr, file.path(summaryDir, "attr_burd.csv"))
}
attrBurden_gr <- fread(file.path(summaryDir, "attr_burd.csv"))

## ---plot ------
for (his_or in unique(attrBurden_gr$Hispanic.Origin)) {
  attrBurden_gr_his <- attrBurden_gr %>% filter(Hispanic.Origin == his_or)

  for (measure in c("Deaths", "YLL", "attrDeaths", "attrYLL","effPaf","pop_size","crudeDeaths",
                    "crudeYLL","crudeAttrDeaths","crudeAttrYLL")) {
    g <- attrBurden_gr_his %>%
      ggplot(aes_string(x = "Year", y = measure, group = "Race", color = "Race")) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle(paste("hispanic origin:", his_or)) +
      theme_ipsum() +
      ylab(paste("burden measured in", measure)) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      geom_line()

    ggsave(file.path(summaryDir, paste0(measure, "_", his_or, ".png")),
      plot = g
    )
  }
}
