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
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
agr_by <- args[10]
tmpDir <- args[3]
censDir <- args[8]
pop.summary.dir <- args[16]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2009
  agr_by <- "nation"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
}

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv()
plotDir <- file.path(pop.summary.dir, "plot", agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)
# load meta data
census_meta <- file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")) %>% fread()

pop.summary.dir <- file.path(pop.summary.dir, agr_by)
dir.create(pop.summary.dir, recursive = T, showWarnings = F)
pop.summary.dirX <- file.path(pop.summary.dir, paste0("pop_sum_", year, ".csv"))

if (!file.exists(pop.summary.dirX)) {
  # loop over all states
  tic(paste("summarized population data in", year, "by", agr_by))
  pop.summary <- apply(states, 1, function(state) {
    STUSPS <- state[["STUSPS"]]
    name <- state[["NAME"]]

    # read demographic census data by tract
    pop.summary <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% fread()

    pop.summary <- pop.summary %>%
      group_by(state, variable) %>%
      summarize(Population = sum(pop_size))
  }) %>% rbindlist()

  pop.summary <- states %>%
    right_join(pop.summary, by = c("STATEFP" = "state")) %>%
    dplyr::group_by_at(vars(one_of(c(agr_by, "variable")))) %>%
    summarize(Population = sum(Population))

  pop.summary <- pop.summary %>%
    left_join(census_meta, by = "variable") %>%
    select(-c(age_group_id))
  write.csv(pop.summary, pop.summary.dirX, row.names = FALSE)
  toc()
}

if (TRUE) {
  tic("plotted population summary")
  files <- list.files(pop.summary.dir)
  pop.summary <- lapply(files, function(file) {
    fread(file.path(pop.summary.dir, file))
  }) %>%
    #rbindlist() %>%
    do.call(rbind,.) %>%
    as.data.frame()

  for (location in pop.summary[, agr_by] %>% unique()) {
    pop.summary_sub <- pop.summary %>% filter(get(agr_by) == location)
    ## --- by race/ethnicity---
    pop.summary_sub1 <- pop.summary_sub %>%
      mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
      filter(Ethnicity %in% c(
        "White, Not Hispanic or Latino",
        "White, Hispanic or Latino",
        "Black or African American, All Origins",
        "Asian or Pacific Islander, All Origins",
        "American Indian or Alaska Native, All Origins"
      ) &
        Gender.Code == "A" &
        Education == 666)

    pop.summary_sub1 <- pop.summary_sub1 %>%
      group_by(Ethnicity, Year) %>%
      summarize(Population = sum(Population))

    g <- ggplot(pop.summary_sub1, aes(x = Year, y = Population)) +
      geom_line(aes(color = Ethnicity), size = 1) +
      ylab(paste("Population")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("Population in", location))

    ggsave(file.path(plotDir, paste0(location, "_plot_race_cens.png")), plot = g)

    ## --- by education---
    pop.summary_sub2 <- pop.summary_sub %>% filter(Education != 666 &
                                                     Gender.Code == "A")

    pop.summary_sub2 <- pop.summary_sub2 %>%
      group_by(Education, Year) %>%
      summarize(Population = sum(Population)) %>%
      arrange(Year, Education)
    
    replaces3 <- data.frame(
      Education = c(1:7, 666),
      Education2 = c(
        "Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative",
        "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"
      )
    )
    pop.summary_sub2 <- pop.summary_sub2 %>% left_join(replaces3, by = "Education")

    g <- ggplot(pop.summary_sub2, aes(x = Year, y = Population, color = Education2)) +
      geom_line( size = 1) +
      ylab(paste("Population")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 4, byrow = TRUE)) +
      ggtitle(paste("Population 25+ in", location))

    ggsave(file.path(plotDir, paste0(location, "_plot_educ_cens.png")), plot = g)
    
  }
  toc()
}
