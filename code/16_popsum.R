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

# Pass in arguments
args <- commandArgs(trailingOnly = T)

agr_by <- args[2]
cdcPopDir <- args[6]
pop.summary.dir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  cdcPopDir <- "/Users/default/Desktop/paper2021/data/11_cdc_population"
  pop.summary.dir <- "/Users/default/Desktop/paper2021/data/12_population_summary"
}
# https://wonder.cdc.gov/controller/saved/D134/D140F476
cdcPopDir <- file.path(cdcPopDir, agr_by)
pop.summary.dir <- file.path(pop.summary.dir, paste0("pop_",agr_by,".csv"))

agr_by_replace <- c(
  "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
  "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county" = "County.Code"
)
agr_by_new <- agr_by_replace[agr_by]
agr_by_new <- setNames(names(agr_by_new), agr_by_new)

if (!file.exists(pop.summary.dir)) {
  #-----read -----
  files <- list.files(cdcPopDir)
  cdc_pop <- lapply(files, function(file) {
    fileDir <- file.path(cdcPopDir, file)
    cdc_pop <- read.delim(fileDir) %>% filter(Notes == "")

    if (!("Ethnicity" %in% colnames(cdc_pop))) cdc_pop$Ethnicity <- "All Origins"
    if (agr_by == "nation") cdc_pop$nation <- "us"

    cdc_pop <- cdc_pop %>%
      select(all_of(c(
        "min_age" = "Age.Code",
        "max_age" = "Age.Code",
        "Year" = "Yearly.July.1st.Estimates.Code",
        "Hispanic.Origin" = "Ethnicity",
        "Race" = "Race",
        "Population" = "Population",
        agr_by_new
      )))

    return(cdc_pop)
  })

  cdc_pop <- cdc_pop %>%
    do.call(rbind, .) %>%
    as.data.frame()


  cdc_pop <- cdc_pop %>% tibble::add_column(Education = 666)
  cdc_pop$min_age[cdc_pop$min_age == "85+"] <- 85
  cdc_pop$max_age[cdc_pop$max_age == "85+"] <- 150
  cdc_pop$min_age <- as.numeric(cdc_pop$min_age)
  cdc_pop$max_age <- as.numeric(cdc_pop$max_age)
  write.csv(cdc_pop, pop.summary.dir)
}
