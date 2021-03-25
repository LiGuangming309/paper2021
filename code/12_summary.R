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
censDir <- args[3]
attrBurdenDir <- args[4]
allBurdenDir <- args[5]
summaryDir <- args[6]
dataDir <- args[8]
cdcPopDir <- args[14]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  dataDir <- "/Users/default/Desktop/paper2021/data"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/10_attr_burd"
  allBurdenDir <- "/Users/default/Desktop/paper2021/data/11_all_burden"
  summaryDir <- "/Users/default/Desktop/paper2021/data/12_summary"
  cdcPopDir <- "/Users/default/Desktop/paper2021/data/14_cdc_population"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
allBurdenDir <- file.path(allBurdenDir, agr_by)
summaryDir <- file.path(summaryDir, agr_by)
dir.create(summaryDir, recursive = T, showWarnings = F)
cdcPopDir <- file.path(cdcPopDir, agr_by)

lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
states <- file.path(tmpDir, "states.csv") %>% read.csv()

group_variables <- c(
  "Year" = "year",
  # "Gender" = "gender",
  # "Gender.Code" = "gender_label",
  "Race" = "race",
  "Hispanic.Origin" = "hispanic_origin"
)

 agr_by_replace <- c("county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
                    "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county"= "County.Code")
 agr_by_new <- agr_by_replace[[agr_by]]
 group_variables[agr_by_new] <- agr_by

inverse_group_variables <- setNames(names(group_variables), group_variables)

if (!file.exists(file.path(summaryDir, "attr_burd.csv"))) {
  ### --------read attributable burden data----------
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
  
  columns <- c(inverse_group_variables, "measure", "attr")
  attrBurden_gr <- attrBurden %>%
    group_by_at(vars(one_of(columns))) %>%
    summarise(
      value = sum(value)
    ) %>%
    as.data.frame()
  
  ### ---- read cdc population data------
  files <- list.files(cdcPopDir)
  cdc_pop <- lapply(files, function(file) {
    fileDir <- file.path(cdcPopDir, file)
    cdc_pop <- read.delim(fileDir)

    notes_hisp_or <- cdc_pop$Notes[grepl("Hispanic Origin:", cdc_pop$Notes, fixed = TRUE)]

    cdc_pop <- cdc_pop[!apply(is.na(cdc_pop) | cdc_pop == "", 1, all), ]

    if (!"Hispanic.Origin" %in% colnames(cdc_pop)) {
      if (rlang::is_empty(notes_hisp_or)) {
        cdc_pop[, "Hispanic.Origin"] <- "All Origins"
      } else if (grepl("Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        cdc_pop[, "Hispanic.Origin"] <- "Hispanic or Latino"
      } else if (grepl("Not Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        cdc_pop[, "Hispanic.Origin"] <- "Not Hispanic or Latino"
      }
    }

    cdc_pop <- cdc_pop %>% select(Race, Year, Hispanic.Origin, Population) # Gender
    if (agr_by == "nation") {
      cdc_pop[, "Nation"] <- "us"
    }
    return(cdc_pop)
  })

  cdc_pop <- cdc_pop %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    filter(
      Hispanic.Origin != "Not Stated"
    )

  cdc_pop <- cdc_pop %>%
    group_by_at(vars(one_of(inverse_group_variables))) %>%
    summarise(Population = sum(Population))
  ## ---------------- join/write --------------------
  # join everything
  all_burden <- attrBurden_gr %>% 
    filter(attr == "overall") %>%
    rename(overall_value = value) %>% 
    subset(select = -attr)
  
  attrBurden_gr <- attrBurden_gr %>%
    left_join(all_burden, by = unname(c(inverse_group_variables, "measure"))) %>%
    left_join(cdc_pop, by = unname(inverse_group_variables))

  # calculations
  attrBurden_gr <- attrBurden_gr %>%
    mutate(
      # Crude Rates Per 100,000
      crude_rate = value * 100000 / Population,
      # proportions
      prop = 100* value / overall_value,
    )

  test_that("10 plot basic chackes", {
    expect_false(any(is.na(attrBurden_gr)))
  })

  fwrite(attrBurden_gr, file.path(summaryDir, "attr_burd.csv"))
}
attrBurden_gr <- fread(file.path(summaryDir, "attr_burd.csv"))

## ---plot ------

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
#TODO population
for(location in attrBurden_gr_sub[, get(agr_by_new)] %>% unique){
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
  
  ggsave(file.path(summaryDir, paste0("plot",i, ".png")),
         plot = g
  )
  i <- i+1
}

for(measure2 in attrBurden_gr_sub$measure %>% unique){
  for(location in attrBurden_gr_sub[, get(agr_by_new)] %>% unique){
    for(attr2 in attrBurden_gr_sub$attr %>% unique){
      for(column in c("value", "crude_rate", "prop")){
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
        
        ggsave(file.path(summaryDir, paste0("plot",i, ".png")),
               plot = g
        )
        i <- i+1
      }
    }
  }
}

