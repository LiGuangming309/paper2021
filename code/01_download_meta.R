
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tigris", "tictoc", "cdcfluview", "testthat", "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

# TODO l?schen
#year <- 2010

# censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
# tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"

#tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
#censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

# quits, if not downloadable year
if (!year %in% c(2000:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

## ----------download useful data to tmp-------------------------------------------------------------------------------
# download states data
filepathStates <- file.path(tmpDir, "states.csv")
if (!file.exists(filepathStates)) {
  # only contiguous US
  # excluding Alaska, Hawaii, American Samoa, Guam, Commonwealth of the Northern Mariana Islands, Puerto Rico, United States Virgin Islands
  states1 <- states() %>%
    as.data.frame() %>%
    select(c(REGION, DIVISION, STATEFP, STUSPS, NAME)) %>%
    filter(!(STUSPS %in% c("AK", "HI", "AS", "GU", "MP", "PR", "VI"))) %>%
    arrange(STATEFP) %>%
    # rename it can be merged later
    setnames(
      c("REGION", "DIVISION"),
      c("Census_Region", "Census_division")
    )

  data(hhs_regions)

  states2 <- hhs_regions %>%
    as.data.frame() %>%
    select(c(region_number, state_or_territory)) %>%
    setnames(
      c("state_or_territory", "region_number"),
      c("NAME", "hhs_region_number")
    )

  # merge for full information
  states <- merge(states1, states2) %>%
    mutate(nation = "us")

  write.csv(states, filepathStates, row.names = FALSE)
} else {
  states <- read.csv(filepathStates)
}
rm(filepathStates)

### ------------------------download demographic data-----------------------------------------------------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)


## ----- download census metadata------
censMetaDir <- file.path(censDir, "meta")
dir.create(censMetaDir, recursive = T, showWarnings = F)

filepathCensMeta <- paste0("cens_meta_", toString(year), ".csv") %>%
  file.path(censMetaDir, .)

# relevant groups for each year and table names
if (year %in% 2000:2009) {
  # decennical census, sex by age for races
  groups <- c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I", "PCT012J", "PCT012K", "PCT012L", "PCT012M")
  tablename <- "dec/sf1"
} else if (year == 2010) {
  # decennical census, sex by age for races
  groups <- c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M")
  tablename <- "dec/sf1"
} else if (year %in% 2011:2016) {
  # american community survey, sex by age for races
  groups <- c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H")
  # "not hispanic or latino" only available for white
  tablename <- "acs/acs5"
}

# download meta data, if necessary
if (!file.exists(filepathCensMeta)) {
  tic(paste("Downloaded census meta data for year", toString(year)))
  # loop over all relevant groups
  census_meta <- lapply(groups, function(group) {
    listCensusMetadata(
      name = tablename,
      vintage = ifelse(year %in% 2001:2009,
        2000,
        year
      ),
      type = "variables",
      group = group
    ) %>%
      select("name", "label", "concept") %>% # select relevant columns
      mutate(
        year = year,
        group = group,

        ## parse "label", "concept" from String to seperate columns
        label = strsplit(label, "!!"),

        datatype = sapply(label, function(l) {
          # the acs includes estimates and annotation of estimates
          ifelse(tablename == "acs/acs5",
            l[[1]],
            "Estimate"
          )
        }),

        label = lapply(label, function(l) {
          # making acs label notation coherant with dec cens notation
          if (tablename == "acs/acs5") {
            return(l[-1])
          } else {
            return(l)
          }
        }),

        label_len = sapply(label, length),
        # extracts gender
        gender = label %>% sapply(function(l) l[2]),
        gender_label = gender %>% sapply(function(g) ifelse(g == "Female", "F", "M")),
        age = label %>% sapply(function(l) l[3]),
        min_age = age %>% sapply(function(a) {
          # age includes "under", min_age = 0
          if (grepl("Under", a)) {
            return(0)
          }
          # else extract first number in String
          str_extract(a, "[:digit:]+") %>%
            as.numeric()
        }),

        max_age = age %>% sapply(function(a) {
          # if includes "and over", max_age = 150, since humans do not get older
          if (grepl("and over", a)) {
            return(150)
          }
          # otherwise extracts last number in String
          last_num <- str_extract_all(a, "[:digit:]+") %>%
            unlist() %>%
            tail(1) %>%
            as.numeric()
          # if includes "under", max_age = last_num -1
          if (grepl("Under", a)) {
            return(last_num - 1)
          } else { # else just last_num
            return(last_num)
          }
        }),

        age = NULL,
        label = NULL,
        race_his = concept %>% sapply(function(conc) {
          # extract the information in the brackets
          # e.g. sex by age (White alone, not hispanic or latino) => White alone, not hispanic or latino
          regmatches(conc, gregexpr("(?<=\\().*?(?=\\))", conc, perl = T))[[1]]
        }),
        concept = NULL,
        # extract race
        race = race_his %>% sapply(function(race_his) {
          race_his %>%
            strsplit(., ",") %>%
            unlist() %>%
            extract2(1) %>%
            # fetch what comes before ","; e.g.: White alone, not hispanic or latino => White alone
            substr(., 1, nchar(.) - 6)
          # removes " alone", e.g. "White alone" => "White"
        }),
        # extracts hispanic origin. option: not Hispanic or latino, Hispanic or latino, all
        hispanic_origin = race_his %>% sapply(function(race_his) {
          a <- race_his %>%
            strsplit(., ",") %>%
            unlist()
          # if no comma, "all"
          ifelse(length(a) <= 1,
            "all",
            a[2] %>% substring(., 2)
          )
          # extract what comes after ","; e.g.: White alone, not hispanic or latino => not hispanic or latino
        }),
        race_his = NULL
      )
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    filter(
      label_len == 3, # filters granular data with gender and age group
      datatype == "Estimate" # filters Estimates, excluding Annotations and Margins of Error
    ) %>%
    mutate(
      label_len = NULL,
      datatype = NULL
    )

  setnames(census_meta, "name", "variable") # rename for later purpose

  ## add corresponding age_group_id from causes ages
  census_meta <- census_meta %>%
    mutate(
      age_group_id = seq(25, 95, 5)[
        findInterval(
          max_age,
          seq(25, 90, 5),
          left.open =  TRUE
        ) + 1
      ]
    )

  # drop unrequired information
  fwrite(census_meta, filepathCensMeta)
  census_meta <- read.csv(filepathCensMeta)

  # add some useful columns to calculate complement of "NOT HISPANIC OR LATINO" in "all",
  # and create race "Asian or Pacific islander"
  census_meta <- census_meta %>%
    mutate(
      downloaded = TRUE,
      relevant = TRUE,
      tot_var = NA,
      ntot_var = NA
    )

  # find pairs "all", "NOT HISPANIC OR LATINO"
  census_meta_all <- census_meta %>% filter(hispanic_origin == "all")
  census_meta_nhis <- census_meta %>% filter(hispanic_origin == "NOT HISPANIC OR LATINO")

  for (i in 1:nrow(census_meta_all)) {
    # for each row with hisp. or. "all", find all corresponding rows in "NOT HISPANIC OR LATINO"
    # with same age group, year, gender, race
    row <- census_meta_all[i, ]
    census_meta_nhis_sub <- census_meta_nhis %>%
      filter(
        year == row[["year"]],
        gender_label == row[["gender_label"]],
        race == row[["race"]],
        min_age >= row[["min_age"]],
        max_age <= row[["max_age"]]
      )

    # if truly corresponds, add row
    if (nrow(census_meta_nhis_sub) > 0 &&
      row[["min_age"]] == (census_meta_nhis_sub$min_age %>% min()) &&
      row[["max_age"]] == (census_meta_nhis_sub$max_age %>% max())) {
      row_copy <- row %>% mutate(
        tot_var = variable,
        downloaded = FALSE,
        relevant = TRUE,
        variable = paste0(variable, "C"),
        hispanic_origin = "HISPANIC OR LATINO"
      )
      row_copy$ntot_var[1] <- census_meta_nhis_sub$variable %>% list()

      census_meta <- rbind(census_meta, row_copy)
    }
  }

  # find pairs asian, pacific islander
  # TODO
  census_meta <- census_meta %>%
    mutate(relevant = ifelse(race %in% c("ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER"),
      FALSE,
      relevant
    ))
  census_meta_asian <- census_meta %>% filter(race == "ASIAN")
  census_meta_pac <- census_meta %>% filter(race == "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER")

  for (i in 1:nrow(census_meta_asian)) {
    row <- census_meta_asian[i, ]
    census_meta_pac_sub <- census_meta_pac %>%
      filter(
        year == row[["year"]],
        gender_label == row[["gender_label"]],
        min_age == row[["min_age"]],
        max_age == row[["max_age"]],
        hispanic_origin == row[["hispanic_origin"]]
      )

    # if truly corresponds, add row
    if (nrow(census_meta_pac_sub) == 1) {
      row_copy <- row %>% mutate(
        downloaded = FALSE,
        variable = paste0(variable, "P"),
        race = "ASIAN OR PACIFIC ISLANDER",
        ntot_var = NA,
        relevant = TRUE
      )
      variable_asian <- row[["variable"]]
      variable_pac <- census_meta_pac_sub[1, "variable"]

      row_copy$tot_var[1] <- list(c(variable_asian, variable_pac))
      census_meta <- rbind(census_meta, row_copy)
    }
  }

  # aggregate 100+
  census_meta_old <- census_meta %>%
    filter(
      min_age >= 100,
      relevant == TRUE
    ) %>%
    group_by(year, gender, gender_label, race, hispanic_origin) %>%
    summarise(tot_var = list(variable)) 
  
  census_meta_old<-census_meta_old%>%
    mutate(
      ntot_var = NA,
      group = NA,
      min_age = 100,
      max_age = 150,
      age_group_id = 95,
      downloaded = FALSE,
      relevant = TRUE,
      variable = sapply(tot_var, function(list) paste0(list[[1]], "O"))
    )
  
  census_meta <- census_meta %>%
    mutate(relevant = ifelse(min_age >= 100,
                             FALSE,
                             relevant
    ))
  
  census_meta <- rbind(census_meta, census_meta_old)
  fwrite(census_meta, filepathCensMeta)
  toc()
}
