
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census meta data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "DataCombine", "data.table", "tidyverse",
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
if (rlang::is_empty(args)) {
  year <- 2000

  # censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
  # tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
}

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
  # excluding American Samoa, Guam, Commonwealth of the Northern Mariana Islands, Puerto Rico, United States Virgin Islands
  states1 <- states() %>%
    as.data.frame() %>%
    select(c(REGION, DIVISION, STATEFP, STUSPS, NAME)) %>%
    filter(!(STUSPS %in% c("AS", "GU", "MP", "PR", "VI"))) %>%
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
censMetaDir <- file.path(censDir, "meta_down")
dir.create(censMetaDir, recursive = T, showWarnings = F)

filepathCensMeta <- paste0("cens_meta_", toString(year), ".csv") %>%
  file.path(censMetaDir, .)

# relevant groups for each year and table names
if (year %in% 2000:2008) {
  # decennical census, sex by age for races
  table_groups <- data.frame(
    #groups = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I", "PCT012J", "PCT012K", "PCT012L", "PCT012M"),
    groups = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I"),
    tablename = "dec/sf1",
    vintage = 2000
  )
} else if (year == 2010) {
  # decennical census, sex by age for races
  table_groups <- data.frame(
    #groups = c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M"),
    groups = c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I"),
    tablename = "dec/sf1",
    vintage = year
  )
} else if (year %in% c(2009,2011:2016)) {
  # american community survey, sex by age for races
  table_groups <- data.frame(
    groups = c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H"),
    tablename = "acs/acs5",
    vintage = year
  )
}

if (year %in% 2009:2016) {
  # american community survey, sex by age for educational attainment
  table_groups <- rbind(
    table_groups,
    data.frame(groups = "B15001", tablename = "acs/acs5", vintage = year)
  )
}

# download meta data, if necessary
if (!file.exists(filepathCensMeta)) {
  tic(paste("Downloaded census meta data for year", toString(year)))

  # loop over all relevant groups
  census_meta <- apply(table_groups, 1, function(row) {
    tablename <- row[["tablename"]]
    group <- row[["groups"]]
    #download meta data
    census_meta <- listCensusMetadata(
      name = tablename,
      vintage = row[["vintage"]],
      type = "variables",
      group = group
    ) %>%
      select("name", "label", "concept") # select relevant columns
    
    #parse information a bit
    census_meta <- census_meta %>% mutate(
      Year = year,
      tablename = tablename,
      group = group,
      ## parse "label", "concept" from String to seperate columns
      label = strsplit(label, "!!"),
    ) 
    
    # the acs includes estimates and annotation of estimates
    census_meta$datatype <- sapply(census_meta$label, function(l) ifelse(tablename == "acs/acs5", l[[1]], "Estimate"))
    
    census_meta$label <- lapply(census_meta$label , function(l) {
      # making acs label notation coherant with dec cens notation
      if(tablename == "acs/acs5"){
        return(l[-1])
      }else{
        return(l)
      }
      })
    
    #filter relevant rows
    census_meta <- census_meta %>%
      mutate(label_len = sapply(label, length)) %>%
      filter(
        label_len == ifelse(group == "B15001", 4, 3), # filters granular data with gender and age group
        datatype == "Estimate" # filters Estimates, excluding Annotations and Margins of Error
      ) %>% mutate(label_len = NULL, datatype = NULL)

    census_meta <- census_meta %>% mutate(
      # extracts gender
      gender = label %>% sapply(function(l) l[2]),
      Gender.Code = gender %>% sapply(function(g) ifelse(g == "Female", "F", "M")),
      #age
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
      age = NULL)
    
    if(group == "B15001"){
      census_meta <- census_meta %>% mutate(
        Education = label %>% sapply(function(l) l[4]),
        Race = "all",
        Hispanic.Origin = "all"
      )
    }else{
      census_meta <- census_meta %>% mutate(
        race_his = concept %>% sapply(function(conc) {
          # extract the information in the brackets
          # e.g. sex by age (White alone, not hispanic or latino) => White alone, not hispanic or latino
          regmatches(conc, gregexpr("(?<=\\().*?(?=\\))", conc, perl = T))[[1]]
        }),
        # extract race
        Race = race_his %>% sapply(function(race_his) {
          race_his %>%
            strsplit(., ",") %>%
            unlist() %>%
            extract2(1) %>%
            # fetch what comes before ","; e.g.: White alone, not hispanic or latino => White alone
            substr(., 1, nchar(.) - 6)
          # removes " alone", e.g. "White alone" => "White"
        }),
        # extracts hispanic origin. option: not Hispanic or latino, Hispanic or latino, all
        Hispanic.Origin = race_his %>% sapply(function(race_his) {
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
        race_his = NULL,
        Education = 666
      )
    }
    census_meta <- census_meta %>% mutate(label = NULL, concept = NULL)
    return(census_meta)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()  

  setnames(census_meta, "name", "variable") # rename for later purpose

  # drop unrequired information
  fwrite(census_meta, filepathCensMeta)

  toc()
}
