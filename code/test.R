# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "DataCombine", "data.table", "tidyverse",
  "tigris", "tictoc", "cdcfluview", "testthat", "rlang","tigris"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


  year <- 2010
  
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_demog"
  # tmpDir <-  "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"


filepathStates <- file.path(tmpDir, "states.csv")
states <- read.csv(filepathStates)

census_meta <- read.csv(file.path(censDir, "meta_down", paste0("cens_meta_", toString(year), ".csv")))
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

# identify relevant variables
if(year %in% 2001:2009){
  census_meta <- census_meta %>% filter(tablename != "dec/sf1")
} 

relevant_variables <- census_meta$variable %>% unique()
table_groups <- census_meta %>%
  select(group, tablename) %>%
  distinct

apply(states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  dem.state.data <- apply(table_groups, 1, function(row) {
    group <- row[["group"]]
    tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
    dem.state.data <- getCensus(
      name = row[["tablename"]],
      vintage = year,
      vars = paste0("group(", group, ")"),
      region = "tract:*",
      regionin = sprintf("state:%02d", STATEFP)
    )
    toc()
    return(dem.state.data)
  }) %>%
    rbindlist(fill = TRUE) %>%
    as.data.frame()
  
  dem.state.data$GEO_ID <- paste0(
    dem.state.data$state , #%>% as.character %>% str_pad(width = 2, pad = "0")
    dem.state.data$county ,
    dem.state.data$tract 
  )
  
  #tracts <- tracts(state = STUSPS, cb = TRUE, year = 2000)
  #tracts$GEO_ID <-paste0(tracts$STATE,tracts$COUNTY,tracts$TRACT)
  
  tracts<-tidycensus::get_decennial(geography = "tract", variables = "PCT012A009",
           state = STUSPS, geometry = TRUE, year = year)
  
  tracts <- tracts %>% rename(GEO_ID = GEOID)
  test1 <- anti_join(dem.state.data, tracts, by = "GEO_ID")
  test2 <- anti_join( tracts, dem.state.data, by = "GEO_ID")
  print("test")
  })