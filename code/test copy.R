
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tictoc", "cdcfluview", "testthat", "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

year <- 2000
dem.state.data <- getCensus(
  name = "dec/sf1",
  vintage = 2000,
  vars = paste0("group(", "P012A", ")"),
  region = "tract:*",
  regionin = sprintf("state:%02d", 2)
) %>% select(state, county,tract, GEO_ID)

dem.state.data <- dem.state.data %>% mutate(GEO_ID1 = paste0(state, county,tract)#,
                                            #GEO_ID2 = sub(".*US", "", GEO_ID)
                                            #,GEO_ID5 = str_remove(GEO_ID2, "^0+")
                                            )

crosswalk_2000_2010 <- read.csv("~/Desktop/paper2021/data/crosswalk_2000_2010.csv")%>% 
  mutate(trtid00 = str_pad(trtid00, 11, pad = "0"),
         trtid00 = case_when(str_sub(trtid00,-2,-1) == "00"~ str_sub(trtid00,1,-3),
                              TRUE ~ trtid00)
)

  
tracts<-tidycensus::get_decennial(geography = "tract", variables = "P012A005", year = year, state = "AK", geometry = FALSE, key = key)%>% 
  rename(GEO_ID = GEOID)

dem.state.data <- dem.state.data %>% mutate(GEO_ID = as.character(GEO_ID))
crosswalk_2000_2010 <- crosswalk_2000_2010 %>% mutate(trtid00 = as.character(trtid00))
tracts <- tracts %>% mutate(GEO_ID = as.character(GEO_ID))

test2 <- setdiff(dem.state.data$GEO_ID1, tracts$GEO_ID)
test2

test1 <- setdiff(dem.state.data$GEO_ID1, crosswalk_2000_2010$trtid00)
test1
