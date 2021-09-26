
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
  vintage = year,
  vars = paste0("group(", "P012A", ")"),
  region = "tract:*",
  regionin = sprintf("state:%02d", 2)
) %>% select(state, county,tract, GEO_ID)

dem.state.data <- dem.state.data %>% mutate(GEO_ID1 = paste0(state, county,tract),#,
                                            GEO_ID2 = sub(".*US", "", GEO_ID),
                                            GEO_ID5 = str_remove(GEO_ID2, "^0+"),
                                            n =nchar(GEO_ID1))

crosswalk_2000_2010 <- read.csv("~/Desktop/paper2021/data/crosswalk_2000_2010.csv")%>% 
  transmute(GEO_ID1 = trtid00,
         GEO_ID2 = str_pad(trtid00, 11, pad = "0"),
         GEO_ID3 = case_when(str_sub(GEO_ID2,-2,-1) == "00"~ str_sub(GEO_ID2,1,-3),
                              TRUE ~ GEO_ID2)
)

  
tracts<-tidycensus::get_decennial(geography = "tract", variables = "P012A005", year = year, state = "AK", geometry = FALSE, key = key)%>% 
  transmute(GEO_ID1 = GEOID,
            GEO_ID2 = str_pad(GEO_ID1,11, side="right",pad = "0"))


test2 <- setdiff(dem.state.data$GEO_ID2, tracts$GEO_ID2)
test2

test1 <- setdiff(dem.state.data$GEO_ID2, crosswalk_2000_2010$GEO_ID2)
test1

dem.state.data <- dem.state.data %>% 
  transmute(state, county, tract, GEO_ID1=as.numeric(GEO_ID1)) %>%
  mutate(GEO_ID1 = as.character(GEO_ID1),
         n= nchar(GEO_ID1))

dem.state.data2 <- dem.state.data %>%
  mutate(GEO_ID1 = as.character(GEO_ID1),
         GEO_ID2 = case_when(nchar(GEO_ID1) >=10 ~ GEO_ID1,
                           TRUE ~ paste0(GEO_ID1, "00")),
         GEO_ID2 = str_pad(GEO_ID2, 11, pad = "0"))

dem.state.data3 <- dem.state.data %>%
  mutate(GEO_ID1 = as.character(GEO_ID1),
         GEO_ID2 = case_when(nchar(GEO_ID1) >=10 ~ GEO_ID1,
                             TRUE ~ paste0(GEO_ID1, "00")),
         GEO_ID2 = str_pad(GEO_ID2, 11, pad = "0"))