
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tictoc", "cdcfluview", "testthat", "rlang","tidycensus"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)
options(tigris_use_cache = TRUE)

year <- 2010
STUSPS <- "FL"
STATEFPI <- "12"
if(year == 1990){
  dem.state.data <- fread(file.path("/Users/default/Desktop/paper2021/data", "nhgis0002_ds120_1990_tract.csv"))
  
  cols <- c("state" = "STATEA", "county" = "COUNTYA", "tract" = "TRACTA", "ANPSADPI", "GEO_ID" = "GISJOIN") # "GEO_ID" = "GISJOIN",
  dem.state.data <- dem.state.data %>% select(all_of(cols))
  
  #BNA 9676.99
  dem.state.data <- dem.state.data %>%
    mutate(
      tract = ifelse(str_detect(ANPSADPI, "\\."), #ifelse(tract <1000, #
                     tract,
                     tract * 100
      ),
      GEO_ID1 = paste0( #implement
        state,
        sprintf("%03d", county),
        sprintf("%06d", tract)
      ),
      #GEO_ID2 = str_sub(GEO_ID, 2, -1) , #favorit
      #GEO_ID3 = paste0(
      #  sprintf("%02d", state),
      #  sprintf("%03d", county),
      #  sprintf("%06d", tract)
      #),

      ANPSADPI = NULL,
      #GEO_ID4 = str_sub(GEO_ID, 2, 12),
      #n = nchar(GEO_ID3)
    ) #%>%
    #mutate_at(c("GEO_ID2","GEO_ID3","GEO_ID4"),as.integer)
}else{
  if(year == 2000){
    group <- "P012A"
  }else if(year == 2010){
    group <- "PCT12A"
  }
  dem.state.data <- getCensus(
    name = "dec/sf1",
    vintage = year,
    vars = paste0("group(", group, ")"),
    region = "tract:*",
    regionin = sprintf("state:%02d", as.numeric(STATEFPI))
  ) %>% select(state, county,tract, GEO_ID)
  
  dem.state.data <- dem.state.data %>% mutate(#GEO_ID1 = paste0(state, county,tract),# implemented
                                              GEO_ID2 = sub(".*US", "", GEO_ID), #TO be implemented
                                              #GEO_ID5 = str_remove(GEO_ID2, "^0+"),
                                              #n =nchar(GEO_ID1)
                                              )
}



if(year %in% c(1990,2000)){
  #crosswalk <- read.csv(paste0("~/Desktop/paper2021/data/crosswalk_", year, "_2010.csv")) %>%
  #  mutate(GEO_ID1 = trtid00,
  #    trtidTo = as.character(trtidTo),
  #    stateTo = str_sub(trtidTo, 1, -10),
  #    countyTo = str_sub(trtidTo, -9, -6),
  #    tractTo = str_sub(trtidTo, -5, -1),
  #    tractTo = case_when(str_sub(tractTo,-2,-1) == "00"~ str_sub(tractTo,1,-3),
  #                        TRUE ~ tractTo),
  #    trtidTo = case_when(
  #      str_sub(trtidTo, -2, -1) == "00" ~ str_sub(trtidTo, 1, -3),
  #      TRUE ~ trtidTo
  #    ),
  #    trtidTo = as.numeric(trtidTo)
  #  )
  
  crosswalk <- read.csv(paste0("~/Desktop/paper2021/data/crosswalk_", year, "_2010.csv")) %>%
    transmute(  #GEO_ID1 = trtid00,
      GEO_ID1 = trtid90, #implemented
      #GEO_ID2 = str_pad(GEO_ID1, 11, pad = "0"),
      #GEO_ID3 = case_when(
      #  str_sub(GEO_ID2, -2, -1) == "00" ~ str_sub(GEO_ID2, 1, -3),
      #  TRUE ~ GEO_ID2
      #),
      #GEO_ID4 = case_when(
      #  str_sub(GEO_ID2, -2, -1) == "00" ~ str_sub(GEO_ID2, 1, -3), # ,"99"
      #  str_sub(GEO_ID2, -2, -1) == "99" ~ paste0(str_sub(GEO_ID2, 1, -3)), # ,"01"
      #  TRUE ~ GEO_ID2
      #)
    )
}


if(year == 1990){
  #tracts<-get_decennial(geography = "tract", variables = "TODO", year = 1990, state = STUSPS, geometry = TRUE)
  tracts <- tigris::tracts(state = STUSPS, cb = TRUE, year = year)
  tracts <- tracts %>% mutate(GEO_ID = paste0(STATEFP, COUNTYFP, TRACTBASE, TRACTSUF))%>%
    select(-c("AREA", "PERIMETER", "geometry")) 

}else if(year %in% c(1991:1999,2001:2008, 2010)){
  tracts<-get_decennial(geography = "tract", variables = "PCT012A009", year = 2010, state = STUSPS, geometry = TRUE, key = key)%>% 
    rename(GEO_ID = GEOID)
}else if(year == 2000){
  tracts<-get_decennial(geography = "tract", variables = "P012A005", year = 2000, state = STUSPS, geometry = TRUE, key = key)%>% 
    rename(GEO_ID = GEOID)
  
  # tracts<-tidycensus::get_decennial(geography = "tract", variables = "P012A005", year = year, state = "AK", geometry = FALSE, key = key)%>%
  #  transmute(GEO_ID1 = GEOID,
  #            GEO_ID2 = str_pad(GEO_ID1,11, side="right",pad = "0"))
  
}else if(year %in% c(2009,2011:2016)){
  tracts<-get_acs(geography = "tract", variables = "B01001A_003E", state = STUSPS, geometry = TRUE, year = year, key = key)%>% 
    rename(GEO_ID = GEOID)
}


test1 <- anti_join(dem.state.data, crosswalk, by = c("GEO_ID1" = "GEO_ID1"))
test1
test1 <- anti_join(crosswalk,dem.state.data,  by = c("GEO_ID1" = "GEO_ID2"))
test1

#so farbest combination: dem.state.data$GEO_ID2, crosswalk$GEO_ID2, tracts$GEO_ID1
#=> work on crosswalk$GEO_ID2!

test2 <- setdiff(tracts$GEO_ID, dem.state.data$GEO_ID1)
test2
test2 <- setdiff(dem.state.data$GEO_ID1, tracts$GEO_ID)
test2

dem.state.data <- dem.state.data %>%
  transmute(state, county, tract, GEO_ID1 = as.numeric(GEO_ID1)) %>%
  mutate(
    GEO_ID1 = as.character(GEO_ID1),
    n = nchar(GEO_ID1)
  )

dem.state.data2 <- dem.state.data %>%
  mutate(
    GEO_ID1 = as.character(GEO_ID1),
    GEO_ID2 = case_when(
      nchar(GEO_ID1) >= 10 ~ GEO_ID1,
      TRUE ~ paste0(GEO_ID1, "00")
    ),
    GEO_ID2 = str_pad(GEO_ID2, 11, pad = "0")
  )

dem.state.data3 <- dem.state.data %>%
  mutate(
    GEO_ID1 = as.character(GEO_ID1),
    GEO_ID2 = case_when(
      nchar(GEO_ID1) >= 10 ~ GEO_ID1,
      TRUE ~ paste0(GEO_ID1, "00")
    ),
    GEO_ID2 = str_pad(GEO_ID2, 11, pad = "0")
  )
