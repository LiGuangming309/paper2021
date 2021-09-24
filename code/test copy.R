library(censusapi)
library(tidycensus)

# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

dem.state.data <- getCensus(
  name = "dec/sf1",
  vintage = 2010,
  vars = paste0("group(", "PCT12A", ")"),
  region = "tract:*",
  regionin = sprintf("state:%02d", 12)
)

dem.state.data$GEO_ID <- dem.state.data$GEO_ID %>%
  str_sub(., -11, -1)

#1001020801 -lost in interpolation
#12087980100 actually missing
tracts<-get_decennial(geography = "tract", variables = "PCT012A009", year = 2010, state = "FL", geometry = TRUE, key = key)%>% 
  rename(GEO_ID = GEOID)

#plot(tracts)

#anti_join(tracts,dem.state.data,  by = "GEO_ID")
