

#tracts<-tidycensus::get_decennial(geography = "tract",
#                                  table = "dec/sf1",
#                                  variables = "PCT012A009", 
#                                  year = year, 
#                                  state = STUSPS, 
#                                  geometry = FALSE,
#                                  key = key)%>% 
#  rename(GEO_ID = GEOID)

#dem.state.data <- getCensus(
#  name = row[["tablename"]],
#  vintage = year,
#  vars = paste0("group(", group, ")"),
#  region = "tract:*",
#  regionin = sprintf("state:%02d", STATEFP)
#)
year <- 2000
dem.state.data <- getCensus(
  name = "dec/sf1",
  vintage = 2000,
  vars = paste0("group(", "P012A", ")"),
  region = "tract:*",
  regionin = sprintf("state:%02d", 2)
)


  
dem.state.data <- dem.state.data %>% mutate(GEO_ID = paste0(state, county,tract))
tracts<-tidycensus::get_decennial(geography = "tract", variables = "P012A005", year = 2010, state = "AK", geometry = TRUE, key = key)%>% 
  rename(GEO_ID = GEOID)

tracts<-tidycensus::get_acs(geography = "tract", variables = "B01001A_003E", state = "AK", geometry = FALSE, year = year, key = key)%>% 
  rename(GEO_ID = GEOID)

test <- inner_join(dem.state.data, tracts, by = "GEO_ID")
