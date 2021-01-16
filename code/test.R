test_dem.state.data <- censData00 %>%
  group_by(GEO_ID,variable) %>%
  summarise(appearances = n())
#1126080
#1048576