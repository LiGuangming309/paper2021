STUSPS_copy<-STUSPS
test<-missing_statesDir %>%
  read.csv %>%
  filter(STUSPS != STUSPS_copy)