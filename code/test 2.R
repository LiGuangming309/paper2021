packages <- c("dplyr","tidyr", "testthat","magrittr","stringr","data.table","tictoc","foreign","sets")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
dataDir <- "/Users/default/Desktop/paper2020/data"
censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

crosswalk <- read.dta(file.path(dataDir,"crosswalk_2010_2000.dta"))
crosswalk_filt <- crosswalk %>%
  filter(weight != 0)

states <- file.path(tmpDir, "states.csv") %>% read.csv
state<-states[1,]
STUSPS <- state[["STUSPS"]] 
name <- state[["NAME"]]
STATEFP <- state[["STATEFP"]] %>%formatC(width = 2, flag=0)

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")

censData00 <-  file.path(censDir00, paste0("census_2000_", STUSPS, ".csv"))%>%
  fread(colClasses=c(pop_size="numeric")) %>%
  mutate(GEO_ID = str_pad(GEO_ID, 11, pad = "0"))

censData10 <-  file.path(censDir10, paste0("census_2010_", STUSPS, ".csv"))%>%
  fread(colClasses=c(pop_size="numeric")) %>%
  mutate(GEO_ID = str_pad(GEO_ID, 11, pad = "0"))

geo_id00 <-censData00$GEO_ID %>% unique
geo_id10 <-censData10$GEO_ID %>% unique

geo_id00_cross <- crosswalk %>% 
  select(trtid00) %>%
  filter(str_sub(trtid00,1,2)==STATEFP) %>%
  unique

geo_id10_cross <- crosswalk %>% 
  select(trtid10) %>%
  filter(str_sub(trtid10,1,2)==STATEFP) %>%
  unique

#geo_id_dif <- sets::set_symdiff(geo_id00,geo_id10)
