#count census tracts
# load packages, install if missing
packages <- c(
  "dplyr", "magrittr",  "data.table", "tidyverse",
  "tictoc"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/02_tracts"

years <- list.files(tracDir)
count_tracts <- lapply(years, function(year){
  states <- list.files(file.path(tracDir, year))
  count_tracts <- sapply(states, function(count_tracts){
    readRDS(file.path(tracDir, year,count_tracts )) %>% nrow
  }) 
  return(data.frame( year = year, nrow = sum(count_tracts)))
  
}) %>% rbindlist

sum(count_tracts$nrow)
