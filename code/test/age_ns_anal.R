# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
agr_by <- "nation"

totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
totalBurdenDir <- file.path(totalBurdenDir, agr_by)

label_causes <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "cvd_stroke")
filenames <- c(paste0(label_causes,".txt"),paste0(label_causes,"_all.txt"))

total_burden<-lapply(filenames, function(filename){
  total_burden <- read.delim(file.path(totalBurdenDir, filename))
  
  total_burden <- total_burden %>% subset(select = -Notes)
  total_burden <- total_burden[!apply(is.na(total_burden) | total_burden == "", 1, all), ]
  
  total_burden <- total_burden %>%
    select(Single.Year.Ages, Deaths) %>%
    filter(Deaths != "Suppressed") %>%
    mutate(Deaths = as.numeric(Deaths))
  
  return(total_burden)
}) %>% do.call(rbind, .)

options(scipen = 50)

total_burden <- total_burden %>% 
  group_by(Single.Year.Ages) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(prop = 100*Deaths/sum(Deaths))