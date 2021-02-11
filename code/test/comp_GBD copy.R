# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"


label_causes <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "cvd_stroke")
filenames <- c(paste0(label_causes,".txt"),paste0(label_causes,"_all.txt"))
lapply(filenames, function(filename){
  
})