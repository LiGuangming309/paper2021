#TODO
#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

packages <- c("data.table", "plyr", "magrittr", "testthat", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc", "units", "stats","matrixStats")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

result <- lapply(2009:2016, function(year){
  tic(year)
  mortDir <- paste0("~/Desktop/paper2021/data/08_total_burden/nvss/mort",year,".csv")
  if(file.exists(mortDir)){
    mort <- fread(mortDir)
    return(c(year, sum(!is.na(mort$educ2003))/nrow(mort)))
  }else{
    return(NA)
  }
  toc()
})

result <- as.data.frame(do.call(rbind, result))
plot(result$V1, result$V2)

