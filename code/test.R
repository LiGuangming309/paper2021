# TODO l?schen
args <- commandArgs(trailingOnly = T)
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  cens_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
}

cens_agrDir <- file.path(cens_agrDir, agr_by)
years <- list.files(cens_agrDir)
tmrel<-lapply(years, function(year) {
  files <- list.files(file.path(cens_agrDir, year))
  lapply(files, function(file) {
    cens_agr<-read.csv(file.path(cens_agrDir, year,file))
    return(min(cens_agr$pm))
  }) %>% min
}) %>% min
