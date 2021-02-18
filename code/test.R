# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
exposure <- read.csv("~/Downloads/annual_conc_by_monitor_2000.csv", header=TRUE)

exposure <- exposure %>% filter(State.Code %in% c(1,15,53))
#	PM2.5 - Local Conditions

test <- exposure$Parameter.Name %>% unique %>%sort
