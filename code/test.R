packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes", "stats")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

wt <- c(5,  5,  4,  2)*15
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)
