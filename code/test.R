
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "truncnorm", "triangle",
  "matrixStats"
)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

total_burden_2016 <- read.csv("~/Desktop/paper2021/data/12_total_burden_parsed2/nation/nvss/total_burden_2016.csv")

total_burden_2016 <- total_burden_2016 %>%
  filter(Race == "All" & Hispanic.Origin == "All Origins" & Education == 666
  & label_cause == "all-cause" & measure1 == "Deaths"  & rural_urban_class == 666)

total_burden_2016 <- total_burden_2016 %>% mutate(age65 = (min_age >= 65))

total_burden_2016 <- total_burden_2016 %>%
  group_by(age65, measure2) %>%
  summarise(value = sum(value)) %>%
  group_by(measure2) %>%
  mutate(value = value/sum(value))
