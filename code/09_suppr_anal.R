
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[3]
agr_by <- args[10]

totalBurdenDir <- args[13]


# TODO delete
if (rlang::is_empty(args)) {
  year <- 2016
  agr_by <- "nation"
  
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
}

label_causes_all <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "Cvd_stroke")
Cvd_stroke_join <- lapply(label_causes_all, function(label_cause) {
  lapply(c("", "_all"), function(end) {
    filename <- paste0(label_cause, end, ".txt")
    # https://wonder.cdc.gov/controller/saved/D77/D105F096
    Cvd_stroke1 <- read.delim(paste0("~/Desktop/paper2021/data/08_total_burden/nation/", filename))
    if (!"Hispanic.Origin" %in% colnames(Cvd_stroke1)) Cvd_stroke1[, "Hispanic.Origin"] <- "All Origins"
    # https://wonder.cdc.gov/controller/saved/D77/D107F342
    Cvd_stroke2 <- read.delim(paste0("~/Desktop/paper2021/data/test/", filename))
    if (!"Hispanic.Origin" %in% colnames(Cvd_stroke2)) Cvd_stroke2[, "Hispanic.Origin"] <- "All Origins"

    # TODO age not sted?
    Cvd_stroke1 <- Cvd_stroke1 %>%
      filter(
        Deaths != "Suppressed"
      ) %>%
      mutate(Deaths = as.numeric(Deaths)) %>%
      group_by(Race, Hispanic.Origin) %>%
      summarise(Deaths = sum(Deaths)) %>%
      filter(!is.na(Deaths))

    Cvd_stroke2 <- Cvd_stroke2 %>%
      filter(Deaths != "Suppressed") %>%
      mutate(
        Deaths = as.numeric(Deaths),
        Population = as.numeric(Population),
        Crude.Rate = as.numeric(Crude.Rate)
      ) %>%
      group_by(Race, Hispanic.Origin) %>%
      summarise(
        Deaths = sum(Deaths),
        Population = sum(Population),
        Crude.Rate = sum(Crude.Rate)
      )

    Cvd_stroke_join <- left_join(Cvd_stroke1, Cvd_stroke2, by = c("Race", "Hispanic.Origin"))

    Cvd_stroke_join$label_cause <- label_cause
    return(Cvd_stroke_join)
  }) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)


#Cvd_stroke_join2 <- Cvd_stroke_join %>%
#  group_by(Race, Hispanic.Origin) %>%
#  summarise(
#    Deaths.x = sum(Deaths.x),
#    Deaths.y = sum(Deaths.y),
#    Population = sum(Population),
#    Crude.Rate = sum(Crude.Rate)
#  )

Cvd_stroke_join <- Cvd_stroke_join %>%
  mutate(
    suppressed = 100 - 100 * Deaths.x / Deaths.y,
    factor = Deaths.y / Deaths.x
  )

fwrite(
  Cvd_stroke_join,
  "~/Desktop/paper2021/data/test/ethn_suppr.csv"
)
