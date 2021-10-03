#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr",
  "dplyr", "tigris", "tmap", "testthat","tidyverse"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2016
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  
  #tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  #summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
  #figuresDir <- "C:/Users/Daniel/Desktop/paper2021/data/15_figures"
  scenarioI <- "A"
  methodI <- "di_gee"
}

## --- read files---
file_list <- list.files(file.path(summaryDir, "county"))
file_list <- file.path(summaryDir, "county", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = TRUE)
rm(file_list)

attr_burd$method %>% unique()
attr_burd <- attr_burd %>%
  filter(
    Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == methodI
    & attr == "attributable" &
      source == "National Vital Statistics System" & scenario == scenarioI & rural_urban_class == "All" & agr_by == "county"
    & Year %in% year # 2000:2016
  )


attr_burd_sum <- attr_burd %>%
  filter(measure3 == "value" & Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American")) %>%
  mutate(mean = case_when(
    Ethnicity == "White, Not Hispanic or Latino" ~ mean,
    Ethnicity == "Black or African American" ~ -mean
  )) %>%
  group_by(Region, Year) %>%
  summarise(mean = sum(mean)) %>%
  group_by(Region) %>%
  summarise(mean = mean(mean))

### ---- download shape if not downloaded yet---
counties_shapeDir <- file.path(tmpDir, paste0("counties_", year, ".RData"))
if (!file.exists(counties_shapeDir)) {
  counties_shape <- tigris::counties(year = 2016) %>% select(STATEFP, GEOID, geometry)
  saveRDS(counties_shape, counties_shapeDir)
}
counties_shape <- readRDS(counties_shapeDir) %>% mutate(GEOID = as.integer(GEOID))
rm(counties_shapeDir)

states <- tigris::states() %>% filter(!(STUSPS %in% c("AS", "GU", "MP", "PR", "VI", "HI", "AK"))) #TODO + Alaska, Hawaii

## ---plot---
test_that("figure5 map anti join", {
  anti_join1 <- anti_join(counties_shape, attr_burd, by = c("GEOID" = "Region")) %>% select(GEOID)
  anti_join2 <- anti_join(attr_burd, counties_shape, by = c("Region" = "GEOID")) %>%
    select(Region) %>%
    distinct()

  expect_equal(nrow(anti_join1) * nrow(anti_join2), 0)
})

#counties_shape <- counties_shape %>% filter(STATEFP == "06") #TODO
counties_join <- inner_join(counties_shape, attr_burd_sum, by = c("GEOID" ="Region"))

#https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html
tm1 <- tm_shape(states) +
  tm_borders(lwd = 0.5, col = "black") +
  tm_fill(col = "grey", bg.alpha = 0.6)+ 
  tm_shape( counties_join)+#, projection = 26916
  tm_polygons(col = "mean", midpoint = 0, title = "burden for White - burden for Black")+ 
  tm_legend(bg.color = "white", bg.alpha = 0.6)# #+
  #tm_layout("Wealth (or so)",
  #          legend.title.size = 1,
  #          legend.text.size = 0.6,
  #          legend.position = c("left","bottom"),
  #          legend.bg.color = "white",
  #          legend.digits = 5,
  #          legend.bg.alpha = 1)
  # tmap_mode(mode = c("view"))

tm1
  ## ---save---
  tmap_save(tm1, file.path(figuresDir, "figure4.png"),
            dpi = 300)
