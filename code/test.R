
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "tigris","tmap" , "tictoc",
 "testthat","tidyverse"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
pop.summary.dir <- "/Users/default/Desktop/paper2021/data/11_population_summary"
summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"

###----- pm summ----
agr_bys <- "nation" #TODO löschen
pm_summ <- lapply(agr_bys, function(agr_by){
  tic(paste("summarized pm data by", agr_by))
  years <- list.files(file.path(dem_agrDir, agr_by))
  years <- 2016
  pm_summ <- lapply(years, function(year){
    meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
    files <- list.files(file.path(dem_agrDir, agr_by, year))
    pm_summ<-lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist
    if(nrow(pm_summ) >0){
      pm_summ<- pm_summ %>% left_join(meta, by = "variable")
      return(pm_summ)
    }else{
      return(NULL)
    }
    
  }) %>% rbindlist
  #make compatible
  pm_summ <- pm_summ %>% rename("Region":=!!agr_by)
  pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)
  
  pm_summ <- pm_summ %>% filter(Race == "All" &  Hispanic.Origin == "All Origins" & Education == "666" & rural_urban_class == "666")
  pm_summ <- pm_summ %>%
    group_by_at(vars(all_of(setdiff(colnames(pm_summ),c("variable","pop_size","prop","min_age", "max_age"))))) %>%
    summarise(pop_size = sum(pop_size))
  toc()
  return(pm_summ)
}) %>% rbindlist

pm_summ <- pm_summ %>%
  group_by_at(vars(all_of(setdiff(colnames(pm_summ),c("variable","pop_size","prop","min_age", "max_age","pm"))))) %>%
  #group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, scenario, Education) %>%
  summarise(mean = weighted.mean(pm, pop_size),
            median = matrixStats::weightedMedian(pm, pop_size))

pm_summ <- pm_summ %>%
  pivot_longer(c(mean,median),
               names_to = "pm_metric")
pm_summ <- pm_summ %>% filter(pm_metric == "mean")
##--- read rural urban class----
rural_urban_class <- read.csv("~/Desktop/paper2021/data/rural_urban_class.csv") %>%
  filter(fromYear == 2010) %>%
  select(FIPS.code, rural_urban_class)
##--shape files----
counties_shapeDir <- file.path(tmpDir, paste0("counties_", 2016, ".RData"))
if (!file.exists(counties_shapeDir)) {
  counties_shape <- tigris::counties(year = 2016) %>% select(STATEFP, GEOID, geometry)
  saveRDS(counties_shape, counties_shapeDir)
}
counties_shape <- readRDS(counties_shapeDir) %>% 
  mutate(GEOID = as.integer(GEOID))%>%
  select(GEOID, geometry)
rm(counties_shapeDir)

##-- plot---
rural_ruban_shape <- inner_join(counties_shape, rural_urban_class, by = c("GEOID" ="FIPS.code")) %>%
  mutate(GEOID = NULL)
#plot(rural_ruban_shape)

g1<- ggplot(data = rural_ruban_shape) +
  geom_sf()
g1
#pm_summ_shape <- inner_join(pm_summ, rural_urban_class, by = c("GEOID" ="Region")) %>%
#  mutate(GEOID = NULL)