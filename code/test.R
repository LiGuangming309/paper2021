
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "tigris","tmap" , "tictoc",
 "testthat","tidyverse",  "readxl",
 
 "data.table", "magrittr",
 "dplyr", "tigris", "tmap", "testthat","tidyverse"
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

dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr"


###----- pm summ----
agr_bys <- "county" #TODO löschen
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
  
  pm_summ <- pm_summ %>% filter(Race == "All" &  Hispanic.Origin == "All Origins" & Education == "666") #& rural_urban_class == "666"
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
rural_urban_class1 <- read.csv(file.path(dataDir, "rural_urban_class.csv")) %>%
  filter(fromYear == 2010) %>%
  select(FIPS.code, rural_urban_class)

#suppressMessages(
#  rural_urban_class2 <- read_excel(file.path(dataDir, "NCHSURCodes2013.xlsx"), .name_repair = "universal") %>%
#    transmute(FIPS.code, rural_urban_class = ..2013.code) #, pop_size = County.2012.pop
#)
#rural_urban_class2<- rural_urban_class2 %>% mutate(rural_urban_class = as.factor(rural_urban_class))
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

states <- tigris::states() %>% filter(!(STUSPS %in% c("AS", "GU", "MP", "PR", "VI", "HI", "AK"))) #TODO + Alaska, Hawaii

##-- plot---
join_everything <- counties_shape %>%
  inner_join(rural_urban_class1, by = c("GEOID" ="FIPS.code")) %>%
  inner_join(pm_summ %>% filter(Year == 2016), by = c("GEOID" ="Region"))


tm1 <- tm_shape(states) +
  tm_borders(lwd = 0.5, col = "black") +
  tm_fill(col = "grey", bg.alpha = 0.6)+
  tm_shape(join_everything)+#, projection = 26916
  tm_polygons(col = "rural_urban_class.x")+ 
  tm_legend(bg.color = "white", bg.alpha = 0.6)

#tmap_save(tm1,   "C:/Users/Daniel/Desktop/rural_ruban1.png",
#          dpi = 100)

tm2 <- tm_shape(states) +
  tm_borders(lwd = 0.5, col = "black") +
  tm_fill(col = "grey", bg.alpha = 0.6)+
  tm_shape(join_everything)+#, projection = 26916
  tm_polygons(col = "rural_urban_class.y")+ 
  tm_legend(bg.color = "white", bg.alpha = 0.6)

tmap_save(tm2,   "C:/Users/Daniel/Desktop/rural_ruban2.png",
          dpi = 100)

tm3 <- tm_shape(states) +
  tm_borders(lwd = 0.5, col = "black") +
  tm_fill(col = "grey", bg.alpha = 0.6)+
  tm_shape(join_everything)+#, projection = 26916
  tm_polygons(col = "value")+ 
  tm_legend(bg.color = "white", bg.alpha = 0.6)
tmap_save(tm3,   "C:/Users/Daniel/Desktop/pm_summ.png",
                    dpi = 100)
