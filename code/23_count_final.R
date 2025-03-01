#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales", "tidyr") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)

#load calculated data
#summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
cens_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr/nation"
summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"

tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/02_tracts"
exp_tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/03_exp_tracts"
censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
cens_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr/nation"
summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
#if not downloaded, load from github
if(!file.exists(summaryDir)) summaryDir <- 'https://raw.github.com/FridljDa/paper2021/master/data/14_summary'

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attrBurden <- lapply(file_list, fread) %>% rbindlist
rm(file_list)

all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pm_summ <- pm_summ %>% filter(scenario == "A")
pop_summary <- fread(file.path(summaryDir, "pop_summary.csv"))

##-----pm summary ----
pm_summ_sub <- pm_summ %>% 
  filter(Ethnicity == "All, All Origins" & Region == "United States" & Year %in% c(1990,2016)
         & Education == 666 & rural_urban_class == "All" & pm_metric == "mean") %>%
  mutate(value = round(value, 2))
## count states, where whites less affected
attrBurden1 <- attrBurden %>% filter(Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American") &
                                      Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & Region != "United States"
                                     & Year == 2004 & method == "burnett" & measure3 == "value")

attrBurden1 <- attrBurden1 %>%
  group_by(Region, scenario) %>%
  slice(which.max(mean)) %>%
  group_by(scenario, Ethnicity) %>%
  summarise(n = n())

## count states, where more than 10% of disparity explained by PM2.5
attrBurden2 <- attrBurden %>% filter(Ethnicity %in% c("White, Not Hispanic or Latino") &
                                       Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & Region != "United States"
                                     & Year == 2016 & method == "burnett" & measure3 == "proportion of disparity to Black or African American attributable")
attrBurden2 <- attrBurden2 %>%
  group_by(scenario) %>%
  summarise(n = sum(mean >= 10))

## count tracts not sufficing national quality standard
year <- 2016
files <- list.files(file.path(exp_tracDir,"2016"))
exp_tracts1990 <- lapply(files, function(file){
   file.path(exp_tracDir,toString(year),file)  %>% 
          fread %>%
          mutate(GEO_ID = as.numeric(GEO_ID)) 
}

  ) 

exp_tracts1990 <- rbindlist(exp_tracts1990)
100*sum(exp_tracts1990$pm > 12)/nrow(exp_tracts1990)

##what part of population affected 

cens_meta <- fread(file.path(censDir, "meta", paste0("cens_meta_",year,".csv")))
cens_agr <- fread(file.path(cens_agrDir, year, paste0("cens_agr_",year,"_us.csv"))) %>% filter(scenario == "A")

cens_agr <- left_join(cens_agr, cens_meta, by= "variable") %>%
  group_by(Year, Race,Hispanic.Origin, pm) %>%
  summarize(pop_size = sum(pop_size)) %>%
  group_by(Year, Race, Hispanic.Origin)  %>%
  mutate(prop = pop_size/sum(pop_size)) %>%
  ungroup %>%
  mutate(above = pm > 8) %>%
  group_by(Year, Race, Hispanic.Origin, above) %>%
  summarise(prop = 100*sum(prop),
            pop_size = sum(pop_size))

##--- study population----
pop_summary1 <- pop_summary %>% filter(Region == "United States"  & Gender.Code == "All genders" & Education != 666 & rural_urban_class == "All" & Year %in% c(2009,2016)) %>%
          group_by(Year) %>%
        mutate(prop = 100*Population/sum(Population),
               prop = round(prop, 2))

pop_summary2 <- pop_summary %>% filter(Region == "United States"  & Gender.Code == "All genders" & Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year %in% c(2000,2016)) %>%
  group_by(Year) %>%
  mutate(prop = 100*Population/sum(Population),
         prop = round(prop, 2))

pop_summary3 <- pop_summary %>% filter(Region == "United States"  & Gender.Code == "All genders" & Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All"
                                        & Year %in% c(1990,2016) 
                                        & source2 == "Official Bridged-Race Population Estimates") %>%
  group_by(Year) %>%
  mutate(prop = 100*Population/sum(Population),
         prop = round(prop, 2))

pop_summary_sub <-rbind(pop_summary1, pop_summary2, pop_summary3)

rm(pop_summary1, pop_summary2, pop_summary3)
## count tract year combinations
years <- 1990:2016
#tract_years <- sapply(years, function(year){
#  files <- list.files( file.path(tracDir, toString(year)))
#  sapply(files, function(file){
#    readRDS(file.path(tracDir, toString(year), file)) %>% nrow
#  }) %>% sum
#}) %>% sum
##--- total number of deaths considered----
all_burden_sub <- all_burden %>% filter(Region == "United States" & Ethnicity == "All, All Origins" &
                                          Education == "666" & rural_urban_class == "All" & measure1 == "Deaths" & measure2 == "absolute number")



##------read results-------
attrBurden3 <- attrBurden %>% 
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" 
         & Region == "United States"
         & Year %in% c(1990, 2016) & method == "di_gee" & measure3 == "value" & scenario == "A"
         & Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All"
         ) %>%
  mutate(mean =  round(mean, 2),
         lower =  round(lower, 2),
         upper =  round(upper, 2))

attrBurden4 <- attrBurden %>% filter(Ethnicity %in% c("White, All Origins") &
                                       Gender.Code == "All genders" & measure1 == "Deaths" &
                                       scenario == "A" 
                                     & measure2 == "age-adjusted rate per 100,000" & Region != "United States"
                                     & Year %in% c(1990,2000) & method == "dii_gee" 
                                     & measure3 == "proportion of disparity to Black or African American attributable")  %>% 
                              select(Region, Year, mean)
attrBurden4 <- attrBurden4 %>%
  group_by(Year) %>%
  mutate(my_ranks = order(order(mean, decreasing=TRUE)))

attrBurden4 <- attrBurden4 %>%
  tidyr::pivot_longer(cols = c("mean","my_ranks")) %>%
  tidyr::pivot_wider(names_from = "Year",
                     values_from = "value")
attrBurden4 <- attrBurden4 %>% 
  filter(name == "my_ranks")

##----higher than average-----
attrBurden5 <- attrBurden %>% filter(method == "dii_gee" & Region == "United States" )
