#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: aggregate
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis",
  "hrbrthemes"
)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_tracDir <- args[7]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]

# TODO l?schen
if (rlang::is_empty(args)) {
  year <- 2009
  agr_by <- "nation"

  #tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  #exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  #censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  #cens_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"

   tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
   exp_tracDir <- "C:/Users/Daniel/Desktop/paper2021/data/03_exp_tracts"
   censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
   cens_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr"
}
if (!agr_by %in% c("county", "Census_Region", "Census_division", "hhs_region_number", "STATEFP", "nation")) {
  print(paste(agr_by, "is an invalid agr_by argument"))
  quit()
}

cens_agrDirC <- file.path(cens_agrDir, "county", year)
dir.create(cens_agrDirC, recursive = T, showWarnings = F)

cens_agrDir <- file.path(cens_agrDir, agr_by, year)
dir.create(cens_agrDir, recursive = T, showWarnings = F)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
  fread() %>%
  as.data.frame()


## ---- calculate county-------

# calculate first on county level, even if agr_by != "county"
# loop over all states
apply(states, 1, function(state) {
  STUSPS <- state[["STUSPS"]]
  name <- state[["NAME"]]

  cens_agrDirCX <- file.path(cens_agrDirC, paste0("cens_agr_", toString(year), "_", STUSPS, ".csv"))

  # if not calculated for this state yet
  if (!file.exists(cens_agrDirCX)) {
    tic(paste("Aggregated Census data in", name, "in year", year, "by pm and county"))

    # read demographic census data by tract
    trac_censData <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% 
      fread()  %>% 
      mutate(GEO_ID = as.numeric(GEO_ID)) 

    # read pm exposure data by tract
    exp_tracDataDir <- file.path(exp_tracDir, year, paste0("exp_trac_", toString(year), "_", STUSPS, ".csv"))
    if(!file.exists(exp_tracDataDir) & year < 2000 & STUSPS %in% c("AK","HI")) return()
    
    exp_tracData <-fread(exp_tracDataDir) %>% mutate(GEO_ID = as.numeric(GEO_ID)) 
    
    #stylized scenarios
    exp_tracData <- exp_tracData %>% mutate(scenario = "A")
    if(agr_by == "nation")
    exp_tracData <- rbind(exp_tracData,
                          exp_tracData %>% mutate(scenario = "B",
                                                  pm = pmin(pm, 12)))

    # tigris does not provide all tract boundaries
    anti <- anti_join(trac_censData, exp_tracData, by = "GEO_ID") %>% filter(pop_size > 0)

    if (nrow(anti) > 0) {
      anti <- anti %>%
        group_by(GEO_ID) %>%
        summarise(pop_size = sum(pop_size))

      print(paste(nrow(anti), "GEO_ID worth", sum(anti$pop_size), "persons missing in exposure-tract data in", year, "in", name))
      print(anti$GEO_ID)

      test_that("06_aggregate county anti >0", {
        anti2 <- anti_join(exp_tracData, trac_censData, by = "GEO_ID")
        expect_equal(0, nrow(anti2))
      })
    }


    cens_agr <- inner_join(trac_censData,
      exp_tracData,
      by = "GEO_ID"
    ) %>%
      group_by(state, county, variable,scenario, pm) %>%
      # calculate number of persons of exposed to particulare level of exposure,
      # in particulare county by sex, age group, ethinicity, hispanic origin
      summarise(pop_size = sum(pop_size)) %>%
      filter(pop_size != 0)

    cens_agr <- cens_agr %>%
      group_by(state, county, variable, scenario) %>%
      mutate(prop = pop_size / sum(pop_size)) %>%
      ungroup()

    # test, check
    test_that("06_aggregate county", {
      expect_false(any(is.na(cens_agr)))
      expect_false(any(is.na(trac_censData)))
      cens_agr %>%
        group_by(state, county, variable, scenario) %>%
        summarise(sum_prop = sum(prop)) %>%
        apply(1, function(row) {
          expect_equal(1, row[["sum_prop"]] %>% as.numeric())
        })

      # test that population does not change
      comp1 <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>%
        fread() %>%
        filter(!GEO_ID %in% anti$GEO_ID) %>%
        group_by(state, county, variable) %>%
        summarise(pop_size = sum(pop_size))

      comp2 <- cens_agr %>%
        group_by(state, county, variable, scenario) %>%
        summarise(pop_size = sum(pop_size)) %>%
        inner_join(comp1, by = c("state", "county", "variable"))

     # comp2$pop_size.x[is.na(comp2$pop_size.x)] <- 0

      expect_equal(comp2$pop_size.x, comp2$pop_size.y)
      if (any(comp2$pop_size.x != comp2$pop_size.y)){
        comp2 <- comp2 %>% filter(pop_size.x != pop_size.y)
        #browser()
      }
    })
    
    fwrite(cens_agr, cens_agrDirCX)
    toc()
  }
})

## ------ calculate not county -----
# if agr_by != "county", aggregate data from above according to agr_by

if (agr_by != "county") {
  regions <- states[, agr_by] %>% unique()

  for (region in regions) {
    cens_agrDirX <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .)

    if (!file.exists(cens_agrDirX)) {
      tic(paste("Aggregated Census data in", agr_by, region, "in year", year, "by pm"))
      statesX <- states[states[, agr_by] == region, "STUSPS"]

      # rbind all states from this region
      cens_agr <- lapply(statesX, function(STUSPS) {
        cens_agrDir <- file.path(cens_agrDirC, paste0("cens_agr_", toString(year), "_", STUSPS, ".csv"))
        if(!file.exists(cens_agrDir) & year < 2000 & STUSPS %in% c("AK","HI")){#
          return(NULL)
        }  else{
          return(fread(cens_agrDir))
        } 
        
      }) %>%
        rbindlist() %>%
        as.data.frame() 
       
      if(nrow(cens_agr) > 0){
        cens_agr<- cens_agr %>%
          group_by(variable, scenario, pm) %>%
          summarise(pop_size = sum(pop_size))
        
        # add proportions
        cens_agr <- cens_agr %>%
          group_by(variable, scenario) %>%
          mutate(prop = pop_size / sum(pop_size)) %>%
          ungroup()
        
        # test, check
        test_that("06_aggregate agr_by", {
          cens_agr %>%
            group_by(variable, scenario) %>%
            summarise(sum_prop = sum(prop)) %>%
            apply(1, function(row) {
              expect_equal(1, row[["sum_prop"]] %>% as.numeric())
            })
          expect_false(any(is.na(cens_agr)))
        })
        
        # add region
        cens_agr[, agr_by] <- region
        
        write.csv(cens_agr, cens_agrDirX, row.names = FALSE)
      }
    
      toc()
    }
  }
}
""
