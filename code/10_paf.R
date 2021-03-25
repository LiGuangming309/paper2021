#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/18/2020
# Purpose: calculate PAF
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc","testthat", "MALDIquant","ggplot2")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_rrDir <- args[6]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]
pafDir <- args[11]

# TODO löschen
if (rlang::is_empty(args)) {
  year <- 2011
  agr_by <- "nation"
  
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  cens_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
}

#load meta data
census_meta <-  file.path(censDir,"meta", paste0("cens_meta_", toString(year), ".csv")) %>% 
                     read.csv %>%
                    #filter(relevant == TRUE) %>%
                     select(variable,year,gender,gender_label,min_age,max_age,race,hispanic_origin)

# create directories
cens_agrDir <- cens_agrDir %>% file.path(., agr_by, year)
pafDir <- pafDir %>% file.path(., agr_by, year) # TODO drop year, everything in one file
dir.create(pafDir, recursive = T, showWarnings = F)

# load some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv
#example pm exposures
example_exp_rr<-file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv
pm_levels <- example_exp_rr$exposure_spline

### -----calculation
regions <- states[, agr_by] %>% unique
for (region in regions) {
  pafDirX <- paste0("paf_", toString(year), "_", region, ".csv") %>%
    file.path(pafDir, .)

  if (!file.exists(pafDirX)) {
    tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm"))

    #read census data aggregated by pm exposure
    cens_agr <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .) %>%
      read.csv() %>%
      select(variable, pm, prop) 

    #add column, if something from pm_levels missing
    test <- merge(cens_agr$variable%>% unique,
                  pm_levels) %>%
      rename(variable = x,
             pm = y)
    test$prop <- 0
    cens_agr <- rbind(cens_agr, test)
    
    #make wider
    cens_agr <- cens_agr %>%
      mutate(pm = sapply(pm, function(x) pm_levels[match.closest(x, pm_levels)]
      )) %>% 
      pivot_wider(names_from = pm,
                  names_sort = TRUE,
                  values_fn = sum,
                  values_from = prop,
                  values_fill = 0)
    #as matrix
    matrix_cens_agr <- cens_agr %>%
      subset(select=-variable) %>%
      as.matrix
    rownames(matrix_cens_agr) <- cens_agr$variable
    
    censMetaAll <- paste0("cens_meta_", toString(year), ".csv") %>%
      file.path(censDir, "meta", .) %>%
      read.csv()

    #loop over all exp_rr curves
    pafs <- apply(causes_ages, 1, function(cause_age) {
      label_cause <- cause_age[["label_cause"]]
      age_group_idX <- cause_age[["age_group_id"]]
      #tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm")

      exp_rr <- ifelse(age_group_idX == "all ages",
        paste0(label_cause, ".csv"),
        paste0(label_cause, "_", age_group_idX, ".csv")
      ) %>%
        file.path(exp_rrDir, .) %>%
        read.csv

      matrix_exp_rr <- matrix(data = exp_rr$rr,
                              dimnames = list(exp_rr$exposure_spline,
                                              #paste0("draw", 1:ncol(exp_rr))
                                              "draw1"
                                              ))
      
      ifelse(age_group_idX == "all ages",
             censMeta <- censMetaAll,
             censMeta <- censMetaAll %>% filter(age_group_id == as.numeric(age_group_idX))
             )
      
      if(nrow(censMeta)==0){
        return() #TODO
      }
      #subset rows with right age
      matrix_cens_agr_sub <- matrix_cens_agr %>% subset(rownames(.) %in% censMeta$variable)
      
      #apply formular sum(prop * (rr-1))/(1+sum(prop * (rr-1)))
      matrix <- matrix_cens_agr_sub %*% (matrix_exp_rr-1)
      matrix <- apply(matrix, 1:2, function(x) x/(1+x))
      #write to dataframe
      result<-data.frame(
        label_cause = rep(label_cause, nrow(matrix)),
        variable = rownames(matrix),
        pafs =rowMeans(matrix) 
      )
      
      test_that("07_paf sum(props)", {
        expect_false(any(is.na(result)))
      })
      
      return(result)
    }) %>% do.call(rbind, .)

    pafs[, agr_by] <- region
    
    pafs <- left_join(pafs,census_meta, by= "variable")
    
    test_that("07_paf distinct rows", {
      expect_false(any(is.na(pafs)))

      pafs_dis<-pafs %>% distinct(label_cause,year,gender_label,min_age,max_age,race,hispanic_origin)
      expect_equal(nrow(pafs_dis),nrow(pafs)) 
      #TODO löschen
  
    })                        
      
    write.csv(pafs, pafDirX, row.names = FALSE)
    toc()
  }
}
""