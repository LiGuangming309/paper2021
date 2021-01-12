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
packages <- c("dplyr", "magrittr", "data.table", "testthat","tidyverse", "tictoc","viridis",
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
#year <- 2010
#agr_by <- "nation"

# tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
# exp_tracDir <- "/Users/default/Desktop/paper2020/data/03_exp_tracts"
# censDir <- "/Users/default/Desktop/paper2020/data/06_demog"
# cens_agrDir <- "/Users/default/Desktop/paper2020/data/07_dem.agr"

#tmpDir <- "C:/Users/Daniel/Desktop/paper2020/data/tmp"
#exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"
#censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
#cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"

if (!agr_by %in% c("county", "Census_Region", "Census_division", "hhs_region_number", "state", "nation")) {
  print(paste(agr_by, "is an invalid agr_by argument"))
  quit()
}

cens_agrDirC <-  file.path(cens_agrDir, "county", year)
dir.create(cens_agrDirC, recursive = T, showWarnings = F)

cens_agrDir <-  file.path(cens_agrDir, agr_by, year)
dir.create(cens_agrDir, recursive = T, showWarnings = F)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv


## ---- calculate county-------

#calculate first on county level, even if agr_by != "county"
#loop over all states
apply(states, 1, function(state) {
  STUSPS <- state[["STUSPS"]] 
  name <- state[["NAME"]]
  
  cens_agrDirCX <- file.path(cens_agrDirC,paste0("cens_agr_", toString(year), "_", STUSPS, ".csv") )
  
  #if not calculated for this state yet
  if (!file.exists(cens_agrDirCX)) {
    tic(paste("Aggregated Census data in", name, "in year", year, "by pm and county"))
    
    #read demographic census data by tract, make data wider
    trac_censData <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
      file.path(censDir, year, .)%>%
      read.csv %>%
      #setnames("GEO_ID", "AFFGEOID") %>%
      pivot_wider(
        names_from = variable,
        values_from = pop_size
      ) 
    
    #read pm exposure data by tract
    exp_tracData <- file.path(exp_tracDir, year, paste0("exp_trac_", toString(year), "_", STUSPS, ".csv")) %>%
      read.csv
    
    #tigris does not provide all tract boundaries
    sym_dif<-sets::set_symdiff(exp_tracData$GEO_ID,trac_censData$GEO_ID) %>% unlist
    if (!is.null(sym_dif)){
      trac_censData_sub <- trac_censData %>% 
        filter(GEO_ID %in% sym_dif) 
      
      trac_censData_sub<-trac_censData_sub%>%
        mutate(rowsum=rowSums(trac_censData_sub[,-c(1:4)])) %>%
        filter(rowsum >0)
      if(nrow(trac_censData_sub)>0){
        print(paste("In",name, "exp_tracData and trac_censData differ by",nrow(trac_censData_sub),"rows:"))
        glimpse(trac_censData_sub$GEO_ID)
        #browser()
      }
    } 
    
    #join above datasets
    cens_agr <- inner_join(trac_censData,
                          exp_tracData,
                          by = "GEO_ID" 
    )  %>%
      #make long again
      setDT %>%
      melt(
        id.vars = c("state", "county", "tract", "GEO_ID", "pm"),
        variable.name = "variable"
      ) %>%
      group_by(state, county, variable, pm) %>%
      #calculate number of persons of exposed to particulare level of exposure,
      #in particulare county by sex, age group, ethinicity, hispanic origin
      summarise(pop_size = sum(value)) %>%
      filter(pop_size != 0)
    
    cens_agr <- cens_agr  %>%
      group_by(state, county, variable) %>%
      #calculate marginal sum regardsless of exposure
      summarise(totals = sum(pop_size)) %>%
      filter(totals != 0)  %>%
      inner_join(cens_agr, by = c("state", "county", "variable")) %>%
      #calculate proportion this way
      mutate(prop = pop_size / totals)
    
    #test, check 
    test_that("06_aggregate county", {
      cens_agr %>%
        group_by(state, county, variable) %>%
        summarise(sum_prop = sum(prop)) %>%
        apply(1,function(row){
          expect_equal(1,row[["sum_prop"]] %>% as.numeric)
        })
      expect_equal(any(is.na(cens_agr)), FALSE)
    })
    
    write.csv(cens_agr, cens_agrDirCX)
    toc()
  }
})

##------ calculate not county -----
# if agr_by != "county", aggregate data from above according to agr_by

if (agr_by != "county") {
  regions <- states[, agr_by] %>% unique
  
  for (region in regions) {
    
    cens_agrDirX <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .)
    
    if (!file.exists(cens_agrDirX)) { 
      tic(paste("Aggregated Census data in",agr_by, region, "in year", year, "by pm"))
      statesX <- states[states[, agr_by] == region, "STUSPS"]
      
      #rbind all states from this region
      cens_agr <- lapply(statesX, function(STUSPS) {
        paste0("cens_agr_", toString(year), "_", STUSPS, ".csv") %>%
          file.path(cens_agrDirC, .) %>%
          read.csv 
      }) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        group_by(variable, pm) %>%
        summarise(pop_size = sum(pop_size))
      
      # add proportions
      cens_agr <- cens_agr %>%
        group_by(variable) %>%
        summarise(totals = sum(pop_size)) %>%
        inner_join(cens_agr, by = "variable") %>%
        mutate(prop = pop_size / totals)
      
      #test, check 
      test_that("06_aggregate agr_by", {
        cens_agr %>%
          group_by(variable) %>%
          summarise(sum_prop = sum(prop)) %>%
          apply(1,function(row){
            expect_equal(1,row[["sum_prop"]] %>% as.numeric)
          })
        expect_false(any(is.na(cens_agr)))
      })
      
      # add region
      cens_agr[, agr_by] <- region
      
      write.csv(cens_agr, cens_agrDirX)
      toc()
    }
#---- -----Plot-----------    
    if(TRUE){
      census_meta <-  file.path(censDir,"meta", paste0("cens_meta_", toString(year), ".csv")) %>% read.csv
      
      cens_agr_plotDir <- file.path(cens_agrDir, "plots")
      if (!file.exists(cens_agr_plotDir)) { 
        tic(paste("Plotted aggregated Census data in",agr_by, region, "in year", year, "by pm"))
        dir.create(cens_agr_plotDir, recursive = TRUE)
        cens_agr <- cens_agrDirX %>% 
                      read.csv %>%
                      left_join(.,census_meta, by = "variable") %>%
                      group_by(race,hispanic_origin, pm) %>%
                      summarise(pop_size = sum(pop_size))
        
        #print total pop sizes, regardless of pm
        cens_agr %>%
          group_by(race,hispanic_origin) %>%
          summarise(pop_size = sum(pop_size))%>%
          write.csv(.,file.path(cens_agr_plotDir,"total_pop.csv"))
        
        #seperate plot for all his or.
        for(his_or in unique(cens_agr$hispanic_origin)){
          cens_agr_his <- cens_agr %>% filter(hispanic_origin == his_or)
          
          #totals
          g<-cens_agr_his %>%
            ggplot(aes(x=pm, y=pop_size, group=race, color=race)) +
            scale_color_viridis(discrete = TRUE) +
            ggtitle(paste("hispanic origin:",his_or,"year:",year)) +
            theme_ipsum() +
            ylab("Number of persons exposed")+
            xlab("particulate matter (pm)")+
            ylim(0,NA)
          
          ggsave(file.path(cens_agr_plotDir,paste0(region,"_",his_or,"_total.png")),
                 plot = g +geom_line())
          
          #scale down/proportion
          # proportions
          cens_agr_his <- cens_agr_his %>%
            group_by(race) %>%
            summarise(totals = sum(pop_size)) %>%
            inner_join(cens_agr_his, by = "race") %>%
            mutate(prop = pop_size / totals)
          
          test_that("06_aggregate plot race", {
            cens_agr_his %>%
              group_by(race) %>%
              summarise(sum_prop = sum(prop)) %>%
              apply(1,function(row){
                expect_equal(1,row[["sum_prop"]] %>% as.numeric)
              })
            expect_equal(any(is.na(cens_agr_his)), FALSE)
          })
          
          #totals
          g<-cens_agr_his %>%
            ggplot(aes(x=pm, y=prop, group=race, color=race)) +
            scale_color_viridis(discrete = TRUE) +
            ggtitle(paste("hispanic origin:",his_or)) +
            theme_ipsum() +
            ylab("Proportion of persons exposed")+
            xlab("particulate matter (pm)")+
            ylim(0,NA)
          
          ggsave(file.path(cens_agr_plotDir,paste0(region,"_",his_or,"_prop.png")),
                 plot = g +geom_line())
        }
        toc()
      }
      
    }
  }
}
""