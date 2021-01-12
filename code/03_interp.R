#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr","tidyr", "testthat","magrittr","stringr","data.table","tictoc","foreign")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
dataDir <- args[2]
tmpDir<-args[3]
censDir <- args[8]

#year<-2001
#dataDir <- "/Users/default/Desktop/paper2020/data"
#tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
#censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

if (!year %in% 2001:2009) {
  print(paste("can not interpolate census data for", year))
  quit()
}

states <- file.path(tmpDir, "states.csv") %>% read.csv

crosswalk <- read.dta(file.path(dataDir,"crosswalk_2010_2000.dta"))%>%
  select(trtid00,trtid10,weight)#%>%
  #filter(weight != 0)

#crosswalk <- file.path(tmpDir,"crosswalk_2000_2010.csv") %>% 
#read.table(., header=TRUE) %>%
#  setnames("trtid00.","trtid00") %>%
#  mutate(trtid00= str_sub(trtid00,1,11))

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")

##-----pair meta data from 2000 and 2010 -----
meta_crosswalkDir <- file.path(censDir,"meta","2000_2010_cross.csv")
if(!file.exists(meta_crosswalkDir)){
  meta00 <-read.csv(file.path(censDir,"meta","cens_meta_2000.csv"))
  meta10 <-read.csv(file.path(censDir,"meta","cens_meta_2010.csv"))
  
  meta_crosswalk <-full_join(meta00,meta10,
                             by=c("gender_label"="gender_label",
                                  "race"="race",
                                  "hispanic_origin"="hispanic_origin"
                             ))
  
  meta_crosswalk <-meta_crosswalk %>%filter(
    min_age.x <= min_age.y,
    max_age.x >= max_age.y  
  )  
  
  meta_crosswalk<-meta_crosswalk%>%
    rename(variable00 =variable.x,
           variable10 =variable.y)%>%
    select(variable00,variable10)
  
  fwrite(meta_crosswalk,meta_crosswalkDir)
}
meta_crosswalk<-fread(meta_crosswalkDir)

##-----calculate 2010 data in 2000 boundaries and meta data -----
censDir10_in00<-file.path(censDir, "2010_in_2000")
dir.create(censDir10_in00, recursive = T, showWarnings = F)

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  censDir10_in00X <- file.path(censDir10_in00, paste0("census_2010_", STUSPS, ".csv"))
  if(!file.exists(censDir10_in00X)){
    tic(paste("calculated 2010 demographic census data in 2000 boundaries in",name))
    #read demographic census data by tract,
    censData10 <-  file.path(censDir10, paste0("census_2010_", STUSPS, ".csv"))%>%
      fread(colClasses=c(pop_size="numeric")) %>%
      mutate(GEO_ID = str_pad(GEO_ID, 11, pad = "0"))
    
    #translate variables
    censData10 <- censData10 %>%
      left_join(meta_crosswalk, by=c("variable"="variable10"))%>%
      mutate(variable = NULL)%>%
      rename(variable =variable00)
    
    #TODO löschen
    censData10_copy <-censData10
    
    #TODO popsize negative
    #translate tracts
    censData10 <- censData10 %>% 
      inner_join(crosswalk, by=c("GEO_ID"="trtid10")) %>% #TODO left_join 
      mutate(pop_size= pop_size*weight)%>%
      group_by(trtid00,variable)%>%
      summarise(pop_size = sum(pop_size))%>%
      rename(GEO_ID=trtid00)
      as.data.frame
    
      censData10 <- censData10 %>% mutate(
        #TODO 
        state = str_sub(GEO_ID,1,2),
        county =str_sub(GEO_ID,3,6),
        tract =str_sub(GEO_ID,7,11)
      )
      
    #testthat after GEO_ID crosswalk same population size
      test_that("02_interp 2010 in 2000 boundaries", {
        comp1<-censData10_copy %>%
          ungroup%>% 
          group_by(variable)%>%
          summarise(pop_size = sum(pop_size)) 
        
        comp2<-censData10 %>%
          ungroup%>% 
          group_by(variable)%>%
          summarise(pop_size = sum(pop_size))
        
        comp3 <- full_join(comp1,comp2, by ="variable")
        
        apply(comp3,1,function(row){
          expect_equal(row[["pop_size.x"]],row[["pop_size.y"]], tolerance=1)
        }
        )
      })
    
    fwrite(censData10,censDir10_in00X)
    toc()
  }
  })
##----- actual interpolation-----
censDirYear<-file.path(censDir, year)
dir.create(censDirYear, recursive = T, showWarnings = F)

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  censDirYearX <- file.path(censDirYear, paste0("census_",toString(year),"_", STUSPS, ".csv"))
  if(!file.exists(censDirYearX)){
    #TODO something wrong
    censData00<-fread(file.path(censDir00, paste0("census_2000_", STUSPS, ".csv")))%>%
      rename(pop_size00 = pop_size)
    censData10<-fread(file.path(censDir10_in00, paste0("census_2010_", STUSPS, ".csv")))%>%
      rename(pop_size10 = pop_size)
    
    
    censData_joined <-full_join(censData00,censData10, 
                                by=c("GEO_ID"="GEO_ID","variable"="variable"))
    
    test_that("03_interp censData_joined",{
      expect_false(any(is.na(censData_joined)))
    })
     
    #censData_joined<-censData_joined%>% 
    #        mutate(pop_size00=replace_na(pop_size00, 0),
    #               pop_size10=replace_na(pop_size10, 0))
    
    t<-(year-2000)/10
    censDataYear<-censData_joined %>%
                    mutate(pop_size = t*pop_size00+(1-t)*pop_size10)%>%
                    select(state.x,county.x,tract.x,GEO_ID,variable,pop_size)%>%#TODO
                    rename(state =state.x,
                           county =county.x,
                           tract =tract.x)
    
    fwrite(censDataYear,censDirYearX)
    
    #testthat 
    test_that("02_interp actual interpolation", {
      censData00_agr<-censData00 %>%
        ungroup%>% 
        group_by(variable)%>%
        summarise(pop_size00 = sum(pop_size00)) 
      
      censData10_agr<-censData10 %>%
        ungroup%>% 
        group_by(variable)%>%
        summarise(pop_size10 = sum(pop_size10))
      
      censDataYear_agr<-censDataYear %>%
        ungroup%>% 
        group_by(variable)%>%
        summarise(pop_sizeYear = sum(pop_size))
      
      comp4 <- censData00_agr %>%
        full_join(censData10_agr, by= "variable")%>%
        full_join(censDataYear_agr,censData10_agr, by= "variable")
      
      comp4<-comp4 %>% 
              mutate(inInterval=
                dplyr::between(pop_sizeYear,pop_size00,pop_size10)||
                dplyr::between(pop_sizeYear,pop_size10,pop_size00)
                    )
      
        expect_true(all(comp4$inInterval))
      
    })
  }
})