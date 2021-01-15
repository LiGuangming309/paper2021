#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: read total burden data
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc") # "sets","prob"

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenDir <- args[12]
attrBurdenDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2010
  agr_by <- "nation"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
}

totalBurdenDir <- file.path(totalBurdenDir, agr_by)

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv")) 

if (!file.exists(attrBurdenDir)) {
  ##----determine join variables
  join_variables <- c(
    "Year"="year",
    #"Year.code"="year",
    "Gender" = "gender",
    "Gender.Code" = "gender_label",
    "Race" = "race",
    "Hispanic.Origin" = "hispanic_origin",
    "label_cause" = "label_cause"
  )
  
  agr_by_replace <- c("county" = "County", "Census_Region" = "Census.Region", "Census_division" = "Census.division", "hhs_region_number" = "HHS.Region", "STATEFP" = "State", "nation" = "Nation")
  agr_by_new <- agr_by_replace[[agr_by]]
  join_variables[agr_by_new] <- agr_by
  
  inverse_join_variables<- setNames(names(join_variables), join_variables)
  ## ----- read paf------
  states <- file.path(tmpDir, "states.csv") %>% read.csv()
  regions <- states[, agr_by] %>% unique()
  pafs <- lapply(regions, function(region) {
    file.path(pafDir,agr_by, year, paste0("paf_", toString(year), "_", region, ".csv")) %>%
      read.csv()
  }) %>%
    do.call(rbind, .) %>%
    #TODO Asian, Pacific Islander immer noch dabei
    filter(!(race %in% c("ASIAN","NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER"))) %>%
    #TODO old people still included
    filter(!(100 <= min_age & max_age <150 | 100 < min_age)) %>%
    as.data.frame 

  # Find and replace so it is compatible with other data
  replaces1 <- data.frame(
    from = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO", "all"),
    to = c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins")
  )
  pafs <- FindReplace(data = pafs, Var = "hispanic_origin", replaceData = replaces1, from = "from", to = "to", exact = FALSE)

  replaces2 <- data.frame(
    from = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN OR PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN"),
    to = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American")
  )
  pafs <- DataCombine::FindReplace(data = pafs, Var = "race", replaceData = replaces2, from = "from", to = "to", exact = FALSE)
  
  if(agr_by =="STATEFP"){
    replaces3 <- data.frame(
      from = states$STATEFP,
      to = paste0(states$NAME,sprintf(" (%02d)", states$STATEFP))
    )
  }else if(agr_by =="Census_Region"){
    replaces3 <- data.frame(
      from = 1:4,
      to = c("Census Region 1: Northeast (CENS-R1)","Census Region 2: Midwest (CENS-R2)","Census Region 3: South (CENS-R3)","Census Region 4: West (CENS-R4)")
    )
  }else if(agr_by =="nation"){
    replaces3 <- data.frame(from = "us",to = "us")
  }else if(agr_by =="Census_division"){
    replaces3 <- data.frame(
      from = 1:9,
      to = c("Division 1: New England (CENS-D1)","Division 2: Middle Atlantic (CENS-D2)","Division 3: East North Central (CENS-D3)","Division 4: West North Central (CENS-D4)",
             "Division 5: South Atlantic (CENS-D5)","Division 6: East South Central (CENS-D6)","Division 7: West South Central (CENS-D7)","Division 8: Mountain (CENS-D8)",
             "Division 9: Pacific (CENS-D9)")
    )
  }else if(agr_by =="hhs_region_number"){
    replaces3 <- data.frame(
      from = 1:10,
      to = c("HHS Region #1 CT, ME, MA, NH, RI, VT", "HHS Region #2 NJ, NY","HHS Region #3 DE, DC, MD, PA, VA, WV","HHS Region #4 AL, FL, GA, KY, MS, NC, SC, TN",
             "HHS Region #5 IL, IN, MI, MN, OH, WI", "HHS Region #6 AR, LA, NM, OK, TX","HHS Region #7 IA, KS, MO, NE","HHS Region #8 CO, MT, ND, SD, UT, WY",
             "HHS Region #9 AZ, CA, HI, NV","HHS Region #10 AK, ID, OR, WA")
    )
  }
  #TODO problems for STATEFP
  #pafs <- DataCombine::FindReplace(data = pafs, Var = agr_by, replaceData = replaces3, from = "from", to = "to", exact = FALSE)

  #check for missing stuff
    #missing hispanic origin
    missing<-setdiff(replaces1$to,pafs$hispanic_origin)
    if(length(missing)>0){ 
      print("Hispanic origins in paf data missing:")
      print(missing)
    }
    
    #missing races
    missing<-setdiff(replaces2$to,pafs$race)
    if(length(missing)>0){ 
      print("Races in paf data missing:")
      print(missing)
    }
    
    #missing regions
    missing<-setdiff(replaces3$to,pafs[,agr_by])
    if(length(missing)>0){ 
      print("Regions in paf data missing:")
      #print(missing) #TODO
    }
    
    #missing causes
    label_causes_all<-c("resp_copd", "lri", "neo_lung", "t2_dm","cvd_ihd", "cvd_stroke")
    missing<-setdiff(label_causes_all,pafs$label_cause)
    if(length(missing)>0){ 
      print("Causes in paf data missing:")
      print(missing)
    }
  
  ## ----- read total burden ---------
  files <- list.files(totalBurdenDir)
  total_burden <- lapply(files, function(file) {
    fileDir <- file.path(totalBurdenDir, file)
    
    columns<-c(unname(inverse_join_variables),"Notes", "Single.Year.Ages", "Single.Year.Ages.Code", "Deaths")
    if(agr_by_new == "Nation") columns<-columns[columns!="Nation"] #in this case this column does not exist
    
    total_burden <- read.delim(fileDir) %>%
      select(any_of(columns)) %>%
      filter(Single.Year.Ages != "Not Stated") %>% 
      mutate(
        Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
        YLD = sapply(Single.Year.Ages.Code, function(a) max(0, 75 - a)) # TODO right formula?
      )

    cause_icd <- total_burden$Notes[grepl("UCD - ICD-10 Codes:", total_burden$Notes, fixed = TRUE)]
    notes_hisp_or <- total_burden$Notes[grepl("Hispanic Origin:", total_burden$Notes, fixed = TRUE)]

    total_burden$Notes <- NULL
    total_burden <- total_burden[!apply(is.na(total_burden) | total_burden == "", 1, all), ]

    if (grepl("I20-I25 (Ischaemic heart diseases)", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "cvd_ihd"
    } else if (grepl("J41.0 (Simple chronic bronchitis); J41.1 (Mucopurulent chronic bronchitis); J41.8 (Mixed simple and", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "resp_copd"
    } else if (grepl("E11.0 (Non-insulin-dependent diabetes mellitus, with coma); E11.1 (Non-insulin-dependent diabetes mellitus", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "t2_dm"
    } else if (grepl("G45.0 (Vertebro-basilar artery syndrome); G45.1 (Carotid artery syndrome (hemispheric)); G45.2 (Multiple and", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "cvd_stroke"
    } else if (grepl("A48.0 (Gas gangrene); A48.1 (Legionnaires disease); A48.2 (Nonpneumonic Legionnaires disease [Pontiac", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "lri"
    } else if (grepl("C33 (Malignant neoplasm of trachea); C34.0 (Main bronchus - Malignant neoplasms); C34.1 (Upper lobe,", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "neo_lung"
    }

    if (!"Hispanic.Origin" %in% colnames(total_burden)) {
      if (rlang::is_empty(notes_hisp_or)) {
        total_burden[, "Hispanic.Origin"] <- "All Origins"
      } else if (grepl("Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        total_burden[, "Hispanic.Origin"] <- "Hispanic or Latino"
      } else if (grepl("Not Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        total_burden[, "Hispanic.Origin"] <- "Not Hispanic or Latino"
      }
    }

    if (agr_by == "nation") {
      total_burden[, "Nation"] <- "us"
    } 
    return(total_burden)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame %>% 
    filter(Year == year,
           Hispanic.Origin !="Not Stated")
  
  
  #check for missing stuff
  #missing Genders
  missing<-setdiff(c("Male","Female"),total_burden$Gender)
  if(length(missing)>0){ 
    print("Genders in total burden data missing:")
    print(missing)
  }
  
  #missing hispanic origin
  missing<-setdiff(replaces1$to,total_burden$Hispanic.Origin)
  if(length(missing)>0){ 
    print("Hispanic origins in total burden data missing:")
    print(missing)
  }
  
  #missing races
  missing<-setdiff(replaces2$to,total_burden$Race)
  if(length(missing)>0){ 
    print("Races in total burden data missing:")
    print(missing)
  }
  

  missing<-setdiff(replaces3$to,total_burden[,agr_by_new])
  if(length(missing)>0){ 
    print("Regions in total burden paf data missing:")
    print(missing) 
  }
  
  #missing causes
  label_causes_all<-c("resp_copd", "lri", "neo_lung", "t2_dm","cvd_ihd", "cvd_stroke")
  missing<-setdiff(label_causes_all,total_burden$label_cause)
  if(length(missing)>0){ 
    print("Causes in total burden data missing:")
    print(missing)
  }

  ## ----- join total_burden and pafs-----
  
  # give some feedback on what is still missing
  # one side
  missing_rows <- anti_join(total_burden, pafs, by = join_variables) 
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in pafs data for", agr_by, ":"))
    print(head(missing_rows))
  }

  # other side
  missing_rows <- anti_join(pafs, total_burden, by = inverse_join_variables) %>%
    #following combination rarely occurs
    filter(!(race %in% c("Asian or Pacific Islander", "Black or African American") & hispanic_origin == "Hispanic or Latino" |
               race == "American Indian or Alaska Native")) %>%
    select(gender,race,hispanic_origin)%>%
    distinct
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in total burden data for", agr_by, ":"))
    print(head(missing_rows))
  }

  ##
  burden_paf <- inner_join(total_burden, pafs, by = join_variables)

  # filter those, where age in correct interval
  burden_paf <- burden_paf %>%
    filter(
      (min_age <= Single.Year.Ages.Code & Single.Year.Ages.Code <= max_age) |
        (Single.Year.Ages.Code == 100 & 100 <= min_age)
    )

  ## ----- calculate attributable burden------

  test_that("09_calc distinct rows", {
    burden_paf_sub <- burden_paf %>%
      select(Single.Year.Ages.Code, Gender.Code, Race, Year.Code, Hispanic.Origin, label_cause)

    dub_ind <- duplicated(burden_paf_sub) | duplicated(burden_paf_sub, fromLast = TRUE)
    burden_paf_sub <- burden_paf[dub_ind, ]

    expect_equal(nrow(burden_paf_sub), 0)
  })

  attrBurden <- burden_paf %>%
    mutate(
      attrDeaths = Deaths * pafs,
      attrYLD = YLD * pafs
    ) 
  
  #group "out" Single.Age, since age group more appropriate
  columns <-c(unname(inverse_join_variables), "min_age", "max_age")
  attrBurden_old-attrBurden
  attrBurden <- attrBurden %>% 
    group_by_at(vars(one_of(columns))) %>%
    summarize(
      Deaths = sum(Deaths),
      YLD = sum(YLD),
      attrDeaths = sum(attrDeaths),
      attrYLD = sum(attrYLD)
    )

  # some basic tests
  test_that("09_read burden join2", {
    #TODO group_by agr_by
    comp1 <- total_burden %>%
      group_by(Year.code, Gender, Gender.Code, Race, Hispanic.Origin, Single.Year.Ages,label_cause) %>%
      summarize(
        Deaths = sum(Deaths),
        YLD = sum(YLD)
      )

    comp2 <- attrBurden_old %>%
      group_by(Year.code, Gender, Gender.Code, Race, Hispanic.Origin, Single.Year.Ages,label_cause) %>%
      summarize(
        Deaths = sum(Deaths),
        YLD = sum(YLD)
      )
    
    #TODO compare with totalBurden instead of attrBurden_old

    comp3 <- inner_join(comp1, comp2, by = c("Year", "Gender", "Gender.Code", "Race", "Hispanic.Origin","Single.Year.Ages","label_cause"))%>%
      filter(Deaths.x != Deaths.y)
    expect_equal(nrow(comp3),0)

  })
  fwrite(attrBurden, attrBurdenDir)
}
