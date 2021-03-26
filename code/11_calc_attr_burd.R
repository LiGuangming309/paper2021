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
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenDir <- args[12]
attrBurdenDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2011
  agr_by <- "nation"

  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_paf"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/10_attr_burd"
}

totalBurdenDir <- file.path(totalBurdenDir, agr_by)

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv"))

#read some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
#ethn_suppr <- file.path(tmpDir, "ethn_suppr.csv") %>% #TODO delete
#  read.csv() %>%
#  select(Race, Hispanic.Origin, label_cause, factor)
## ----calculations-----
if (!file.exists(attrBurdenDir)) {
  ## ----determine join variables
  join_variables <- c(
    "Year" = "year",
    "Race" = "race",
    "Hispanic.Origin" = "hispanic_origin",
    "label_cause" = "label_cause"
  )
  
  if(agr_by == "nation"){
    join_variables <- c(join_variables,
                        "Gender" = "gender",
                        "Gender.Code" = "gender_label")
  }

  agr_by_replace <- c(
    "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
    "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county" = "County.Code"
  )
  agr_by_new <- agr_by_replace[[agr_by]]
  join_variables[agr_by_new] <- agr_by

  inverse_join_variables <- setNames(names(join_variables), join_variables)
  ## ----- read paf------
  regions <- states[, agr_by] %>% unique()

  pafs <- lapply(regions, function(region) {
    file.path(pafDir, agr_by, year, paste0("paf_", toString(year), "_", region, ".csv")) %>%
      read.csv()
  }) %>%
    do.call(rbind, .) %>%
    # TODO Asian, Pacific Islander immer noch dabei
    filter(!(race %in% c("ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER"))) %>%
    # TODO old people still included
    filter(!(100 <= min_age & max_age < 150 | 100 < min_age)) %>%
    as.data.frame()

  if (agr_by == "STATEFP") {
    possible_regions <- c(1, 4:6, 8:13, 16:42, 44:51, 53:56)
  } else if (agr_by == "Census_Region") {
    pafs$Census_Region <- paste("CENS-R", pafs$Census_Region)
    possible_regions <- paste("CENS-R", 1:4)
  } else if (agr_by == "nation") {
    possible_regions <- "us"
  } else if (agr_by == "Census_division") {
    pafs$Census_division <- paste("CENS-D", pafs$Census_division)
    possible_regions <- paste("CENS-D", 1:9)
  } else if (agr_by == "hhs_region_number") {
    pafs$hhs_region_number <- paste("HHS", pafs$hhs_region_number)
    possible_regions <- paste("HHS", 1:10)
  } else if (agr_by == "county") {
    # pafs$county<-sprintif(%02d,03d,pafs$state,pafs$county)
    # TODO
    pafs$state <- NULL
    possible_regions <- c() # TODO too difficult
  }

  # check for missing stuff
  # missing hispanic origin
  missing <- setdiff(c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins"), pafs$hispanic_origin)
  if (length(missing) > 0) {
    print("Hispanic origins in paf data missing:")
    print(missing)
  }

  # missing races
  missing <- setdiff(c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American"), pafs$race)
  if (length(missing) > 0) {
    print("Races in paf data missing:")
    print(missing)
  }

  # missing regions
  missing <- setdiff(possible_regions, pafs[, agr_by])
  if (length(missing) > 0) {
    print("Regions in paf data missing:")
    print(missing)
  }

  # missing causes
  label_causes_all <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "cvd_stroke")
  missing <- setdiff(label_causes_all, pafs$label_cause)
  if (length(missing) > 0) {
    print("Causes in paf data missing:")
    print(missing)
  }

  if (!(150 %in% pafs$max_age)) print("max_age 150 missing in paf data")

  ## ----- read total burden ---------
  files <- list.files(totalBurdenDir)
  total_burden <- lapply(files, function(file) {
    fileDir <- file.path(totalBurdenDir, file)
    total_burden <- read.delim(fileDir)

    if (agr_by_new == "Nation"){
      columns <- c(unname(inverse_join_variables), "Notes", "Deaths", "Single.Year.Ages", "Single.Year.Ages.Code")
      columns <- columns[columns != "Nation"] # in this case this column does not exist
      
      total_burden <- total_burden%>%
        select(any_of(columns)) %>%
        filter(Single.Year.Ages != "Not Stated")
    }else{
      columns <- c(unname(inverse_join_variables), "Notes", "Deaths", "Five.Year.Age.Groups", "Five.Year.Age.Groups.Code") 
      total_burden <- total_burden%>%
        select(any_of(columns)) %>%
        filter(Five.Year.Age.Groups != "Not Stated")
    } 

    cause_icd <- total_burden$Notes[grepl("UCD - ICD-10 Codes:", total_burden$Notes, fixed = TRUE)]
    notes_hisp_or <- total_burden$Notes[grepl("Hispanic Origin:", total_burden$Notes, fixed = TRUE)]

    total_burden <- total_burden %>% subset(select = -Notes)
    total_burden <- total_burden[!apply(is.na(total_burden) | total_burden == "", 1, all), ]

    if(rlang::is_empty(cause_icd)){
      total_burden$label_cause <- "all-cause"
    }else{
      if (grepl("I20-I25 (Ischaemic heart diseases)", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "cvd_ihd"
      } else if (grepl("J41.0 (Simple chronic bronchitis); J41.1 (Mucopurulent chronic bronchitis); J41.8 (Mixed simple and", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "resp_copd"
      } else if (grepl("E11.0 (Non-insulin-dependent diabetes mellitus, with coma); E11.1 (Non-insulin-dependent diabetes mellitus", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "t2_dm"
      } else if (grepl("G45.0 (Vertebro-basilar artery syndrome); G45.1 (Carotid artery syndrome (hemispheric)); G45.2 (Multiple and", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "cvd_stroke"
      } else if (grepl("A48.1 (Legionnaires disease); A70 (Chlamydia psittaci infection); B34.2 (Coronavirus infection,", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "lri"
      } #else if (grepl("C33 (Malignant neoplasm of trachea); C44.0 (Skin of lip - Malignant neoplasms); C44.1 (Skin of eyelid,", cause_icd, fixed = TRUE)) {
      else if (grepl("C33 (Malignant neoplasm of trachea); C34.0 (Main bronchus - Malignant neoplasms); C34.1 (Upper lobe,", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "neo_lung"
      } else {
        print("unidentifiable ICD Code")
      }
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
  })

  total_burden <- total_burden %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    filter(
      Year == year,
      Hispanic.Origin != "Not Stated"
    ) 

  #parse age groups
  if("Single.Year.Ages.Code" %in% colnames(total_burden)){
    total_burden <- total_burden %>%
      mutate(
        min_age = as.numeric(Single.Year.Ages.Code),
        max_age = as.numeric(Single.Year.Ages.Code)#,
        #Single.Year.Ages.Code = NULL
      )
  }else if("Five.Year.Age.Groups.Code" %in% colnames(total_burden)){
    total_burden <- total_burden %>%
      mutate(
        min_age = Five.Year.Age.Groups.Code %>% sapply(function(a) {
          # extract first number in String
          str_extract(a, "[:digit:]+") %>%
            as.numeric()
        }),
        max_age = Five.Year.Age.Groups.Code %>% sapply(function(a) {
          # if includes "+", max_age = 150, since humans do not get older
          if (grepl("\\+", a)) {
            return(150)
          }
          # otherwise extracts last number in String
          last_num <- str_extract_all(a, "[:digit:]+") %>%
            unlist() %>%
            tail(1) %>%
            as.numeric()
        })#,
        #Five.Year.Age.Groups.Code = NULL
      )
  }
  # ---------check for missing stuff----------------
  # missing Genders
  #missing <- setdiff(c("Male", "Female"), total_burden$Gender)
  #if (length(missing) > 0) {
  #  print("Genders in total burden data missing:")
  #  print(missing)
  #}

  # missing hispanic origin
  missing <- setdiff(c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins"), total_burden$Hispanic.Origin)
  if (length(missing) > 0) {
    print("Hispanic origins in total burden data missing:")
    print(missing)
  }

  # missing races
  missing <- setdiff(c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American"), total_burden$Race)
  if (length(missing) > 0) {
    print("Races in total burden data missing:")
    print(missing)
  }


  missing <- setdiff(possible_regions, total_burden[, agr_by_new])
  if (length(missing) > 0) {
    print("Regions in total burden paf data missing:")
    print(missing)
  }

  # missing causes
  label_causes_all <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "cvd_stroke", "all-cause")
  missing <- setdiff(label_causes_all, total_burden$label_cause)
  if (length(missing) > 0) {
    print("Causes in total burden data missing:")
    print(missing)
  }

  # give some feedback on what is still missing
  # one side
  missing_rows <- anti_join(filter(total_burden, label_cause != "all-cause") , pafs, by = join_variables)
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in pafs data for", agr_by, ":"))
    print(head(missing_rows))
  }

  # other side
  missing_rows <- anti_join(pafs, total_burden, by = inverse_join_variables) %>%
    select(gender, race, hispanic_origin) %>%
    distinct()
  if (nrow(missing_rows) > 0) {
    print(paste(nrow(missing_rows), "rows are still missing in total burden data for", agr_by, ":"))
    print(head(missing_rows))
  }
  # analyse suppression
  nrow_suppressed <- nrow(total_burden)
  total_burden <- total_burden %>% filter(Deaths != "Suppressed")
  print(paste(100*(1-nrow(total_burden)/nrow_suppressed), "% rows suppressed"))
  total_burden <- total_burden %>% mutate(Deaths = as.numeric(Deaths))
  
  ##--- calculate YLL-----
  total_burden$measure <- "Deaths"
  total_burden <- total_burden %>% dplyr::rename(value = Deaths)
  
  total_burden_yll <- total_burden %>%
    dplyr::mutate(
      Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(max_age, lifeExpectancy$Age)], #TODO max_age?
      value = value * Life.Expectancy,
      measure= "YLL",
      Life.Expectancy = NULL 
    )

  total_burden <- rbind(total_burden, total_burden_yll)
  total_burden$attr <- sapply(total_burden$label_cause, function(cause){
    if(cause == "all-cause"){
      "overall"
    }else{
      "total"
    }
  })
  ## ----- join total_burden and pafs-----
  
  total_burden_cause <- total_burden %>% filter(label_cause != "all-cause")
  burden_paf <- inner_join(total_burden_cause, pafs, by = join_variables)

  # filter those, where age in correct interval
  burden_paf <- burden_paf %>%
    filter(
      (min_age.x <= min_age.y & max_age.y <= max_age.x) |
      (min_age.y <= min_age.x & max_age.x <= max_age.y)
    )

  ## ----- calculate attributable burden------

  test_that("09_calc distinct rows", {
    burden_paf_sub <- burden_paf %>%
      select(min_age.x, max_age.y, measure, Gender.Code, Race, Hispanic.Origin, label_cause)

    dub_ind <- duplicated(burden_paf_sub) | duplicated(burden_paf_sub, fromLast = TRUE)
    burden_paf_sub <- burden_paf[dub_ind, ]

    expect_equal(nrow(burden_paf_sub), 0)
  })

  attrBurden <- burden_paf %>%
    mutate(
      #value = value * pafs,
      mean = value* mean,
      lower = value* lower,
      upper = value* upper,
      attr = "attributable",
      value = NULL
    )

  # group "out" ages
  columns <- c(unname(inverse_join_variables), "measure", "attr")
  attrBurden <- attrBurden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(mean = sum(mean),
              lower = sum(lower),
              upper = sum(upper))
  total_burden <- total_burden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(mean = sum(value),
              lower = mean,
              upper = mean)

  attrBurden <- rbind(attrBurden, total_burden)
  # some basic tests
  test_that("09_read burden join2", {
    expect_false(any(is.na(attrBurden)))

    # test that total number of deaths/YLL has not changed
    columns <- unname(inverse_join_variables)
    #comp1 <- total_burden %>%
    #  group_by_at(vars(one_of(columns))) %>%
    #  summarize(Deaths = sum(Deaths), YLL = sum(YLL))

    #comp2 <- attrBurden %>%
    #  group_by_at(vars(one_of(columns))) %>%
    #  summarize(Deaths = sum(Deaths), YLL = sum(YLL))

    #comp3 <- inner_join(comp1, comp2, by = columns)

    #expect_equal(comp3$Deaths.x, comp3$Deaths.x)
    #expect_equal(comp3$YLL.x, comp3$YLL.x)
  })

  fwrite(attrBurden, attrBurdenDir)
}
