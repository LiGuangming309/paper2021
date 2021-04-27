#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: read total burden
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

dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
totalBurdenDir <- args[12]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "STATEFP"

  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/08_total_burden"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
}

totalBurdenDir <- file.path(totalBurdenDir, agr_by)
totalBurdenParsedDir <- file.path(totalBurdenParsedDir, agr_by)
dir.create(totalBurdenParsedDir, recursive = T, showWarnings = F)
totalBurdenParsedDir <- file.path(
  totalBurdenParsedDir,
  "total_burden_wond.csv"
)

if (!file.exists(totalBurdenParsedDir)) {
  toc(paste("read cdc wonder burden data by", agr_by))
  ## ----determine join variables
  select_columns <- c("Year", "Race", "Hispanic.Origin","label_cause", "Gender.Code")

  agr_by_replace <- c(
    "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
    "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "nation", "county" = "County.Code"
  )
  agr_by_new <- agr_by_replace[[agr_by]]
  select_columns <- c(select_columns, agr_by_new)

  ## ----- read total burden ---------
  files <- list.files(totalBurdenDir)
  total_burden <- lapply(files, function(file) {
    fileDir <- file.path(totalBurdenDir, file)
    total_burden <- read.delim(fileDir)
    
    cause_icd <- total_burden$Notes[grepl("UCD - ICD-10 Codes:", total_burden$Notes, fixed = TRUE)]
    notes_hisp_or <- total_burden$Notes[grepl("Hispanic Origin:", total_burden$Notes, fixed = TRUE)]
    notes_gender <- total_burden$Notes[grepl("Gender:", total_burden$Notes, fixed = TRUE)]
    notes_race <- total_burden$Notes[grepl("Race:", total_burden$Notes, fixed = TRUE)]
    total_burden$Notes <-NULL
    total_burden <- total_burden[!apply(is.na(total_burden) | total_burden == "", 1, all), ]
    
    if (rlang::is_empty(cause_icd)) {
      total_burden$label_cause <- "all-cause"
    } else {
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
      } # else if (grepl("C33 (Malignant neoplasm of trachea); C44.0 (Skin of lip - Malignant neoplasms); C44.1 (Skin of eyelid,", cause_icd, fixed = TRUE)) {
      else if (grepl("C33 (Malignant neoplasm of trachea); C34.0 (Main bronchus - Malignant neoplasms); C34.1 (Upper lobe,", cause_icd, fixed = TRUE)) {
        total_burden$label_cause <- "neo_lung"
      } else {
        print("unidentifiable ICD Code")
      }
    }

    if (!"Hispanic.Origin" %in% colnames(total_burden)) {
      if (rlang::is_empty(notes_hisp_or)) {
        total_burden$Hispanic.Origin <- "All Origins"
      } else if (notes_hisp_or == "Hispanic Origin: Hispanic or Latino") {
        total_burden$Hispanic.Origin <- "Hispanic or Latino"
      } else if (notes_hisp_or == "Hispanic Origin: Not Hispanic or Latino") {
        total_burden$Hispanic.Origin <- "Not Hispanic or Latino"
      }
    }
    
    if (!"Gender.Code" %in% colnames(total_burden)) {
      if (rlang::is_empty(notes_gender)) {
        total_burden$Gender.Code <- "A"
      }else if (notes_gender == "Gender: Female") {
        total_burden$Gender.Code <- "F"
      } else if (notes_gender == "Gender: Male") {
        total_burden$Gender.Code <- "M"
      }else{
        print(paste("Gender missing in", file))
      }
    }
    
    if (!"Race" %in% colnames(total_burden)) {
      if (notes_race == "Race: White") {
        total_burden$Race <- "White"
      } else if (notes_race == "Race: American Indian or Alaska Native") {
        total_burden$Race <- "American Indian or Alaska Native"
      }else if (notes_race == "Race: Asian or Pacific Islander") {
        total_burden$Race <- "Asian or Pacific Islander"
      }else if (notes_race == "Race: Black or African American") {
        total_burden$Race <- "Black or African American"
      }else{
        print(paste("Race missing in", file))
      }
    }

    if (agr_by == "nation") {
      total_burden$nation <- "us" 
      columns <- c(select_columns, "Deaths", "Single.Year.Ages.Code")
      
      total_burden <- total_burden %>%
        select(all_of(columns)) %>%
        filter(Single.Year.Ages.Code != "NS")
    } else {
      columns <- c(select_columns,"Deaths", "Five.Year.Age.Groups", "Five.Year.Age.Groups.Code")
      total_burden <- total_burden %>%
        select(all_of(columns)) %>%
        filter(Five.Year.Age.Groups.Code != "NS")
      
      total_burden$Five.Year.Age.Groups.Code[total_burden$Five.Year.Age.Groups == "< 1 year"] <- "0-0"
      total_burden$Five.Year.Age.Groups <- NULL
    }
    
    return(total_burden)
  })

  total_burden <- total_burden %>%
    rbindlist %>%
    as.data.frame() %>%
    filter(Hispanic.Origin != "Not Stated")

  # parse age groups
  if ("Single.Year.Ages.Code" %in% colnames(total_burden)) {
    total_burden <- total_burden %>%
      mutate(
        min_age = as.numeric(Single.Year.Ages.Code),
        max_age = as.numeric(Single.Year.Ages.Code),
        Single.Year.Ages.Code = NULL
      )
  } else if ("Five.Year.Age.Groups.Code" %in% colnames(total_burden)) {
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
        }),
        Five.Year.Age.Groups.Code = NULL
      )
  }
  # ---------check for missing stuff----------------
  # missing Genders
  # missing <- setdiff(c("Male", "Female"), total_burden$Gender)
  # if (length(missing) > 0) {
  #  print("Genders in total burden data missing:")
  #  print(missing)
  # }

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

  # analyse suppression
  nrow_suppressed <- nrow(total_burden)
  total_burden <- total_burden %>% filter(Deaths != "Suppressed")
  print(paste(100 * (1 - nrow(total_burden) / nrow_suppressed), "% rows suppressed"))
  total_burden <- total_burden %>% mutate(Deaths = as.numeric(Deaths))

  ##--- seperate, filter ----
  if(!"A" %in% total_burden$Gender.Code){
    #add Gender A
    total_burden_all_gend <- total_burden %>%
      filter(Gender.Code != "A")
    group_by_at(setdiff(colnames(total_burden), c("Gender.Code", "Deaths"))) %>%
      summarise(Deaths = sum(Deaths)) %>%
      mutate(Gender.Code = "A")
    total_burden <- rbind(total_burden, total_burden_all_gend) %>% distinct()
    rm(total_burden_all_gend)
  }
  
  total_burden <- total_burden %>%
    mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
    filter(Ethnicity %in% c(
      "White, Not Hispanic or Latino",
      "White, Hispanic or Latino",
      "Black or African American, All Origins",
      "Asian or Pacific Islander, All Origins",
      "American Indian or Alaska Native, All Origins",
      "All, All Origins"
    ))  %>%
    mutate(Ethnicity = NULL)
  ## --write to csv----
  locations <- total_burden[,agr_by_new]
  total_burden[,agr_by_new] <- NULL
  total_burden[,agr_by] <- locations
  
  total_burden <- total_burden %>% tibble::add_column(Education = 666)
  total_burden$attr <- sapply(total_burden$label_cause, function(cause) {
    if (cause == "all-cause") {
      "overall"
    } else {
      "total"
    }
  })
  
  test_that("basic check", {
    expect_false(any(is.na(total_burden)))
    total_burden_test <- total_burden %>% select(setdiff(colnames(total_burden),c("Deaths")))
    total_burden_test <- total_burden_test[duplicated(total_burden_test), ]
    expect_equal(nrow(total_burden_test), 0)
  })
  total_burden <- total_burden %>% tibble::add_column(source = "wonder")
  fwrite(total_burden, totalBurdenParsedDir)
  toc()
}
