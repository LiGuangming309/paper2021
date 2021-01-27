#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
censDir <- args[3]
attrBurdenDir <- args[4]
allBurdenDir <- args[5]
summaryDir <- args[6]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/data/09_attr_burd"
  allBurdenDir <- "/Users/default/Desktop/paper2021/data/10_all_burden"
  summaryDir <- "/Users/default/Desktop/paper2021/data/10_plots"
}

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
allBurdenDir <- file.path(allBurdenDir, agr_by)
summaryDir <- file.path(summaryDir, agr_by)
dir.create(summaryDir, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()

group_variables <- c(
  "Year" = "year",
   "Gender" = "gender",
   "Gender.Code" = "gender_label",
  "Race" = "race",
  "Hispanic.Origin" = "hispanic_origin"
)

#agr_by_replace <- c("county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code", 
#                    "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county"= "County.Code")
#agr_by_new <- agr_by_replace[[agr_by]]
#group_variables[agr_by_new] <- agr_by

inverse_group_variables <- setNames(names(group_variables), group_variables)

if (!file.exists(file.path(summaryDir, "attr_burd.csv"))) {
  ### --------read attributable burden data----------
  files <- list.files(attrBurdenDir)

  attrBurden <- lapply(files, function(file) {
    attrBurden <- file.path(attrBurdenDir, file) %>% read.csv()
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()

  missing <- setdiff(2000:2016, attrBurden$Year)
  if (length(missing) > 0) {
    print("Years missing in attributable burden data:")
    print(missing)
  }

  attrBurden_gr <- attrBurden %>%
    group_by_at(vars(one_of(inverse_group_variables))) %>%
    summarise(
      Deaths = sum(Deaths),
      YLL = sum(YLL),
      attrDeaths = sum(attrDeaths),
      attrYLL = sum(attrYLL)
    ) %>%
    as.data.frame 
  ##---------- read all burden data----------
  files <- list.files(allBurdenDir)
  all_burden <- lapply(files, function(file) {
    fileDir <- file.path(allBurdenDir, file)
    
    columns <- c(unname(inverse_group_variables), "Notes", "Single.Year.Ages", "Single.Year.Ages.Code", "Deaths")
    if (agr_by == "nation") columns <- columns[columns != "Nation"] # in this case this column does not exist
    
    all_burden <- read.delim(fileDir) %>%
      select(any_of(columns)) %>%
      filter(Single.Year.Ages != "Not Stated") 
    
    notes_hisp_or <- all_burden$Notes[grepl("Hispanic Origin:", all_burden$Notes, fixed = TRUE)]
    
    all_burden$Notes <- NULL
    all_burden <- all_burden[!apply(is.na(all_burden) | all_burden == "", 1, all), ]
    
    if (!"Hispanic.Origin" %in% colnames(all_burden)) {
      if (rlang::is_empty(notes_hisp_or)) {
        all_burden[, "Hispanic.Origin"] <- "All Origins"
      } else if (grepl("Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        all_burden[, "Hispanic.Origin"] <- "Hispanic or Latino"
      } else if (grepl("Not Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
        all_burden[, "Hispanic.Origin"] <- "Not Hispanic or Latino"
      }
    }
    
    if (agr_by == "nation") {
      all_burden[, "Nation"] <- "us"
    }
    return(all_burden)
  })
  
  all_burden <-all_burden %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    filter(
      Hispanic.Origin != "Not Stated"
    )
  
  ###---- analyse suppression ------
  suppressedRows <- sum(all_burden$Deaths == "Suppressed")
  suppressedRowsPerc <- (100*suppressedRows/nrow(all_burden)) %>% round
  print(paste0(suppressedRows," (",suppressedRowsPerc,"%) rows suppressed in all burden data"))
  all_burden <- all_burden %>% filter(Deaths != "Suppressed")
  
  #calculate YLL
  all_burden<-all_burden%>%
    mutate(
      Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
      Deaths = as.numeric(Deaths),
      Life.Expectancy = ifelse(Gender.Code == "M", 80, 82.5),
      YLL = Deaths*(abs(Life.Expectancy - Single.Year.Ages.Code)+(Life.Expectancy - Single.Year.Ages.Code))/2
    )%>%
    rename(allDeaths = Deaths,
           allYLL = YLL)
  
  all_burden <- all_burden %>%
    group_by_at(vars(one_of(group_variables))) %>%
    summarise(allDeaths = sum(allDeaths),
              allYLL=sum(allYLL))
  
  ## --------- read demographic census data -----------
  tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", ")))
  censData_agr <- lapply(unique(attrBurden$Year), function(year) {
    tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", "), "in", year))
    
    censData_agr <- apply(states, 1, function(state) {
      STUSPS <- state["STUSPS"]
      name <- state["NAME"]
      
      censData_agrDir<-file.path(censDir, year, paste0("agr_census_", toString(year), "_", STUSPS, ".csv"))
      if(!file.exists(censData_agrDir)){
        tic(paste("aggregated census data by", paste(inverse_group_variables, collapse = ", "), "in", year, "in", name))
        censData <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% read.csv
        
        censData_agr <- censData %>%
          group_by(variable) %>%
          summarise(pop_size = sum(pop_size))
        
        fwrite(censData_agr,censData_agrDir)
        toc()
      }
      censData_agr <-fread(censData_agrDir)
      return(censData_agr)
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame
    
    meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
    censData_agr<- censData_agr %>%
      left_join(meta, by = "variable")
    
    toc()
    return(censData_agr)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  censData_agr <- censData_agr %>%
    group_by_at(vars(one_of(inverse_group_variables))) %>%
    summarise(pop_size = sum(pop_size))
  
  toc()
  ## ---------------- join/write --------------------
  #join everything
  print("group_variables")
  print(group_variables)
  print("attrBurden_gr")
  print(colnames(attrBurden_gr))
  print("censData_agr")
  print(colnames(censData_agr))
  print("all_burden")
  print(colnames(all_burden))
  
  attrBurden_gr <- left_join(attrBurden_gr, censData_agr, by = group_variables) 
  
  attrBurden_gr<-attrBurden_gr%>%
    left_join(all_burden, by = unname(inverse_group_variables)) #inverse_group_variables
  
  #calculations
  attrBurden_gr<- attrBurden_gr%>%
    mutate(
      # Crude Rates Per 100,000
      crudeDeaths = Deaths * 100000 / pop_size,
      crudeYLL = YLL * 100000 / pop_size,
      crudeAttrDeaths = attrDeaths * 100000 / pop_size, 
      crudeAttrYLL = attrYLL * 100000 / pop_size,
      #effective PAF
      effPaf = attrDeaths / Deaths,
      #proportions
      propDeaths = attrDeaths/allDeaths,
      propYll = attrYLL/allYLL,
      #test
      test1= Deaths/allDeaths,
      test2 = YLL/allYLL,
    )

  test_that("10 plot basic chackes", {
    expect_false(any(is.na(attrBurden_gr)))
  })

  fwrite(attrBurden_gr, file.path(summaryDir, "attr_burd.csv"))
}
attrBurden_gr <- fread(file.path(summaryDir, "attr_burd.csv"))

## ---plot ------
for (his_or in unique(attrBurden_gr$Hispanic.Origin)) {
  attrBurden_gr_his <- attrBurden_gr %>% filter(Hispanic.Origin == his_or)

  for (measure in c("Deaths", "YLL", "attrDeaths", "attrYLL","effPaf","pop_size","crudeDeaths",
                    "crudeYLL","crudeAttrDeaths","crudeAttrYLL","propDeaths","propYll","test1","test2")) {
    g <- attrBurden_gr_his %>%
      ggplot(aes_string(x = "Year", y = measure, group = "Race", color = "Race")) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle(paste("hispanic origin:", his_or)) +
      theme_ipsum() +
      ylab(paste("burden measured in", measure)) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      geom_line()

    ggsave(file.path(summaryDir, paste0(measure, "_", his_or, ".png")),
      plot = g
    )
  }
}
