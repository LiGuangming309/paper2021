#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1] %>% as.numeric()
dataDir <- args[2]
tmpDir <- args[3]
censDir <- args[8]

if (rlang::is_empty(args)) {
  year <- 1991
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # dataDir <- "C:/Users/Daniel/Desktop/paper2021/data"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
}

states <- file.path(tmpDir, "states.csv") %>% read.csv()
censDirTo <- file.path(censDir, year)
dir.create(censDirTo, recursive = T, showWarnings = F)

if (year %in% 2001:2009) {
  year_lower <- 2000
  censDirLower <- file.path(censDir, "2000_in_2010")
  year_upper <- 2010
  censDirUpper <- file.path(censDir, 2010)
  
  metaUpper <- read.csv(file.path(censDir, "meta", "cens_meta_2010.csv")) %>%
    filter(Education == 666)
} else if (year %in% 1991:1999) {
  year_lower <- 1990
  censDirLower <- file.path(censDir, "1990_in_2010")
  year_upper <- 2000
  censDirUpper <- file.path(censDir, "2000_in_2010")
  metaUpper <- read.csv(file.path(censDir, "meta", "cens_meta_2000.csv")) %>%
    filter(Hispanic.Origin == "All Origins")
  
} else {
  print(paste("can not interpolate census data for", year))
  quit()
}


## ---calculation----
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  censDirToX <- file.path(censDirTo, paste0("census_", toString(year), "_", STUSPS, ".csv"))
  bool <- !file.exists(censDirToX) 
  if(file.exists(censDirToX)){
    test <- setdiff(fread(censDirToX)$variable %>% unique,
            metaUpper$variable %>% unique)
    if(length(test) > 0){
      bool <- TRUE
    } 
  }
    
  if (bool) {
    tic(paste("interpolated data in", year, "in", name))
    censDataLower <- fread(file.path(censDirLower, paste0("census_", year_lower, "_", STUSPS, ".csv"))) %>%
      rename(pop_sizeLower = pop_size)
    censDataUpper <- fread(file.path(censDirUpper, paste0("census_", year_upper, "_", STUSPS, ".csv"))) %>%
      rename(pop_sizeUpper = pop_size)
    
    censDataUpper <- censDataUpper %>% filter(variable %in% metaUpper$variable)

    censData_joined <- full_join(censDataLower, censDataUpper, by = c("GEO_ID", "variable")) %>%
      filter(!(pop_sizeLower == 0 & is.na(pop_sizeUpper) |
        is.na(pop_sizeLower) & pop_sizeUpper == 0 |
        is.na(pop_sizeLower) & is.na(pop_sizeUpper)))

    #test_that("02_interp actual interpolation missing variables", {
     # test <- censData_joined %>%
     #   filter(pop_sizeLower > 0 & is.na(pop_sizeUpper) |
     #              is.na(pop_sizeLower) & pop_sizeUpper > 0)
      #test <- test %>%
     #   group_by(GEO_ID) %>%
     #   summarise(pop_sizeUpper = sum(pop_sizeUpper))
     # if(nrow(test)>0){
     #   print(test)
      #}
      #expect_equal(0,nrow(test))
      #if (0 != nrow(test)) browser()
    #})
    
    censData_joined <- censData_joined %>%
      mutate(
        pop_sizeLower = replace_na(pop_sizeLower, 0),
        pop_sizeUpper = replace_na(pop_sizeUpper, 0)
      )
    censData_joined <- as.data.frame(censData_joined)
    if(!"state" %in% colnames(censData_joined)) censData_joined$state <- NA
    if(!"county" %in% colnames(censData_joined)) censData_joined$county <- NA
    if(!"tract" %in% colnames(censData_joined)) censData_joined$tract <- NA
    # fill state/county/tract if NA
    suppressWarnings(
      censData_joined[is.na(censData_joined$state), ] <- censData_joined[is.na(censData_joined$state), ] %>%
        mutate(
          GEO_ID = GEO_ID %>%
            as.character() %>%
            str_pad(., 11, pad = "0"),
          state = str_sub(GEO_ID, 1, 2),
          county = str_sub(GEO_ID, 3, 6),
          tract = str_sub(GEO_ID, 7, 11)
        )
    )

    # convex combination/interpolation
    t <- (year - year_lower) / (year_upper - year_lower)
    censDataTo <- censData_joined %>%
      mutate(pop_size = (1 - t) * pop_sizeLower + t * pop_sizeUpper) %>%
      select(state, county, tract, GEO_ID, variable, pop_size)

    #fwrite(censDataTo, censDirToX)
    suppressWarnings(
      write.table(censDataTo,
                  censDirToX,
                  sep = ",",
                  col.names = !file.exists(censDirToX),
                  append = T,
                  row.names = F
      )
    )

    # testthat
    test_that("02_interp actual interpolation", {
      expect_false(any(is.na(censDataTo)))
      expect_false(any(censDataTo == ""))

      censDataLower_agr <- censDataLower %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_sizeLower = sum(pop_sizeLower))

      censDataUpper_agr <- censDataUpper %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_sizeUpper = sum(pop_sizeUpper))

      censDataTo_agr <- censDataTo %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_sizeYear = sum(pop_size))

      comp4 <- censDataLower_agr %>%
        full_join(censDataUpper_agr, by = "variable") %>%
        full_join(censDataTo_agr, by = "variable")

      comp4 <- comp4 %>%
        mutate(
          inInterval =
            (pop_sizeLower <= pop_sizeYear & pop_sizeYear <= pop_sizeUpper) |
              (pop_sizeUpper <= pop_sizeYear & pop_sizeYear <= pop_sizeLower)
        )

      expect_true(all(comp4$inInterval))
      #if (!all(comp4$inInterval)) browser()
      toc()
    })
    
  }
})
