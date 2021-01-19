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
  year <- 2001
  dataDir <- "/Users/default/Desktop/paper2021/data"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
}

if (!year %in% 2001:2009) {
  print(paste("can not interpolate census data for", year))
  quit()
}

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")
censDir10_in00 <- file.path(censDir, "2010_in_2000")
dir.create(censDir10_in00, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()
possible_states <- states$STATEFP %>%
  as.character() %>%
  str_pad(., 2, pad = "0")

crosswalk <- read.dta(file.path(dataDir, "crosswalk_2010_2000.dta")) %>%
  select(trtid00, trtid10, weight) %>%
  mutate(state = str_sub(trtid00, 1, 2)) %>%
  filter(state %in% possible_states)


# states, for which 2000 and 2010 still needs to be calculated
missing_statesDir <- file.path(censDir10_in00, "missing_states.csv")
if (!file.exists(missing_statesDir)) write.csv(states, missing_statesDir)
missing_states <- read.csv(missing_statesDir)

## -----pair meta data from 2000 and 2010 -----
meta_crosswalkDir <- file.path(censDir, "meta", "2000_2010_cross.csv")
if (!file.exists(meta_crosswalkDir)) {
  meta00 <- read.csv(file.path(censDir, "meta", "cens_meta_2000.csv"))
  meta10 <- read.csv(file.path(censDir, "meta", "cens_meta_2010.csv"))

  meta_crosswalk <- full_join(meta00, meta10,
    by = c(
      "gender_label" = "gender_label",
      "race" = "race",
      "hispanic_origin" = "hispanic_origin"
    )
  )

  meta_crosswalk <- meta_crosswalk %>% filter(
    min_age.x <= min_age.y,
    max_age.x >= max_age.y
  )

  meta_crosswalk <- meta_crosswalk %>%
    rename(
      variable00 = variable.x,
      variable10 = variable.y
    ) %>%
    select(variable00, variable10)

  test_that("03 interp meta crosswalk", {
    missing_variables <- setdiff(meta00$variable, meta_crosswalk$variable00)
    expect_true(rlang::is_empty(missing_variables))
    missing_variables <- setdiff(meta10$variable, meta_crosswalk$variable10)
    expect_true(rlang::is_empty(missing_variables))
  })
  fwrite(meta_crosswalk, meta_crosswalkDir)
}
meta_crosswalk <- fread(meta_crosswalkDir)

## -----calculate 2010 data in 2000 boundaries and meta data -----
apply(missing_states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  if (!is.na(STUSPS)) {
    tic(paste("calculated 2010 demographic census data in 2000 boundaries in", name))
    # read demographic census data by tract,
    censData10 <- file.path(censDir10, paste0("census_2010_", STUSPS, ".csv")) %>%
      fread(colClasses = c(pop_size = "numeric")) %>%
      select(GEO_ID, variable, pop_size) %>%
      mutate(GEO_ID = str_pad(GEO_ID, 11, pad = "0"))


    # translate variables
    censData10 <- censData10 %>%
      left_join(meta_crosswalk, by = c("variable" = "variable10")) %>%
      mutate(variable = NULL) %>%
      rename(variable = variable00)

    test <- censData10 %>%
      anti_join(crosswalk, by = c("GEO_ID" = "trtid10")) %>%
      filter(pop_size > 0)
    if (nrow(test) > 0) warning("03_interp missing GEO_IDs")

    # translate tracts
    censData10 <- censData10 %>%
      inner_join(crosswalk, by = c("GEO_ID" = "trtid10")) %>%
      mutate(pop_size = pop_size * weight) %>%
      group_by(trtid00, variable) %>%
      summarise(pop_size = sum(pop_size)) %>%
      rename(GEO_ID = trtid00) %>%
      as.data.frame()

    censData10 <- censData10 %>%
      mutate(state = str_sub(GEO_ID, 1, 2))

    for (statefp in unique(censData10$state)) {
      censData10_sub <- censData10 %>% filter(state == statefp)
      STUSPS_corresponding <- states[states[, "STATEFP"] == as.numeric(statefp), "STUSPS"]

      censDir10_in00X <- file.path(censDir10_in00, paste0("census_2010_", STUSPS_corresponding, ".csv"))

      if (!file.exists(censDir10_in00X)) {
        fwrite(censData10_sub, censDir10_in00X)
      } else {
        write.table(censData10_sub,
          censDir10_in00X,
          sep = ",",
          col.names = !file.exists(censDir10_in00X),
          append = T
        )
      }
    }

    # delete this state from missing_states
    STUSPS_copy <- STUSPS
    missing_statesDir %>%
      read.csv() %>%
      filter(STUSPS != STUSPS_copy) %>%
      write.csv(missing_statesDir)

    toc()
  }
})

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  censData10Dir <- file.path(censDir10_in00, paste0("census_2010_", STUSPS, ".csv"))
  censData10 <- fread(censData10Dir)

  if ("state" %in% colnames(censData10)) {
    tic(paste("aggregated ", name, "by GEO_ID and variable"))
    test_that("basic", {
      expect_equal(1, length(unique(censData10$state)))
    })
    censData10 <- censData10 %>%
      group_by(GEO_ID, variable) %>%
      summarise(pop_size = sum(pop_size))

    fwrite(censData10, censData10Dir)
    toc()
  }
})
## ----give overview over missing GEO_IDs in crosswalk
#missing_GEOIDsDir <- file.path(censDir10_in00, paste0("missing_GEO_ID.csv"))
#if (!file.exists(missing_GEOIDsDir)) {
#  GEOIDs <- apply(states, 1, function(state) {
#    STUSPS <- state["STUSPS"]
#    censData00_GEO <- fread(file.path(censDir00, paste0("census_2000_", STUSPS, ".csv"))) %>%
#      mutate(GEO_ID = GEO_ID %>%
#        as.character() %>%
#        str_pad(., 11, pad = "0")) %>%
#      select(GEO_ID) %>%
#      unlist() %>%
#      unique()

#    return(censData00_GEO)
#  }) %>%
#    unlist()

  # TODO comment
#  missing_GEOIDs <- setdiff(GEOIDs, unique(crosswalk$trtid00))
#  print(paste(length(missing_GEOIDs), "GEO_ID, which are present in the 2000 census data, are not present in the crosswalk files"))
#  write.csv(missing_GEOIDs, missing_GEOIDsDir)

#  missing_GEOIDs2 <- setdiff(unique(crosswalk$trtid00), GEOIDs)
#  write.csv(missing_GEOIDs2, file.path(censDir10_in00, paste0("missing_GEO_ID2.csv")))
#} else {
#  missing_GEOIDs <- fread(missing_GEOIDsDir)
#  missing_GEOIDs2 <- fread(file.path(censDir10_in00, paste0("missing_GEO_ID2.csv")))
#}

## ----- actual interpolation-----
censDirYear <- file.path(censDir, year)
dir.create(censDirYear, recursive = T, showWarnings = F)

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  censDirYearX <- file.path(censDirYear, paste0("census_", toString(year), "_", STUSPS, ".csv"))
  if (!file.exists(censDirYearX)) {
    tic(paste("interpolated data in", year, "in", name))
    censData00 <- fread(file.path(censDir00, paste0("census_2000_", STUSPS, ".csv"))) %>%
      rename(pop_size00 = pop_size)
    censData10 <- fread(file.path(censDir10_in00, paste0("census_2010_", STUSPS, ".csv"))) %>%
      rename(pop_size10 = pop_size)

    censData_joined <- full_join(censData00, censData10, by = c("GEO_ID", "variable")) %>%
      filter(!(pop_size00 == 0 & is.na(pop_size10) |
        is.na(pop_size00) & pop_size10 == 0 |
        is.na(pop_size00) & is.na(pop_size10)))

    # give an overview, how much is missing
    #  missing1 <- censData_joined %>% filter(is.na(pop_size00))

    #  if (nrow(missing1) > 0){
    #    missing1<-missing1%>%
    #      group_by(GEO_ID) %>%
    #      summarise(variables = n(),
    #                pop_size10 = sum(pop_size10)) %>%
    #      mutate(test = GEO_ID %in% missing_GEOIDs2$x) %>%
    #      arrange(desc(pop_size10))

    #    print(paste(nrow(missing1), "GEO_ID worth", sum(missing1$pop_size10), "persons missing in 2000 data in", name))
    #  }

    #  missing2 <- censData_joined %>% filter(is.na(pop_size10))

    #  if (nrow(missing2) > 0){
    #    missing2<-missing2%>%
    #      group_by(GEO_ID) %>%
    #      summarise(variables = n(),
    #                pop_size00 = sum(pop_size00)) %>%
    #      mutate(test = GEO_ID %in% missing_GEOIDs$x) %>%
    #      arrange(desc(pop_size00))

    #  print(paste(nrow(missing2), "GEO_ID worth", sum(missing2$pop_size00), "persons missing in 2010 data in 20000 boundaries in", name))
    # }
    # if (any(is.na(censData_joined))) browser()
    # TODO delete above

    censData_joined <- censData_joined %>%
      mutate(
        pop_size00 = replace_na(pop_size00, 0),
        pop_size10 = replace_na(pop_size10, 0)
      )

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
    t <- (year - 2000) / 10
    censDataYear <- censData_joined %>%
      mutate(pop_size = t * pop_size00 + (1 - t) * pop_size10) %>%
      select(state, county, tract, GEO_ID, variable, pop_size)

    fwrite(censDataYear, censDirYearX)

    # testthat
    test_that("02_interp actual interpolation", {
      expect_false(any(is.na(censDataYear)))

      censData00_agr <- censData00 %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_size00 = sum(pop_size00))

      censData10_agr <- censData10 %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_size10 = sum(pop_size10))

      censDataYear_agr <- censDataYear %>%
        ungroup() %>%
        group_by(variable) %>%
        summarise(pop_sizeYear = sum(pop_size))

      comp4 <- censData00_agr %>%
        full_join(censData10_agr, by = "variable") %>%
        full_join(censDataYear_agr, censData10_agr, by = "variable")

      comp4 <- comp4 %>%
        mutate(
          inInterval =
            dplyr::between(pop_sizeYear, pop_size00, pop_size10) ||
              dplyr::between(pop_sizeYear, pop_size10, pop_size00)
        )

      expect_true(all(comp4$inInterval))
      if (!all(comp4$inInterval)) browser()
    })
    toc()
  }
})
