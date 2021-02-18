#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 15/02/2021
# Purpose: assign PM exposure to each census tract in Hawaii and Alaska
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing

packages <- c("data.table", "plyr", "magrittr", "testthat", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc", "units")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]
exp_tracDir <- args[7]
openaq.script <- args[14] # TODO warum 16?

if (rlang::is_empty(args)) {
  year <- 2009
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  expDir <- "/Users/default/Desktop/paper2021/data/01_exposure"
  tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  openaq.script <- "/Users/default/Desktop/paper2021/code/07_openaq.R"
}

expDir <- file.path(expDir, "openaq")
dir.create(expDir, recursive = T, showWarnings = F)

tracts_locationsDir <- file.path(exp_tracDir, "openaq_tmp")
dir.create(tracts_locationsDir, recursive = T, showWarnings = F)

exp_tracDir <- file.path(exp_tracDir, toString(year))
dir.create(exp_tracDir, recursive = T, showWarnings = F)

exposure_locationsDir <- file.path(tmpDir, "openaq_locations.csv")
## ---------------load data---------------
# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  filter(STUSPS %in% c("AK", "HI"))

source(openaq.script)

## -----------------get locations location---------------
if (!file.exists(exposure_locationsDir)) {
  tic("Downloaded all locations in US and CA in OpenAq")
  urlAQ <- paste0(base_url(), "locations")
  # get locations
  argsList <- list(country = "US", country = "CA", limit = 20000, page = 1)
  exposure_locations <- getResults(urlAQ, argsList) %>%
    select(id, name, country, parameters, firstUpdated, lastUpdated, city, latitude, longitude) %>%
    filter(!is.na(longitude) & !is.na(latitude))

  # check that location measures pm2.5
  exposure_locations <- exposure_locations %>%
    mutate(measures_pm25 = sapply(parameters, function(paramter) {
      paramter %>%
        select(parameter) %>%
        unlist() %>%
        is.element("pm25", .)
    })) %>%
    filter(measures_pm25) %>%
    mutate(measures_pm25 = NULL) %>%
    select(id, name, country, longitude, latitude) %>%
    as.data.frame()

  write.csv(exposure_locations, exposure_locationsDir, row.names = FALSE)
  toc()
}

exposure_locations <- read.csv(exposure_locationsDir)
exposure_locations <- st_as_sf(exposure_locations,
  coords = c("longitude", "latitude"),
  crs = 4326,
  agr = "constant"
)

##### ------------assign measurement location to tracts--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  tracts_locationsDirX <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) 
  # quit execution, if already calculated
  if (file.exists(tracts_locationsDirX)) {
    return()
  }

  tic(paste("assigned measurement location to all tracts in", name, "in", toString(year)))
  # load shape files
  tracts <- file.path(tracDir, toString(year), paste0("tracts_", toString(year), "_", STUSPS, ".rds")) %>%
    readRDS()

  exposure_locations2 <- lapply(tracts$geometry, function(geometry) {
    # subset points, which are inside of the tract
    suppressMessages(points_in_tract <- exposure_locations[geometry, , op = st_within])

    # if there are points inside of the tract, the tract is assigned the mean of pm of those points
    # if there are none, the pm of the closest poin
    if (nrow(points_in_tract) > 0) {
      df <- data.frame(
        location_ids = I(list(points_in_tract$id)),
        distance = 0
      )
      return(df)
    } else {
      tract_centroid <- geometry %>% st_centroid()
      tract_centroid <- data.frame(
        longitude = tract_centroid[1],
        latitude = tract_centroid[2]
      ) %>%
        st_as_sf(
          coords = c("longitude", "latitude"),
          crs = st_crs(exposure_locations),
          agr = "constant"
        )

      # plot(geometry)
      # plot(tract_centroid, add = TRUE, pch = 3, col = 'red')
      exposure_locations$dist <- st_distance(x = exposure_locations, y = tract_centroid) %>%
        set_units(1, "km")

      closest_measurement <- exposure_locations[which.min(exposure_locations$dist), ] %>% as.list()

      exposure_locations$dist <- NULL
      df <- data.frame(
        location_ids = closest_measurement$id,
        distance = closest_measurement$dist
      )
      return(df)
    }
  }) %>% do.call(rbind, .)

  tracts_locations <- cbind(tracts, exposure_locations2) %>%
    as.data.frame() %>%
    select(GEO_ID, location_ids, distance)

  # save as csv
  fwrite(tracts_locations, tracts_locationsDirX, row.names = FALSE)
  toc()
})
##### ------------get pm data--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  expDirX <- file.path(expDir, paste0("exp_", toString(year), "_", STUSPS, ".csv"))

  # quit execution, if already calculated
  if (file.exists(expDirX)) {
    return()
  }

  tic(paste("downloaded PM exposure in", name, "in", toString(year)))
  tracts_locations <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>% fread()

  location_ids <- sapply(tracts_locations$location_ids, function(id) {
    id %>%
      toString() %>%
      strsplit(., "|", fixed = TRUE) %>%
      unlist()
  }) %>%
    unlist() %>%
    unique() %>%
    as.list()

  names(location_ids) <- rep_along(location_ids, "location")

  argsList <- list(
    limit = 10000,
    page = 1,
    parameter_id = 2,
    temporal = "year",
    spatial = "location"
  )
  argsList <- c(argsList, location_ids)

  urlAQ <- paste0(base_url(), "averages")
  exposure <- getResults(urlAQ, argsList) %>%
    filter(parameter == "pm25") %>%
    group_by(id) %>%
    arrange(year) %>%
    summarise(
      id = first(id),
      name = first(name),
      year = first(year),
      average = first(average),
      subtitle = first(subtitle),
      measurement_count = first(measurement_count)
    )

  write.csv(exposure, expDirX, row.names = FALSE)
  toc()
})
##### ------------assign annual pm to tracts--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  exp_tracDirX <- file.path(exp_tracDir, paste0("exp_trac_", toString(year), "_", STUSPS, ".csv"))

  # quit execution, if already calculated
  if (file.exists(exp_tracDirX)) {
    return()
  }

  tic(paste("assigned PM exposure to all tracts in", name, "in", toString(year)))
  tracts_locations <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>% fread() #TODO
  exposure <- file.path(expDir, paste0("exp_", toString(year), "_", STUSPS, ".csv")) %>% fread()

  tracts_locations$pm <- apply(tracts_locations, 1, function(tract) {
    location_ids <- tract[["location_ids"]] %>%
      toString() %>%
      strsplit(., "|", fixed = TRUE) %>%
      unlist()

    exposure_sub <- exposure %>% filter(name %in% location_ids)
    pm <- mean(exposure_sub$average, na.rm = TRUE) %>% round(digits = 2)

    return(pm)
  })
  test_that("08_ass_trac_AKHI.R basic check", {
    expect_false(any(is.na(tracts_locations)))
    })
  
  ## -----save as csv--------
  tract_exposure <- tracts_locations %>%
    as.data.frame() %>%
    select(c("GEO_ID", "pm"))

  write.csv(tract_exposure, exp_tracDirX, row.names = FALSE)
  toc()
})
