
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

packages <- c("plyr", "magrittr", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc")

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
tracDir <- args[5]
exp_tracDir <- args[7]
openaq.script <- args[16] # TODO warum 16?

if (rlang::is_empty(args)) {
  year <- 2009
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  openaq.script <- "/Users/default/Desktop/paper2021/code/07_openaq.R"
}
exp_tracDir <- file.path(exp_tracDir, toString(year))
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
  argsList <- list(country = "CA", limit = 1000, page = 1)
  exposure_locations_CA <- getResults(urlAQ, argsList) %>%
    select(id, name, country, parameters, firstUpdated, lastUpdated, city, latitude, longitude)

  argsList <- list(country = "US", limit = 20000, page = 1)
  exposure_locations_US <- getResults(urlAQ, argsList) %>%
    select(id, name, country, parameters, firstUpdated, lastUpdated, city, latitude, longitude)

  exposure_locations <- rbind(exposure_locations_CA, exposure_locations_US) %>%
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
  crs = st_crs(geometry),
  agr = "constant"
)

##### ------------assign measurement location to tracts--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  tracts_locationsDir <- file.path(tmpDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv"))
  # quit execution, if already calculated
  if (file.exists(tracts_locationsDir)) {
    return()
  }

  tic(paste("assigned measurement location to all tracts in", name, "in", toString(year)))
  # load shape files
  tracts <- file.path(tracDir, toString(year), paste0("tracts_", toString(year), "_", STUSPS, ".rds")) %>%
    readRDS()

  tracts$location_ids <- apply(tracts, 1, function(tract) {
    geometry <- tract[["geometry"]]

    # subset points, which are inside of the tract
    suppressMessages(points_in_tract <- exposure_locations[geometry, , op = st_within])

    # if there are points inside of the tract, the tract is assigned the mean of pm of those points
    # if there are none, the pm of the closest poin
    locations_of_tract <- ifelse(nrow(points_in_tract) > 0,
      points_in_tract$id,
      geometry %>%
        suppressWarnings(st_centroid) %>%
        st_distance(x = exposure_locations, y = .) %>%
        which.min() %>%
        exposure_locations[., ] %>%
        pull(id)
    )
    # TODO analyse distance
    return(locations_of_tract)
  })

  tracts_locations <- tracts %>%
    as.data.frame() %>%
    select(GEO_ID, location_ids)

  write.csv(tracts_locations, tracts_locationsDir)
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
  tracts_locations <- file.path(tmpDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>% read.csv()

  urlAQ <- paste0(base_url(), "averages")
  tracts_locations$pm <- apply(tracts_locations, 1, function(tract) {
    testthat::expect_equal(1, length(tract[["location_ids"]]))

    argsList <- list(
      limit = 10000,
      page = 1,
      parameter_id = 2,
      temporal = "year",
      spatial = "location",
      location = tract[["location_ids"]] # TODO multiple locations
    )

    exposure <- getResults(urlAQ, argsList) %>%
      filter(parameter == "pm25") %>%
      arrange(year)
    # take earliest year
    pm <- exposure[[1, "average"]]

    return(pm)
  })

  ## -----save as csv--------
  tract_exposure <- tracts_locations %>%
    as.data.frame() %>%
    select(c("GEO_ID", "pm"))

  write.csv(tract_exposure, exp_tracDirX, row.names = FALSE)
  toc()
})
