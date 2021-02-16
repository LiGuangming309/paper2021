
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

packages <- c("data.table", "plyr", "magrittr", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc", "units")

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
  year <- 2000
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  tracDir <- "/Users/default/Desktop/paper2021/data/02_tracts"
  exp_tracDir <- "/Users/default/Desktop/paper2021/data/03_exp_tracts"
  openaq.script <- "/Users/default/Desktop/paper2021/code/07_openaq.R"
}
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
  crs = 4326, 
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
  fwrite(tracts_locations, tracts_locationsDir, row.names = FALSE)
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
  tracts_locations <- file.path(tmpDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>% fread()

  urlAQ <- paste0(base_url(), "averages")
  tracts_locations$pm <- apply(tracts_locations, 1, function(tract) {
    location_ids <- tract[["location_ids"]] %>%
      toString %>%
      strsplit(., "|", fixed = TRUE) %>%
      unlist()

    pm <- sapply(location_ids, function(location_id) {
      argsList <- list(
        limit = 10000,
        page = 1,
        parameter_id = 2,
        temporal = "year",
        spatial = "location",
        location = location_id
      )

      exposure <- getResults(urlAQ, argsList)

      exposure <- exposure %>%
        filter(parameter == "pm25") %>%
        arrange(year)
      # take earliest year
      pm <- exposure[[1, "average"]]
    })

    return(mean(pm))
  })

  ## -----save as csv--------
  tract_exposure <- tracts_locations %>%
    as.data.frame() %>%
    select(c("GEO_ID", "pm"))

  write.csv(tract_exposure, exp_tracDirX, row.names = FALSE)
  toc()
})
