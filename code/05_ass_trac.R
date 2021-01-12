
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign PM exposure to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing

packages <- c("plyr", "magrittr", "tigris", "sf", "tidyverse", "sp", "tmap", "tictoc","rhdf5")

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

# TODO l?schen
# year <- 2016
#agr_by <- "county"

# tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
# exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"
# censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
# cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_dem.agr"

#tmpDir <- "C:/Users/Daniel/Desktop/paper2020/data/tmp"
#expDir <- "C:/Users/Daniel/Desktop/paper2020/data/01_exposure"
#tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/02_tracts"
#exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"

## ---------------load data---------------

# load exposure data
filenameExp <- paste0(toString(year), ".h5")
filepathExp <- file.path(expDir, filenameExp)
hdf_file <- H5Fopen(filepathExp)
exp_data <- as.matrix(hdf_file$CorrectedPM2.5)
long_vec <- c(as.matrix(hdf_file$longitude))
lat_vec <- c(as.matrix(hdf_file$latitude))

# load some useful estimates to optimize code
filenameM <- paste0("m_exp_", toString(year), ".RData")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
                read.csv

# create folder, where calculations will be stored
exp_tracDir <- file.path(exp_tracDir, toString(year))
dir.create(exp_tracDir, recursive = T, showWarnings = F)

## -----------------calculation---------------
tic(paste("Assigned pm exposure to each tract for year", toString(year), "for all states"))
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  exp_tracDirX <- paste0("exp_trac_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(exp_tracDir, .)

  # quit execution, if already calculated
  if (file.exists(exp_tracDirX)) {
    return()
  }

  # load shape files
  tracts <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
    file.path(tracDir, toString(year), .) %>%
    readRDS(.)

  tic(paste("Assigned pm exposure to each tract for year", toString(year), "in", name))
  # estimate pm exposure for each tract
  #tracts$pm <- sapply(tracts$geometry, function(tract) {
  tracts$pm <- apply(tracts,1, function(tract) {
    # get enclosing box, make sure in range of exposure data
    geometry <-tract[["geometry"]]
    bbox <- st_bbox(geometry)
    long_min <- bbox$xmin %>%
      max(., long_vec[1])
    lat_min <- bbox$ymin %>%
      max(., lat_vec[1])
    long_max <- bbox$xmax %>%
      min(., long_vec[length(long_vec)])
    lat_max <- bbox$ymax %>%
      min(., lat_vec[length(lat_vec)])
    
    
    # estimate corresponding grid in pm exposure data
    long_row_min <- -1 + ((long_min - long_vec[1]) / m_max_long) %>%
      floor()
    lat_row_min <- -1 + ((lat_min - lat_vec[1]) / m_max_lat) %>%
      floor()
    long_row_max <- 1 + ((long_max - long_vec[1]) / m_min_long) %>%
      ceiling()
    lat_row_max <- 1 + ((lat_max - lat_vec[1]) / m_min_lat) %>%
      ceiling()
    
    long_subset <- long_vec[long_row_min:long_row_max]
    lat_subset <- lat_vec[lat_row_min:lat_row_max]
    pm_subset <- exp_data[long_row_min:long_row_max, lat_row_min:lat_row_max]

    # estimates, where to look for the pm data for this particular tract to improve run time
    points_subset <- data.frame(
      lat = rep(lat_subset, times = length(long_subset)),
      lng = rep(long_subset, each = length(lat_subset)),
      pm = as.vector(t(pm_subset))
    ) %>%
      st_as_sf(.,
        coords = c("lng", "lat"),
        crs = st_crs(geometry),
        agr = "constant"
      )

    # subset points, which are inside of the tract
    suppressMessages(points_in_tract <- points_subset[geometry, , op = st_within])

    # if there are points inside of the tract, the tract is assigned the mean of pm of those points
    # if there are none, the pm of the closest point
    pm <- ifelse(nrow(points_in_tract) > 0,
      points_in_tract$pm %>%
        mean(., na.rm = TRUE),
      geometry %>%
        suppressWarnings(st_centroid) %>%
        st_distance(x = points_subset, y = .) %>%
        which.min() %>%
        points_subset[., ] %>%
        pull(pm)
    ) %>%
      round() %>%
      prod(0.01)
  })
  toc()

  ## --------------plot-----------
  # save everything as interactive map via tmap
  if (TRUE) {
    tm <- tm_shape(tracts) +
      tm_polygons("pm", alpha = 0.6)
    #+tm_format("NLD",title=paste("Particulate Matter Exposure for",year,"in",name)) #TODO

    filepathExpTrac_plot <- paste0("exp_trac_", toString(year), "_", STUSPS, ".html") %>% # png/html möglich
      file.path(exp_tracDir, .)

    tmap_save(tm, filename = filepathExpTrac_plot)
  }

  ## -----save as csv--------
  
  tracts <- tracts %>%
    as.data.frame() %>%
    select(c("GEO_ID", "pm"))  

  write.csv(tracts, exp_tracDirX, row.names = FALSE)
})
toc()
""