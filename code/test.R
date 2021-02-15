urlAQ <- paste0(base_url(), "averages")

argsList <- list(
  limit = 10000,
  page = 1,
  #parameter_id = 2,
  parameter_id = "50",
  temporal = "year",
  spatial = "location",
  location = 8670 #TODO multiple locations
) 

exposure <- getResults(urlAQ, argsList)

tracts2 <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
  file.path(tracDir, toString(year), .) %>%
  readRDS(.)
