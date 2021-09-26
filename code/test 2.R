test <- censDataFrom %>%
  left_join(crosswalk, by = c("GEO_ID" = "trtidFrom"))
