A <- exp_rr2 %>% filter(interpolated == "not interpolated")
B <- exp_rr2 %>% filter(interpolated == "not interpolated",
                        measure == "MR-BRT")

C <- exp_rr2 %>% filter(interpolated == "not interpolated",
                        measure == "RR")


class()
test <- st_crs(tracts)
test$input
