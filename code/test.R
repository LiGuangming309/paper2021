A <- exp_rr2 %>% filter(interpolated == "not interpolated")
B <- exp_rr2 %>% filter(interpolated == "not interpolated",
                        measure == "MR-BRT")

C <- exp_rr2 %>% filter(interpolated == "interpolated")


#ggplot(data = exp_rr2, aes(x = exposure_spline, y = value)) +
  #geom_point(aes(color = measure, shape = interpolated), size =2) +

