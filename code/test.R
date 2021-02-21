A <- exp_rr2 %>% filter(interpolated == "not interpolated")
B <- exp_rr2 %>% filter(interpolated == "not interpolated",
                        measure == "MR-BRT")

C <- exp_rr2 %>% filter(interpolated == "interpolated")


#ggplot(data = exp_rr2, aes(x = exposure_spline, y = value)) +
  #geom_point(aes(color = measure, shape = interpolated), size =2) +
ggplot(exp_rr2, aes(x = exposure_spline, y = value))+
  geom_point(data =exp_rr2 %>% filter(interpolated == "not interpolated"),
             color = "black", 
             size =1, 
             shape = 2) +
  geom_line(data = exp_rr2 %>% filter(interpolated == "interpolated"),
            aes(color = measure), 
            size =1) +
  xlab("Exposure") +
  ylab("value") +
  ggtitle(paste0(label_cause, ", ", age_group_id))
