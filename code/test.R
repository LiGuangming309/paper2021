test <- predictions %>%
  group_by(year, Id2) %>%
  summarize(sum = sum(over65))
