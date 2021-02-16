test <- exposure

test2 <- test %>%
  arrange(year) %>%
  summarise(id = first(id),
            name = first(name),
            year = first(year),
            average = first(average),
            subtitle = first(subtitle),
            measurement_count = first(measurement_count))
