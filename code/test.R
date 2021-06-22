test <- pop_summary %>%
  filter(source2 == "Own Interpolation" & Education == 666 & agr_by == "nation" & Ethnicity != "All, All Origins")

ggplot(test, aes(x = Year, y = Population, color = Ethnicity)) +geom_line()


