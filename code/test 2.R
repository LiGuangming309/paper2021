pm_summary <- read.csv("C:/Users/Daniel/Desktop/paper2021/data/14_summary/pm_summary.csv")

pm_summary <- pm_summary %>%
  filter(scenario == "A" & rural_urban_class != "All" & Ethnicity == "All, All Origins" & Education == "666" & pm_metric == "mean")

g5 <- ggplot(pm_summary, aes(x = Year, y = value, color = rural_urban_class)) +geom_line()
g5
