
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "tigris","tmap" 
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

pm_summary <- read.csv("C:/Users/Daniel/Desktop/paper2021/data/14_summary/pm_summary.csv")
pm_summary <- pm_summary %>% filter(
  scenario == "A" & rural_urban_class != "666" & Region == "United States" & Education == "666" & pm_metric == "mean" & Ethnicity == "All, All Origins"
) %>%
  mutate(rural_urban_class = as.factor(rural_urban_class))


g5 <- ggplot(pm_summary, aes(x = Year, y = value, color = rural_urban_class)) +
  geom_line()
g5
