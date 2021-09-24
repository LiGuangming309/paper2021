
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

attr_burd3_2001 <- read.csv("~/Desktop/paper2021/data/13_attr_burd/county/nvss/attr_burd3_2001.csv")

attr_burd <- attr_burd3_2001 %>%
  filter(measure1 == "Deaths" & measure2 == "age-adjusted rate" &
 scenario == "A" & rural_urban_class == 666
         & Year %in% 2000:2016 & method == "di_gee" & Race == "Black or African American") %>% 
  mutate(county = as.integer(county))

counties <- tigris::counties() %>% mutate(GEOID = as.integer(GEOID))

counties_join <- tigris::geo_join(counties, attr_burd, "GEOID", "county", how = "inner")

tm2 <- tm_shape(counties_join#, projection = 26916
                ) +
  tm_fill("mean", style = "quantile", n = 7, palette = "Greens", 
          title = "attributable burden for Blacks") + 
  tm_legend(bg.color = "white", bg.alpha = 0.6)# + 
  #tmap::tm_style_gray()
tm2
