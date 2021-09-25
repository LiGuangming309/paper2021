
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

attr_burd3_2000 <- read.csv("~/Desktop/paper2021/data/13_attr_burd/county/nvss/attr_burd3_2000.csv")
total_burden_nvss_2000 <- read.csv("~/Desktop/paper2021/data/09_total_burden_parsed/county/nvss/total_burden_nvss_2000.csv")

attr_burd <- attr_burd3_2000 %>%
  filter(measure1 == "Deaths" & measure2 == "age-adjusted rate" &
 scenario == "A" & rural_urban_class == 666
         & Year %in% 2000:2016 & method == "di_gee" & Race == "Black or African American") %>% 
  mutate(county = as.integer(county))

total_burden <- total_burden_nvss_2000 %>% 
  filter(rural_urban_class == 666 &Race == "Black or African American" & label_cause == "all-cause") %>% 
  group_by(county) %>%
  summarise(Deaths =sum(Deaths)) 

counties <- tigris::counties(year = 2000) %>% 
  mutate(GEOID = as.integer(CNTYIDFP00)) %>%
  select(GEOID, geometry)
#counties<-get_decennial(geography = "county", variables = "P012A005", year = 2000, state = STUSPS, geometry = TRUE, key = key)%>% 
#  rename(GEO_ID = GEOID)

#choose year vintage!
basemap <- tigris::states()
counties_join1 <- tigris::geo_join(counties, attr_burd, "GEOID", "county", how = "inner")
#counties_join2 <- tigris::geo_join(counties, total_burden, "GEOID", "county", how = "inner")

#tm1 <- tm_shape(counties_join1, border.col = "black"#, projection = 26916
#                ) +#
#  tm_fill("mean", style = "quantile", n = 7, palette = "Greens", 
#          title = "attributable burden for Blacks") + 
#  tm_legend(bg.color = "white", bg.alpha = 0.6)+
#  tmap_mode(mode = c("view"))# + 
  #tmap::tm_style_gray()
#tm1

#https://stackoverflow.com/questions/56903210/how-to-add-a-basemap-to-a-tmap-plot-in-r
tm2 <- tm_shape(basemap)+
  tm_shape(counties_join2#, projection = 26916
) +
  tm_fill("Deaths", style = "quantile", n = 7, palette = "Greens", 
          title = "total Deaths for Blacks") + 
  tm_legend(bg.color = "white", bg.alpha = 0.6)+
  #tmap_mode(mode = c("view"))+
  tm_borders(lwd = 0.5, 
             col = "black")
tm2
#TODO show base map

tmap_save(tm2, "/Users/default/Desktop/counties.png")#

missing_counties <- setdiff( counties$GEOID, total_burden_nvss_2000$county)
