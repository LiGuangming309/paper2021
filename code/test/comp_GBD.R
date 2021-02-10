# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen=10000)

attr_burd2<- lapply(2000:2016, function(year){
  attr_burd <- paste0("~/Desktop/paper2021/data/10_attr_burd/nation/attr_burd_",toString(year),".csv") %>%read.csv
  attr_burd2 <- attr_burd %>% 
    filter(Hispanic.Origin == "All Origins") %>%
    group_by(Year,label_cause) %>%
    summarise(Deaths = sum(Deaths),
              YLL = sum(YLL),
              attrDeaths = sum(attrDeaths),
              attrYLL = sum(attrYLL))
}) %>% do.call(rbind,.)


#####----- plot -----
label_causes <- attr_burd2$label_cause %>% unique

for(cause in label_causes){
  #attr_burd2 <- attr_burd2 %>% filter(label_cause %in% c("neo_lung"))
  attr_burd2_sub <- attr_burd2 %>% filter(label_cause %in% c(cause))
  #http://ihmeuw.org/5cwp 
  g1 <- ggplot(attr_burd2_sub, aes(x = Year, y = Deaths)) +
    geom_line(aes(color = label_cause), size = 1) +
    ylab(paste("Deaths")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) 
  
  g2 <- ggplot(attr_burd2_sub, aes(x = Year, y = YLL)) +
    geom_line(aes(color = label_cause), size = 1) +
    ylab(paste("YLL")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())
  
  ggsave(paste0("~/Desktop/paper2021/data/test/deaths_",cause,".png"), plot = g1)
  ggsave(paste0("~/Desktop/paper2021/data/test/yll_",cause,".png"), plot = g2)
}

attr_burd2_sub <- attr_burd2 %>% filter(Year == 2016)
#http://ihmeuw.org/5d88
#http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/373bbf2f4ed669fd47fc037be67685e7
g1 <- ggplot(attr_burd2, aes(x = Year, y = attrDeaths)) +
  geom_line(aes(color = label_cause), size = 1) +
  ylab(paste("Deaths")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())

g2 <- ggplot(attr_burd2, aes(x = Year, y = attrYLL)) +
  geom_line(aes(color = label_cause), size = 1) +
  ylab(paste("YLL")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())

g1
g2
##------ do not aggregate by disease-------
attr_burd3<- lapply(2000:2016, function(year){
  attr_burd <- paste0("~/Desktop/paper2021/data/10_attr_burd/nation/attr_burd_",toString(year),".csv") %>%read.csv
  attr_burd2 <- attr_burd %>% 
    filter(Hispanic.Origin == "All Origins") %>%
    group_by(Year) %>%
    summarise(Deaths = sum(Deaths),
              YLL = sum(YLL),
              attrDeaths = sum(attrDeaths),
              attrYLL = sum(attrYLL))
}) %>% do.call(rbind,.) 

attr_burd3 <- attr_burd3 %>% 
  pivot_longer(cols = -Year,
               names_to = "measure_name",
               values_to = "val")
###----- read gbd-------
gbd_attr <- read.csv("~/Desktop/paper2021/data/test/IHME-GBD_2019_DATA-7577c7ca-1/IHME-GBD_2019_DATA-7577c7ca-1.csv") %>%
  filter(metric_name == "Number", year %in% 2000:2016) %>%
  select(measure_name, year, val) %>%
  rename(Year = year)

gbd_attr[gbd_attr == "YLLs (Years of Life Lost)"] <- "attributate YLL according to GBD"
gbd_attr[gbd_attr == "Deaths"] <- "attributate Deaths according to GBD"

attr_burd3 <- rbind(attr_burd3, gbd_attr)

attr_burd3[attr_burd3 == "attrDeaths"] <- "attributate Deaths according to my code"
attr_burd3[attr_burd3 == "attrYLL"] <- "attributate YLL according to my code"

attr_burd3_sub <- attr_burd3 %>% filter(measure_name %in% c("attributate YLL according to GBD","attributate YLL according to my code"))
g <- ggplot(attr_burd3_sub, aes(x = Year, y = val)) +
  geom_line(aes(color = measure_name), size = 1) +
  ylab(paste("YLL")) +
  xlab("Year") +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  ylim(0, NA)

attr_burd3_sub <- attr_burd3 %>% filter(measure_name %in% c("attributate Deaths according to GBD","attributate Deaths according to my code"))
g <- ggplot(attr_burd3_sub, aes(x = Year, y = val)) +
  geom_line(aes(color = measure_name), size = 1) +
  ylab(paste("Deaths")) +
  xlab("Year") +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  ylim(0, NA)

##--- save----
fwrite(attr_burd2, "~/Desktop/paper2021/data/test/attr_burd2.csv", sep = ';')
fwrite(attr_burd3, "~/Desktop/paper2021/data/test/attr_burd3.csv", sep = ';')

ggsave("~/Desktop/paper2021/data/test/deaths_attr.png", plot = g3)
ggsave("~/Desktop/paper2021/data/test/YL_attrL.png", plot = g4)

