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

#http://ihmeuw.org/5cwq
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

attr_burd3<- lapply(2000:2016, function(year){
  attr_burd <- paste0("~/Desktop/paper2021/data/09_attr_burd/nation/attr_burd_",toString(year),".csv") %>%read.csv
  attr_burd2 <- attr_burd %>% 
    filter(Hispanic.Origin == "All Origins") %>%
    group_by(Year) %>%
    summarise(Deaths = sum(Deaths),
              YLL = sum(YLL),
              attrDeaths = sum(attrDeaths),
              attrYLL = sum(attrYLL))
}) %>% do.call(rbind,.)

g3 <- ggplot(attr_burd3, aes(x = Year, y = attrDeaths)) +
  geom_line(size = 1) +
  ylab(paste("Deaths")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())

g4 <- ggplot(attr_burd3, aes(x = Year, y = attrYLL)) +
  geom_line(size = 1) +
  ylab(paste("YLL")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())

fwrite(attr_burd2, "~/Desktop/paper2021/data/test/attr_burd2.csv", sep = ';')
fwrite(attr_burd3, "~/Desktop/paper2021/data/test/attr_burd3.csv", sep = ';')

ggsave("~/Desktop/paper2021/data/test/deaths_attr.png", plot = g3)
ggsave("~/Desktop/paper2021/data/test/YL_attrL.png", plot = g4)