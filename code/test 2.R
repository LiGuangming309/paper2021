# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

attr_burd2<- lapply(2000:2016, function(year){
  attr_burd <- paste0("~/Desktop/paper2021/data/09_attr_burd/nation/attr_burd_",toString(year),".csv") %>%read.csv
  attr_burd2 <- attr_burd %>% 
    filter(Hispanic.Origin == "All Origins") %>%
    group_by(Year,label_cause) %>%
    summarise(Deaths = sum(Deaths),
              YLL = sum(YLL),
              attrDeaths = sum(attrDeaths),
              attrYLL = sum(attrYLL))
}) %>% do.call(rbind,.)

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

g1 <- ggplot(attr_burd3, aes(x = Year, y = attrDeaths)) +
  geom_line(size = 1) +
  ylab(paste("Deaths")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  ggtitle(paste("plot1"))

g2 <- ggplot(attr_burd3, aes(x = Year, y = attrYLL)) +
  geom_line(size = 1) +
  ylab(paste("YLL")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  ggtitle(paste("plot2"))

fwrite(attr_burd2, "~/Desktop/paper2021/data/test/attr_burd2.csv", sep = ';')
fwrite(attr_burd3, "~/Desktop/paper2021/data/test/attr_burd3.csv", sep = ';')

ggsave("~/Desktop/paper2021/data/test/deaths.png", plot = g1)
ggsave("~/Desktop/paper2021/data/test/YLL.png", plot = g2)