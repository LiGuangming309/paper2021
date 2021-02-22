#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/23/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
censDir <- args[3]
attrBurdenDir <- args[4]
summaryDir <- args[6]
plotDir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_attr_burd"
  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/11_summary"
  # plotDir <- "C:/Users/Daniel/Desktop/paper2021/data/12_plot"

  tmpDir <- "/Users/default/Desktop/paper2021/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"  
  attrBurdenDir <- "/Users/default/Desktop/paper2021/10_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/12_summary"  
  plotDir <- "/Users/default/Desktop/paper2021/data/13_plot" 
}

summaryDir <- file.path(summaryDir, agr_by)
attrBurden_gr <- fread(file.path(summaryDir, "attr_burd.csv"))
attrBurden_gr<-attrBurden_gr %>% 
  mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
  filter(Ethnicity %in% c("White, Not Hispanic or Latino", 
                          "White, Hispanic or Latino", 
                          "Black or African American, All Origins",
                          "Asian or Pacific Islander, All Origins",
                          "American Indian or Alaska Native, All Origins"))

#attrBurden_gr1[attrBurden_gr1 == "crudeYLL"] <- "YLL from causes associated with PM exposure"
#attrBurden_gr1[attrBurden_gr1 == "crudeAttrYLL"] <- "YLL directly attributable to PM exposure"
#attrBurden_gr1[attrBurden_gr1 == "crudeAttrDeaths"] <- "Deaths directly attributable to PM exposure"
#attrBurden_gr1[attrBurden_gr1 == "crudeAllDeaths"] <- "Deaths from all causes"
### -------plot 1 ---------
attrBurden_gr1 <- attrBurden_gr %>%
  pivot_longer(
    cols = !c("Year", "Race", "Hispanic.Origin","Ethnicity"),
    names_to = "measure",
    values_to = "value"
  )  %>% 
  as.data.frame

attrBurden_gr1 <- attrBurden_gr1 %>% filter(measure == "crudeAllYLL")
attrBurden_gr1[attrBurden_gr1 == "crudeAllYLL"] <- "YLL from all causes"

# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
g <- ggplot(attrBurden_gr1, aes(x = Year, y = value)) +
  geom_line(aes(color = Ethnicity, linetype = measure), size = 1) +
  #scale_color_manual(values = c("green","orange", "steelblue", "purple"))+
  #scale_linetype_manual(values = c("dashed", "solid")) +
  ylab(paste("YLL per 100.000")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))

g
ggsave(file.path(plotDir, paste0("plot1.png")), plot = g)
### -------plot 2 ---------
attrBurden_gr2 <- attrBurden_gr %>%
  pivot_longer(
    cols = !c("Year", "Race", "Hispanic.Origin","Ethnicity"),
    names_to = "measure",
    values_to = "value"
  )  %>% 
  as.data.frame

attrBurden_gr2 <- attrBurden_gr2 %>% filter(measure == "crudeAttrYLL")
attrBurden_gr2[attrBurden_gr2 == "crudeAttrYLL"] <- "YLL directly attributable to PM exposure"

# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
g <- ggplot(attrBurden_gr2, aes(x = Year, y = value)) +
  geom_line(aes(color = Ethnicity, linetype = measure), size = 1) +
  # scale_color_manual(values = c("green","yellow", "steelblue"))+
  #scale_linetype_manual(values = c("dashed", "solid")) +
  ylab(paste("YLL per 100.000")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))

g
ggsave(file.path(plotDir, paste0("plot2.png")), plot = g)
### -------plot 3 -------
attrBurden_gr2 <- attrBurden_gr %>% select(Year,Ethnicity, crudeAllYLL,crudeAttrYLL)
attrBurden_gr3<-inner_join(attrBurden_gr2,attrBurden_gr2,by = "Year") %>%
  mutate(test1 = (crudeAttrYLL.x-crudeAttrYLL.y),
         test2 = (crudeAllYLL.x-crudeAllYLL.y),
         test3 = 100*(crudeAttrYLL.x-crudeAttrYLL.y)/(crudeAllYLL.x-crudeAllYLL.y)) 

attrBurden_gr3 <- attrBurden_gr3 %>% 
  filter(Ethnicity.x == "Asian or Pacific Islander, All Origins",
         (Ethnicity.y %in% c("White, Not Hispanic or Latino", 
                             "White, Hispanic or Latino", 
                             "Black or African American, All Origins",
                             #"Asian or Pacific Islander, All Origins",
                             "American Indian or Alaska Native, All Origins")))

g <- ggplot(attrBurden_gr3, aes(x = Year, y = test3)) +
  geom_line(aes(color = Ethnicity.y), size = 1) +
  ylab(paste("%")) +
  xlab("Year") +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(plotDir, paste0("plot3.png")), plot = g)

### -------plot 4 -------
attrBurden_gr4 <- attrBurden_gr %>% select(Year,Ethnicity, crudeAllYLL,crudeAttrYLL)%>%
  mutate(prop = 100 * crudeAttrYLL/crudeAllYLL)%>% 
  filter(Ethnicity %in% c("Asian or Pacific Islander, All Origins",
                          "White, Not Hispanic or Latino", 
                          "White, Hispanic or Latino", 
                          "Black or African American, All Origins",
                          #"Asian or Pacific Islander, All Origins",
                          "American Indian or Alaska Native, All Origins"))

g <- ggplot(attrBurden_gr4, aes(x = Year, y = prop)) +
  geom_line(aes(color = Ethnicity), size = 1) +
  ylab(paste("%")) +
  xlab("Year") +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))

ggsave(file.path(plotDir, paste0("plot4.png")), plot = g)
g