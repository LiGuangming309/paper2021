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

print(args)
# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_attr_burd"
  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/11_summary"
  # plotDir <- "C:/Users/Daniel/Desktop/paper2021/data/12_plot"

  tmpDir <- "/Users/default/Desktop/paper2021/tmp"
  censDir <- "/Users/default/Desktop/paper2021/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/09_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/11_summary"
  plotDir <- "/Users/default/Desktop/paper2021/12_plot"
}

summaryDir <- file.path(summaryDir, agr_by)
attrBurden_gr <- fread(file.path(summaryDir, "attr_burd.csv"))

attrBurden_gr <- attrBurden_gr %>%
  pivot_longer(
    cols = !c("Year", "Race", "Hispanic.Origin"),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
  as.data.frame

### ----plot 1 -------
attrBurden_gr <- attrBurden_gr %>%
  filter(
    Ethnicity %in% c("White, Not Hispanic or Latino", "White, Hispanic or Latino", "Black or African American, All Origins"),
    measure %in% c("crudeYLL", "crudeAttrYLL")
  )

attrBurden_gr[attrBurden_gr == "crudeYLL"] <- "YLL from causes associated with PM exposure"
attrBurden_gr[attrBurden_gr == "crudeAttrYLL"] <- "YLL directly attributable to PM exposure"

## --- plot-----
# https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
# https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
g <- ggplot(attrBurden_gr, aes(x = Year, y = value)) +
  geom_line(aes(color = Ethnicity, linetype = measure), size = 1) +
  # scale_color_manual(values = c("green","yellow", "steelblue"))+
  scale_linetype_manual(values = c("dashed", "solid")) +
  ylab(paste("YLL[75] per 100.000")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  ggtitle(paste("plot1"))

ggsave(file.path(plotDir, paste0("plot1.png")), plot = g)
