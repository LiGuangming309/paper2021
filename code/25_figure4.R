#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "grid", "cowplot",
  "dplyr"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
}

attr_burd <- rbind(
  fread(file.path(summaryDir, "attr_burd.csv")),
  fread(file.path(summaryDir, "attr_burd_prop.csv"))
)

theme_set(theme_classic())
### ----- read stuff----
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))

# filter
all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           source == "National Vital Statistics System" & attr == "overall" & Education == 666 
           )

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == "burnett" & attr == "attributable" &
           source == "National Vital Statistics System" & Education == 666 & measure3 == "value" ) 

##--claculate---
joined_all_attr <- inner_join(all_burden, attr_burd, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
joined_all_attr <- joined_all_attr %>% filter(Ethnicity %in% c("Black or African American", "White, Not Hispanic or Latino") & Year == 2000)

g1 <- ggplot(joined_all_attr %>% filter(agr_by == "STATEFP"),
             aes(x= overall_value, y = mean, color = Ethnicity)) +
  geom_point(size =3) +
  geom_text(aes(label=Region),hjust=1, vjust=1) #+
  #geom_point(joined_all_attr %>% filter(agr_by == "STATEFP"),
  #           size =3) 
g1  
