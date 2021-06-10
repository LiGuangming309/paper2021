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
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales", "ggrepel", 
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

theme_set(theme_classic())
### ----- read stuff----
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()
attr_burd <- fread(file.path(summaryDir, "attr_burd.csv"))

# filter
all_burden <- all_burden %>% filter(attr == "overall")
attr_burd <- attr_burd %>% filter( method == "burnett" & attr == "attributable" & measure3 == "value")

## --filter more---
joined_all_attr <- inner_join(all_burden, attr_burd, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
joined_all_attr <- joined_all_attr %>%
  filter(Ethnicity %in% c("Black or African American", "White, Not Hispanic or Latino") & 
           Year == 2004 &
           Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & 
           source == "National Vital Statistics System" & Education == 666 &
    Region %in% c(
      "United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", " Michigan",
      "New Jersey", "Virginia", "Washington", "Arizona", "Massachusetts", "Tennessee", "Indiana", "Maryland", "Missouri", "Wisconsin",
      "Colorado", "Minnesota", "South Carolina", "Alabama", "Kentucky", "Oregon", "Oklahoma", "Connecticut", "Utah", "Iowa" 
    ))

##---get convex hull----
convex_hull <- joined_all_attr %>%
  select(overall_value, mean) %>%
  as.matrix() %>% 
  chull()

convex_regions <- joined_all_attr[convex_hull, "Region"]
##---plot---

group.colors<- hue_pal()(5)[c(3,5)]
names(group.colors) <- c("Black or African American","White, Not Hispanic or Latino")
joined_all_attr[joined_all_attr== "United States"] <- "national"

g1 <- ggplot(joined_all_attr, aes(x = overall_value, y = mean)) +
  geom_point(size = 3, aes(color = Ethnicity)) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.2),
    legend.text = element_text(size = 9),
    legend.box.background = element_rect(colour = "black"),
    legend.background = element_rect(fill = "transparent")
  ) +
  ylab("attributable burden") +
  xlab("all-cause burden") +
  geom_label_repel(
    aes(label=ifelse(Region %in% c(convex_regions, "national"), as.character(Region),'')
        ), #,shape = agr_by
    size = 2.5,
    box.padding = 0.2, #0.35
    point.padding = 0.5,
    segment.color = "grey50"
  ) +  
  scale_colour_manual(values=group.colors)

#https://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2
#https://cran.r-project.org/web/packages/ggExtra/readme/README.html
ggMarginal(g1, groupColour = TRUE, groupFill = TRUE)

ggsave(file.path(figuresDir, "figure7.svg"), g1,
  height = 5, width = 8
)
