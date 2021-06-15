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
    source == "National Vital Statistics System" & attr == "overall")

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & 
           method == "burnett" & attr == "attributable" &
    source == "National Vital Statistics System" & scenario == "A")

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
# https://rpubs.com/Koundy/71792
# http://rstudio-pubs-static.s3.amazonaws.com/9575_8a5dc0315e7d48ea94e0fd2546727041.html
## --- figure 1, population-weighted mean particular matter exposure---
pm_summ <- pm_summ %>% filter(agr_by == "nation" & pm_metric == "mean" & Gender.Code == "All genders" & scenario == "A")
pm_summ1 <- pm_summ %>% filter(Education == 666 & Ethnicity != "All, All Origins")
g1 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("population-weighted annual average PM2.5 in μg/m^3") +
  xlim(2000, 2016) +
  theme(
    legend.title = element_blank(),
    #legend.position = position_dodge(3),
    legend.position = c(0.72, 0.88),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = FALSE))
#ggsave(file.path(figuresDir, "figure1a.svg"), g1)
#https://ggplot2.tidyverse.org/reference/position_dodge.html

pm_summ2 <- pm_summ %>% filter(Education != 666 & Ethnicity == "All, All Origins")
g2 <- ggplot(pm_summ2, aes(x = Year, y = value, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("population-weighted annual average PM2.5 in μg/m^3") +
  theme(
    legend.title = element_blank(),
    #legend.position = c(0.4, 0.2), # plot.margin = unit(c(0,1,0,-1), "lines") plot.margin = unit(c(0,-.5,0,.5), "lines"),
    legend.position = c(0.43, 0.14),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure1b.svg"), g2)

g3 <- ggarrange(g1, g2, ncol = 1, labels = "AUTO", align = "v")
ggsave(file.path(figuresDir, "figure1.svg"), g3, height = 9, width = 8)

# https://stackoverflow.com/questions/64757410/shared-x-and-y-axis-labels-ggplot2-with-ggarrange
rm(g1, g2,g3, pm_summ1, pm_summ2)
## -- figure 2, all-cause burden ----
all_burden1 <- all_burden %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins")
g1 <- ggplot(all_burden1, aes(x = Year, y = overall_value, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("all-cause age-adjusted mortality per 100k") +
  xlim(2000, 2016) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.733, 0.9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(20,20,20,20),"points")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure2a.svg"), g1)
#g1$layout$clip[g1$layout$name == "panel"] <- "off"

all_burden2 <- all_burden %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins")
g2 <- ggplot(all_burden2, aes(x = Year, y = overall_value, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("all-cause age-adjusted mortality per 100k") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.09),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
#ggsave(file.path(figuresDir, "figure2b.svg"), g2)

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.1, 1),
                labels = c("A", "", "B"),
                align = "v"
                )

ggsave(file.path(figuresDir, "figure2.svg"), g3, height = 9, width = 8)

rm(all_burden1, all_burden2, g1, g2, g3)
## -- figure 3, attributable burden----
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins"& measure3 == "value" )
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("age-adjusted death rate per 100,000") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.85),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins"& measure3 == "value")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("age-adjusted death rate per 100,000") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.1),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) #0.1

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.2, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir, "figure3.png"), g3, height = 9, width = 8)
rm(attr_burd1, attr_burd2, g1, g2, g3)
## -- figure 4, prop. of overall burden----
attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins"& measure3 == "prop. of overall burden")
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins"& measure3 == "prop. of overall burden")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 1.1),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))  +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE)

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.2, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir, "figure4.svg"), g3, height = 9, width = 8)

rm(attr_burd1, attr_burd2, g1, g2) 
##---figure 5, prop of difference----

attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins"& measure3 == "proportion of disparity to White, Not Hispanic attributable" #&
                                     #Ethnicity %in% c("Black or African American")
                                     )
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.72, 0.9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) 

attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins"& measure3 == "proportion of disparity to Graduate or professional degree attributable")
g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("%") +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.69, 1.03),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) 

g3 <- ggarrange(g1,NULL, g2, 
                ncol = 1, 
                heights = c(1, 0.1, 1),
                labels = c("A", "", "B"),
                align = "v"
)
ggsave(file.path(figuresDir, "figure5.svg"), g3, height = 9, width = 8)
rm(attr_burd1, attr_burd2, g1, g2) 
## ---figure 6, differences by state ---
all_burden1 <- all_burden %>%
  filter(Year == 2000 &
    Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American") &
    Region %in% c(
      "United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia",
      "North Carolina"
    )) %>%
  arrange(Ethnicity) %>%
  group_by(Region, agr_by) %>%
  summarize(difference = first(overall_value) - last(overall_value))
all_burden1[all_burden1 == "United States"] <- "national level"

g1 <- ggbarplot(all_burden1,
  x = "Region", y = "difference",
  fill = "agr_by", # change fill color by cyl
  # color = "white",            # Set bar border colors to white
  palette = "jco", # jco journal color palett. see ?ggpar
  sort.val = "asc", # Sort the value in dscending order
  sort.by.groups = FALSE, # Don't sort inside each group
  x.text.angle = 90, # Rotate vertically x axis texts
  rotate = TRUE
) + theme(axis.title.y = element_blank())

attr_burd1 <- attr_burd %>%
  filter(Year == 2000 &
          measure3 == "value"  & 
    Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American") &
    Region %in% c(
      "United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia","North Carolina"
    )) %>%
  arrange(Ethnicity) %>%
  group_by(Region, agr_by) %>%
  summarize(difference = first(mean) - last(mean))

attr_burd1[attr_burd1 == "United States"] <- "national level"
g2 <- ggbarplot(attr_burd1,
  x = "Region", y = "difference",
  fill = "agr_by", # change fill color by cyl
  # color = "white",            # Set bar border colors to white
  palette = "jco", # jco journal color palett. see ?ggpar
  sort.val = "asc", # Sort the value in dscending order
  sort.by.groups = FALSE, # Don't sort inside each group
  x.text.angle = 90, # Rotate vertically x axis texts
  rotate = TRUE
) + 
  theme(axis.title.y = element_blank()) +
  xlab("difference in attributable burden")

g3 <- ggarrange(g1, g2,
  legend = "none",
  labels = "AUTO",
  align = "h"
)
#ggsave(file.path(figuresDir, "figure6.svg"), g3)

rm(all_burden1, attr_burd1, g1, g2, g3)
