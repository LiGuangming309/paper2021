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
  "data.table", "magrittr", "shiny", "ggplot2",  "ggpubr", "scales",
  "dplyr"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# load calculated data
summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
# if not downloaded, load from github
if (!file.exists(summaryDir)) summaryDir <- "https://raw.github.com/FridljDa/paper2021/master/data/14_summary"

attr_burd <- rbind(
  fread(file.path(summaryDir, "attr_burd.csv")),
  fread(file.path(summaryDir, "attr_burd_prop.csv"))
)

###----- read stuff----
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))

# filter
all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & attr == "overall")

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    measure3 == "value" & method == "burnett" & attr == "attributable" &
    source == "National Vital Statistics System")

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
# https://rpubs.com/Koundy/71792
# http://rstudio-pubs-static.s3.amazonaws.com/9575_8a5dc0315e7d48ea94e0fd2546727041.html
## --- figure 1, particular matter exposure---
pm_summ <- pm_summ %>% filter(Gender.Code == "All genders" & agr_by == "nation" & pm_metric == "mean")
pm_summ1 <- pm_summ %>% filter(Education == 666)
g1 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("μg/m^3") +
  xlim(2000, 2016) +
  theme_classic() +
  #theme(legend.position="bottom") +
  theme(legend.title=element_blank(),
        legend.position = c(0.72, 0.9),
        legend.text=element_text(size=9)) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
g1
ggsave(file.path(figuresDir, "figure1a.png"), g1)

pm_summ2 <- pm_summ %>% filter(Education != 666)
g2 <- ggplot(pm_summ2, aes(x = Year, y = value, color = Education)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("μg/m^3") +
  theme_classic() +
  theme(legend.position="bottom") +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))
ggsave(file.path(figuresDir, "figure1b.png"), g2)
rm(g1,g2, pm_summ1, pm_summ2)

##-- figure 2, all-cause burden ----

## ---figure 4, differences by state ---
all_burden1 <- all_burden %>%
  filter(Year == 2000 &
    Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American, All Origins") &
    Region %in% c(
      "United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia",
      "North Carolina", "Michigan"
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
    Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American, All Origins") &
    Region %in% c(
      "United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia",
      "North Carolina", "Michigan"
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
) + theme(axis.title.y = element_blank())

g3 <- ggarrange(g1, g2,legend = "none",
  labels = "AUTO"
)
ggsave(file.path(figuresDir, "figure4.png"), g3)

rm(all_burden1, attr_burd1, g1, g2, g3)
