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
#theme_classic() 

# https://stackoverflow.com/questions/19437934/r-how-can-i-make-a-barplot-with-labels-parallel-horizontal-to-bars
#### ----figure 1----
#total_burden2003revision <- read.csv("~/Desktop/paper2021/data/test/total_burden2003revision.csv")
# pdf(file=file.path(figuresDir, "figure1.pdf"),width=4,height=4,useDingbats = F)
# par(mar=c(3.5, 4, 0, 0))

# plot(total_burden2003revision[,c("Year","prop")],type="l",col="cyan",ylab="",axes=F,xlab="",
#     ylim=c(0, 1), lwd=1.5)

# axis(2,las=1)
# axis(1)
# title(ylab="% of death counts with 2003 Education revision")
# mtext("Mortality from PM2.5 per 100k per year", side=2, line=3, adj=-0.5)

# dev.off()

## ---figure 2 ---
#https://stackoverflow.com/questions/25781284/simplest-way-to-plot-changes-in-ranking-between-two-ordered-lists-in-r
#https://www.r-bloggers.com/2018/04/bump-chart/ 
all_burden1 <- all_burden %>%
  filter(Year == 2000 &
  Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American, All Origins") &
  Region %in% c("United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", 
                "North Carolina", "Michigan")) %>%
  arrange(Ethnicity) %>%
  group_by(Region, agr_by) %>%
  summarize(difference = first(overall_value) - last(overall_value))

g1 <- ggbarplot(all_burden1, x = "Region", y = "difference",
          fill = "agr_by",               # change fill color by cyl
          #color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90 ,          # Rotate vertically x axis texts
          rotate = TRUE#,
          #ggtheme = theme_minimal()
)+ ggtitle("difference in all-cause burden")

attr_burd1 <- attr_burd %>%
  filter(Year == 2000 &
           Ethnicity %in% c("White, Not Hispanic or Latino", "Black or African American, All Origins") &
           Region %in% c("United States", "California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", 
                         "North Carolina", "Michigan")) %>%
  arrange(Ethnicity) %>%
  group_by(Region, agr_by) %>%
  summarize(difference = first(mean) - last(mean))

g2 <- ggbarplot(attr_burd1, x = "Region", y = "difference",
                fill = "agr_by",               # change fill color by cyl
                #color = "white",            # Set bar border colors to white
                palette = "jco",            # jco journal color palett. see ?ggpar
                sort.val = "asc",          # Sort the value in dscending order
                sort.by.groups = FALSE,     # Don't sort inside each group
                x.text.angle = 90 ,          # Rotate vertically x axis texts
                rotate = TRUE#,
                #ggtheme = theme_minimal()
) + ggtitle("difference in attributable burden")

g1
g2
g3 <- ggarrange(g1, g2,
          common.legend = TRUE, legend = "top", 
          labels = "AUTO") 
ggsave(file.path(figuresDir,"figure2.png"), g3)