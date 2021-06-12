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
theme_set(theme_classic())
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()

all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           source == "National Vital Statistics System" & attr == "overall")

##---plot---
all_burden <- all_burden %>%
  filter(
    #Year == 2004 &
     Education == 666 &
      Region %in% c("Wyoming", "Vermont", "Alaska", "North Dakota", "South Dakota", "Delaware", "Montana", "Rhode Island", "Maine", "New Hampshire", "Hawaii")
  )

all_burden <- all_burden %>% 
  group_by(Year, Region) %>% #Ethnicity, , color = Ethnicity
  summarise(overall_value = sum(overall_value))

g1 <- ggplot(all_burden, aes(x = Year, y = overall_value)) +
  geom_line(size = 1) +
  xlab("Year") +
  #ylab("Mortality per 100k") +
  ylab("age-adjusted mortality per 100k") +
  
  theme(legend.position="bottom",
        legend.box.margin = margin(1, 36, 1, 6))+
  #theme(legend.position=c(0.1, 0.1))+
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.1, show.legend = FALSE) +
  #facet_grid(~Region,ncol = 3, labeller=label_wrap_gen(width = 10, multi_line = TRUE)) 
  #scales='free_x', space='free_x', 
  facet_wrap(~Region) 

ggsave(file.path(figuresDir, "test.png"), g1, height = 9, width = 8)