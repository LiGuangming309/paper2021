# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

all_burd <- read.csv("~/Desktop/paper2021/data/14_summary/all_burd.csv")
all_burd <- all_burd %>% 
  filter(Gender.Code == "All genders" &
           Region == "United States" &
           measure1 == "Deaths" &
           measure2 == "absolute number" &
           attr == "overall")

all_burd_educ <- all_burd %>% 
  filter( Education == 666) %>%
  group_by(Year) %>%
  summarise(overall_value = sum(overall_value))

all_burd_race <- all_burd %>% 
  filter( Education != 666) %>%
  group_by(Year) %>%
  summarise(overall_value = sum(overall_value))

test <- inner_join(all_burd_educ, all_burd_race, by = "Year") %>%
  mutate(asf = overall_value.x/overall_value.y)
