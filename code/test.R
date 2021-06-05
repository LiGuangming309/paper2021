# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

##---burden----
all_burd <- read.csv("~/Desktop/paper2021/data/14_summary/all_burd.csv")
all_burd <- all_burd %>% 
  filter(Gender.Code == "All genders" &
           Region == "United States" &
           measure1 == "Deaths" &
           measure2 == "absolute number" &
           attr == "overall")

all_burd_race <- all_burd %>% 
  filter( Education == 666) %>%
  group_by(Year) %>%
  summarise(overall_value = sum(overall_value))

all_burd_educ <- all_burd %>% 
  filter( Education != 666) %>%
  group_by(Year) %>%
  summarise(overall_value = sum(overall_value))

test1 <- inner_join(all_burd_educ, all_burd_race, by = "Year") %>%
  mutate(asf = overall_value.y/overall_value.x)
##---population-----
pop_size <- read.csv("~/Desktop/paper2021/data/14_summary/pop_summary.csv")
pop_size <- pop_size %>%
  filter(agr_by == "nation" &
           Gender.Code == "All genders")

pop_size_race <- pop_size %>%
  filter( Education == "666"& source2 == "Official Bridged-Race Population Estimates") %>%
  group_by(Year) %>%
  summarise(Population = sum(Population))

pop_size_educ <- pop_size %>%
  filter( Education != "666" ) %>%
  group_by(Year) %>%
  summarise(Population = sum(Population))

test2 <- inner_join(pop_size_educ, pop_size_race, by = "Year") %>%
  mutate(asf = Population.y/Population.x)
