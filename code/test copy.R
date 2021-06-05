total_burden_age_adj <- crossing(total_burden_age_adj, standartpopulation)

total_burden_age_adj1 <- total_burden_age_adj %>%
  filter(min_age <= standard_min_age & standard_max_age <= max_age) %>% 
  mutate(
    min_age = pmin(min_age, standard_min_age), max_age = pmax(max_age, standard_max_age),
    standard_min_age = NULL, standard_max_age = NULL
  ) 

total_burden_age_adj1 <- total_burden_age_adj1 %>%
  group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj1),"standard_popsize")))) %>% 
  summarise(standard_popsize = sum(standard_popsize)) %>%
  ungroup()

total_burden_age_adj2 <- total_burden_age_adj %>%
  filter(standard_min_age < min_age & max_age < standard_max_age) %>% 
  mutate(
    min_age = pmin(min_age, standard_min_age), max_age = pmax(max_age, standard_max_age),
    standard_min_age = NULL, standard_max_age = NULL
  ) 

total_burden_age_adj2 <- total_burden_age_adj2 %>%
  group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj2),"value")))) %>% #Population
  summarise(value = sum(value)) %>%
  ungroup()

#combine cases
total_burden_age_adj <- rbind(total_burden_age_adj1, total_burden_age_adj2) %>% distinct
rm(total_burden_age_adj1, total_burden_age_adj2)

#calculate age-adjusted rate
total_burden_age_adj <- total_burden_age_adj %>%
  dplyr::mutate(
    value = value * (standard_popsize/ Population) * (100000 / full_stand_popsize), 
    measure2 = "age-adjusted rate",
    Population = NULL
  )