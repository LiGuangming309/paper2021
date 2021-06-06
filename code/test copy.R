 largest_age_interval <- apply(total_burden_age_adj, 1, function(row){
    total_burden_age <- interval(row[["min_age.x"]],row[["max_age.x"]], "[]")
    pop_summary_age <- interval(row[["min_age.y"]],row[["max_age.y"]], "[]")
    standard_pop_age <- interval(row[["standard_min_age"]],row[["standard_max_age"]], "[]")
    union_age <- interval(row[["min_age"]],row[["max_age"]], "[]")
    if(total_burden_age == union_age){
      return("total_burden")
    }else if(pop_summary_age == union_age){
      return("pop_summary")
    }else if(standard_pop_age == union_age){
      return("standard_pop")
    }else{
      return("none")
    }
  })

apply(total_burden_age_adj, 1, function(row) {
  min_age <- row[["min_age"]]
  max_age <- row[["max_age"]]
  if (min_age == row[["min_age.x"]] & max_age == row[["max_age.x"]]) {
    return("total_burden")
  } else if (min_age == row[["min_age.y"]] & max_age == row[["max_age.y"]]) {
    return("pop_summary")
  } else if (min_age == row[["standard_min_age"]] & max_age == row[["standard_max_age"]]) {
    return("standard_pop")
  } else {
    return("none")
  }
}) %>% as.factor() -> total_burden_age_adj$largest_age_interval

proper_subsets <- apply(total_burden_age_adj, 1, function(row){
  total_burden_age <- sets::interval(row[["min_age.x"]],row[["max_age.x"]], "[]")
  pop_summary_age <- sets::interval(row[["min_age.y"]],row[["max_age.y"]], "[]")
  standard_pop_age <- sets::interval(row[["standard_min_age"]],row[["standard_max_age"]], "[]")
  union_age <- sets::interval(row[["min_age"]],row[["max_age"]], "[]")
  proper_subsets <- c()
  if(sets::interval_is_proper_subinterval(total_burden_age, union_age)) proper_subsets <- c(proper_subsets, "ID_total_burden")
  if(sets::interval_is_proper_subinterval(pop_summary_age, union_age)) proper_subsets <- c(proper_subsets, "ID_pop_summary")
  if(sets::interval_is_proper_subinterval(standard_pop_age, union_age)) proper_subsets <- c(proper_subsets, "ID_standartpopulation")
  return(proper_subsets)
})
###----last----
# make ages of total burden and population compatible
total_burden_age_adj <- left_join(tibble::rowid_to_column(total_burden, "ID_total_burden"), 
                                  tibble::rowid_to_column(pop_summary, "ID_pop_summary"), 
                                  by = setdiff(colnames(pop_summary), c("min_age", "max_age", "Population")))
total_burden_age_adj <- crossing(total_burden_age_adj, 
                                 tibble::rowid_to_column(standartpopulation, "ID_standartpopulation"))

total_burden_age_adj <- total_burden_age_adj %>%
  mutate(
    min_age = pmin(min_age.x, min_age.y, standard_min_age),
    max_age = pmax(max_age.x, max_age.y, standard_max_age)
  ) %>%
  filter((min_age == min_age.x & max_age == max_age.x) |
           (min_age == min_age.y & max_age == max_age.y) |
           (min_age == standard_min_age & max_age == standard_max_age))

total_burden_age_adj$proper_subsets <- apply(total_burden_age_adj, 1, function(row){
  total_burden_age <- sets::interval(row[["min_age.x"]] %>% as.numeric,
                                     row[["max_age.x"]] %>% as.numeric, 
                                     "[]")
  pop_summary_age <- sets::interval(row[["min_age.y"]] %>% as.numeric,
                                    row[["max_age.y"]] %>% as.numeric, 
                                    "[]")
  standard_pop_age <- sets::interval(row[["standard_min_age"]] %>% as.numeric,
                                     row[["standard_max_age"]] %>% as.numeric, "[]")
  union_age <- sets::interval(row[["min_age"]] %>% as.numeric,
                              row[["max_age"]] %>% as.numeric,
                              "[]")
  proper_subsets <- c()
  if(sets::interval_is_proper_subinterval(total_burden_age, union_age)) proper_subsets <- c(proper_subsets, "ID_total_burden")
  if(sets::interval_is_proper_subinterval(pop_summary_age, union_age)) proper_subsets <- c(proper_subsets, "ID_pop_summary")
  if(sets::interval_is_proper_subinterval(standard_pop_age, union_age)) proper_subsets <- c(proper_subsets, "ID_standartpopulation")
  return(proper_subsets)
})
unique_proper_subsets <- unique(total_burden_age_adj$proper_subsets)
total_burden_age_adj <- total_burden_age_adj %>% subset(select = -c(min_age.x, max_age.x, min_age.y, max_age.y, standard_min_age, standard_max_age))

total_burden_age_adj <- lapply(unique_proper_subsets, function(proper_subsetsX){
  total_burden_age_adj_sub <- total_burden_age_adj %>%
    filter(proper_subsets == proper_subsetsX)
  
  group_by_cols <- setdiff(colnames(total_burden_age_adj_sub),
                           c(proper_subsetsX, "standard_popsize", "Population", "value")) 
  
  total_burden_age_adj_sub <- total_burden_age_adj_sub%>%
    group_by_at(vars(all_of(group_by_cols))) %>%
    summarise(standard_popsize = sum(standard_popsize),
              Population = sum(Population),
              value = sum(value))
  total_burden_age_adj_sub <- total_burden_age_adj_sub %>% subset(select = -c(ID_total_burden, ID_pop_summary, ID_standartpopulation))
}) %>% rbindlist