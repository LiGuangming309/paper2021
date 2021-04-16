total_burden_wond <- read.csv("~/Desktop/paper2021/data/09_total_burden_parsed/STATEFP/total_burden_wond.csv")
total_burden_wond <- total_burden_wond %>% select(min_age, max_age) %>% distinct


pop_nation <- read.csv("~/Desktop/paper2021/data/11_population_summary/pop_nation.csv") 
pop_nation <- pop_nation %>% select(min_age, max_age) %>% distinct

test <- data.frame(min_age = seq(25,65,10),
                   max_age = c(seq(34,64,10),150))

