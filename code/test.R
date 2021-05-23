#s1
total_burden_race <- total_burden %>% filter(Education == 666)
total_burden_educ <- total_burden %>% filter(Education != 666 & max_age <= 64)

nrow_education <- nrow(total_burden_educ)
prop_education_unknown <- nrow(total_burden_educ %>% filter(Education == 9))/nrow_education
print(paste(100*prop_education_unknown, "% of all education unknown"))
prop_education_1989 <- nrow(total_burden_educ %>% filter(Education == 10))/nrow_education
print(paste(100*prop_education_1989, "% of all education has 1989 education revision and thus not comparible"))

#counter, above effect
total_burden_educ <- total_burden_educ %>% filter(Education %in% c(1:7))
# prop <- 1-prop_education_unknown-prop_education_1989
prop <- 1-prop_education_1989
total_burden_educ$Deaths <- total_burden_educ$Deaths/prop

total_burden <- rbind(total_burden_race, total_burden_educ)

#s2
total_burden_race <- total_burden %>% filter(Education == 666)
total_burden_educ <- total_burden %>% filter(Education != 666)

nrow_education <- nrow(total_burden_educ)
prop_education_unknown <- nrow(total_burden_educ %>% filter(Education == 9))/nrow_education
print(paste(100*prop_education_unknown, "% of all education unknown"))
prop_education_1989 <- nrow(total_burden_educ %>% filter(Education == 10))/nrow_education
print(paste(100*prop_education_1989, "% of all education has 1989 education revision and thus not comparible"))

#counter, above effect
total_burden_educ <- total_burden_educ %>% filter(Education %in% c(1:7))
# prop <- 1-prop_education_unknown-prop_education_1989
prop <- 1-prop_education_1989
total_burden_educ$Deaths <- total_burden_educ$Deaths/prop

total_burden <- rbind(total_burden_race, total_burden_educ)