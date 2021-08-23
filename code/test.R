a <- setdiff(colnames(attrBurden), c("Race", "Hispanic.Origin", "lower", "mean", "upper"))
b<- c("Race.x", "Hispanic.Origin.x", "Race.y", "Hispanic.Origin.y", "Year", "Gender.Code", "Education", "Region", "measure1", "measure2", "source", "agr_by")

setdiff(a,b)


test <- all_burden %>% filter(is.infinite(overall_value))
test %>%
  mutate_at(c("Race", "Hispanic.Origin", "Education", "rural_urban_class", "Gender.Code", "measure1", "measure2"), as.factor) %>%
  select(Race, Hispanic.Origin, Education, rural_urban_class, Gender.Code, measure1, measure2)#%>% 
  #sapply(levels)
