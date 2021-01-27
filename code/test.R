test<-attrBurden_gr%>%
  left_join(all_burden, by = unname(inverse_group_variables)) 

any(is.na(test))
