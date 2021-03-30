replacement <- total_burden %>%
  select(all_of(replacecolumn)) %>%
  anti_join(findreplace,
            by = setNames("from", replacecolumn)
  ) 

a <- c(123,NA)
a
replace_na(a, 5)
