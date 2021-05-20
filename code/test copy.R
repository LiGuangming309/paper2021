total_burden_cause <- apply(causes, 1, function(cause){
  label_cause1 <- cause[1]
  icd10_cause <- cause[2] %>% unlist
  total_burden_cause <- total_burden %>%
    filter(substring(label_cause, 1, 3) %in% icd10_cause) %>%
    group_by_at(setdiff(inverse_selectcolumns, "label_cause")) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(label_cause = label_cause1,
           attr = "total")
}) %>% rbindlist
