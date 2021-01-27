###---- analyse suppression ------
suppressedRows <- sum(total_burden$Deaths == "Suppressed")
suppressedRowsPerc <- (100*suppressedRows/nrow(total_burden)) %>% round
print(paste0(suppressedRows," (",suppressedRowsPerc,"%) rows suppressed in total burden data in "))
total_burden <- total_burden %>% filter(Deaths != "Suppressed")

#calculate YLL
total_burden<-total_burden%>%
  mutate(
    Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
    Deaths = as.numeric(Deaths),
    Life.Expectancy = ifelse(Gender.Code == "M", 80, 82.5),
    YLL = Deaths*(abs(Life.Expectancy - Single.Year.Ages.Code)+(Life.Expectancy - Single.Year.Ages.Code))/2
  )