packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

label_causes_all <- c("resp_copd", "lri", "neo_lung", "t2_dm", "cvd_ihd", "Cvd_stroke")
Cvd_stroke_join<-lapply(label_causes_all, function(label_cause){
  #https://wonder.cdc.gov/controller/saved/D77/D105F096
  Cvd_stroke1 <- read.delim(paste0("~/Desktop/paper2021/data/08_total_burden/nation/",label_cause,".txt"))
  #https://wonder.cdc.gov/controller/saved/D77/D107F342
  Cvd_stroke2 <- read.delim(paste0("~/Desktop/paper2021/data/test/",label_cause,".txt"))
  
  #TODO age not sted?
  Cvd_stroke1 <- Cvd_stroke1 %>%
    filter(Deaths != "Suppressed" 
           #Single.Year.Ages != "Not Stated"
    ) %>%
    mutate(Deaths = as.numeric(Deaths)) %>%
    group_by(Race,Hispanic.Origin) %>%
    summarise(Deaths =sum(Deaths)) %>%
    filter(!is.na(Deaths))
  
  Cvd_stroke2 <- Cvd_stroke2 %>%
    filter(Deaths != "Suppressed") %>%
    mutate(Deaths = as.numeric(Deaths),
           Population = as.numeric(Population),
           Crude.Rate = as.numeric(Crude.Rate)) %>%
    group_by(Race,Hispanic.Origin) %>%
    summarise(Deaths =sum(Deaths),
              Population = sum(Population),
              Crude.Rate = sum(Crude.Rate))
  
  Cvd_stroke_join <- left_join(Cvd_stroke1, Cvd_stroke2, by= c("Race","Hispanic.Origin"))%>%
    mutate(suppressed = 100-100*Deaths.x/Deaths.y)
  
  Cvd_stroke_join$label_cause<-label_cause
  return(Cvd_stroke_join)
})%>% do.call(rbind,.)


Cvd_stroke_join2 <- Cvd_stroke_join %>%
  group_by(Race,Hispanic.Origin) %>%
  summarise(Deaths.x =sum(Deaths.x),
            Deaths.y =sum(Deaths.y),
            Population = sum(Population),
            Crude.Rate = sum(Crude.Rate))

Cvd_stroke_join2 <- Cvd_stroke_join2 %>%
  mutate(suppressed = 100-100*Deaths.x/Deaths.y,
         factor = Deaths.y/Deaths.x)

Cvd_stroke_join3 <- Cvd_stroke_join2 %>% select(Race, Hispanic.Origin, Deaths.y, suppressed, factor)

fwrite(Cvd_stroke_join, 
       "~/Desktop/paper2021/data/test/test1.csv",
       sep = ';')

fwrite(Cvd_stroke_join2, 
          "~/Desktop/paper2021/data/test/test2.csv",
          sep = ';')

#Cvd_stroke_join3 <- Cvd_stroke_join2 %>% select(Race, Hispanic.Origin, Deaths.y, suppressed, Crude.Rate)
Cvd_stroke_join4 <- Cvd_stroke_join %>%
  group_by(label_cause) %>%
  summarise(Crude.Rate = sum(Crude.Rate))


