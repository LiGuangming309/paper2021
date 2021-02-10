packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes","xlsx")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

# Single.Year.Ages.Code
dataDir <- "/Users/default/Desktop/paper2021/data"
lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))

## add corresponding age_group_id from causes ages
age <- c(19:26,92:96)

corresponding_life_expectancy_age <- lifeExpectancy$Age[findInterval(age, lifeExpectancy$Age)]
life_expectancy <- lifeExpectancy$Life.Expectancy[findInterval(age, lifeExpectancy$Age)]

corresponding_age_group_id <- sapply(age,function(max_age){seq(25, 95, 5)[
  findInterval(
    max_age,
    seq(25, 90, 5),
    left.open =  TRUE
  ) + 1
]})

corresponding_mrbrt_file <- paste0("cvd_ihd_",corresponding_age_group_id,".csv")

df<-cbind(age, corresponding_mrbrt_file,corresponding_life_expectancy_age,life_expectancy) %>% as.data.frame()
df

write.xlsx(df, "/Users/default/Desktop/ages.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
