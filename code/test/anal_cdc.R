#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

testDir <- "~/Desktop/paper2021/data/test"
plotDir <- file.path(testDir, "CDC summary plot")
dir.create(plotDir, recursive = T, showWarnings = F)

dataDir <- "/Users/default/Desktop/paper2021/data"
lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
### ---- load calculated dataa-----
allBurdenDir <- "~/Desktop/paper2021/data/test/CDC summary"

files <- list.files(allBurdenDir)
all_burden <- lapply(files, function(file) {
  fileDir <- file.path(allBurdenDir, file)
  all_burden <- read.delim(fileDir)

  notes_hisp_or <- all_burden$Notes[grepl("Hispanic Origin:", all_burden$Notes, fixed = TRUE)]

  all_burden <- all_burden[!apply(is.na(all_burden) | all_burden == "", 1, all), ]

  if (!"Hispanic.Origin" %in% colnames(all_burden)) {
    if (rlang::is_empty(notes_hisp_or)) {
      all_burden[, "Hispanic.Origin"] <- "All Origins"
    } else if (grepl("Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
      all_burden[, "Hispanic.Origin"] <- "Hispanic or Latino"
    } else if (grepl("Not Hispanic or Latino", notes_hisp_or, fixed = TRUE)) {
      all_burden[, "Hispanic.Origin"] <- "Not Hispanic or Latino"
    }
  }

  all_burden <- all_burden %>% select(Race, Year, Deaths, Population, Crude.Rate, Hispanic.Origin, Single.Year.Ages.Code)
  return(all_burden)
})

all_burden <- all_burden %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) 

all_burden <- all_burden %>%
  filter(Population != "Not Applicable",
         ) %>%
  filter(Ethnicity %in% c("White, Not Hispanic or Latino", 
                          "White, Hispanic or Latino", 
                          "Black or African American, All Origins",
                          "Asian or Pacific Islander, All Origins",
                          "American Indian or Alaska Native, All Origins"))

#unique(all_burden$Single.Year.Ages.Code)
all_burden<-all_burden%>%
  mutate(
    Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
    Deaths = as.numeric(Deaths),
    Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(Single.Year.Ages.Code, lifeExpectancy$Age)],
    YLL = Deaths * Life.Expectancy
  )

all_burden <- all_burden  %>%
  group_by(Ethnicity, Year) %>%
  summarise(Deaths = sum(Deaths),
            YLL = sum(YLL),
            Population = sum(as.numeric(Population)),
            Crude.Rate.Deaths = 100000* Deaths/Population,
            Crude.Rate.YLL = 100000* YLL/Population)
##--- plot ----
columns <- c("Deaths", "Population", "Crude.Rate.Deaths", "YLL", "Crude.Rate.YLL")
for(column in columns){
  g <- ggplot(all_burden, aes_string(x = "Year", y = column)) +
    geom_line(aes(color = Ethnicity), size = 1) +
    ylab(paste(column)) +
    xlab("Year") +
    xlim(2000, 2016) +
    ylim(0,NA)+
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin())  +
    guides(col = guide_legend(nrow = 3, byrow = TRUE))
  g
  ggsave(file.path(plotDir, paste0(column,".png")), plot = g)
}