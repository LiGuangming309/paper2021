packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# TODO delete
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp/"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog/"
  dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr/"

  

dem_agrDir <- file.path(dem_agrDir, "nation")
states <- file.path(tmpDir, "states.csv") %>% read.csv()
## --------- read pm data -----------
tic(paste("aggregated pm data"))
dem_agr <- lapply(2000:2016, function(year) {
  tic(year)
    dem_agrDirY<-file.path(dem_agrDir,toString(year), paste0("cens_agr_", toString(year), "_us.csv"))
    dem_agr <-fread(dem_agrDirY)
  
  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  dem_agr<- dem_agr %>%
    left_join(meta, by = "variable")
  toc()
  return(dem_agr)
}) %>%
  do.call(rbind, .) %>%
  as.data.frame

dem_agr <- dem_agr %>%
  group_by(year,race, hispanic_origin,pm) %>%
  summarise(pop_size = sum(pop_size))

toc()
## --------- read demographic census data -----------
tic(paste("aggregated census data by"))
censData_agr <- lapply(2000:2016, function(year) {
  censData_agr <- apply(states, 1, function(state) {
    STUSPS <- state["STUSPS"]
    name <- state["NAME"]
    censData_agrDir<-file.path(censDir, year, paste0("agr_census_", toString(year), "_", STUSPS, ".csv"))
    censData_agr <-fread(censData_agrDir)
    return(censData_agr)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame
  
  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  censData_agr<- censData_agr %>%
    left_join(meta, by = "variable")

  return(censData_agr)
}) %>%
  do.call(rbind, .) %>%
  as.data.frame()

censData_agr <- censData_agr %>%
  group_by(year, race, hispanic_origin) %>%
  summarise(pop_size = sum(pop_size))

toc()
## --- calculate -----
join <- inner_join(dem_agr, censData_agr, by = c("year", "race", "hispanic_origin"))
join$prop <- join$pop_size.x/join$pop_size.y
join$mean <- join$prop * join$pm

join <- join %>%
  group_by(year, race, hispanic_origin) %>%
  summarise(mean = sum(mean))

write.csv(join, "C:/Users/Daniel/Desktop/paper2021/data/test/pm.csv")
## --plot ---
join$ethnicity <- paste0(join$race, ", ",join$hispanic_origin)
join <- join %>% filter(ethnicity %in% c("White, Not Hispanic or Latino",
                                         "White, Hispanic or Latino",
                                         "Black or African American, All Origins",
                                         "Asian or Pacific Islander, All Origins",
                                         "American Indian or Alaska Native, All Origins"))
  
g <- ggplot(join, aes(x = year, y = mean)) +
  geom_line(aes(color = ethnicity), size = 1) + 
  ylab(paste("mean pm exposure")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))

ggsave("C:/Users/Daniel/Desktop/paper2021/data/test/pm.png", plot = g)
