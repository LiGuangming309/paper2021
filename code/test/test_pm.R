packages <- c("dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes", "stats","matrixStats")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# TODO delete
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp/"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog/"
  dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr/"

dir.create("C:/Users/Daniel/Desktop/paper2021/data/test/pm/", recursive = T, showWarnings = F)

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
  rbindlist() %>%
  as.data.frame


dem_agr <- dem_agr %>%
  filter(Gender.Code == "A") %>%
  group_by(Year,Race, Hispanic.Origin, Education, pm) %>%
  summarise(pop_size = sum(pop_size))

dem_agr <- dem_agr %>%
  group_by(Year, Race, Education, Hispanic.Origin) %>%
  summarise(mean = weighted.mean(pm, pop_size),
         median = matrixStats::weightedMedian(pm, pop_size))

write.csv(dem_agr, "C:/Users/Daniel/Desktop/paper2021/data/test/pm/pm.csv")
toc()

## --plot ---
dem_agr$Ethnicity <- paste0(dem_agr$Race, ", ",dem_agr$Hispanic.Origin)
dem_agr1 <- dem_agr %>% filter(Ethnicity %in% c("White, Not Hispanic or Latino",
                                         "White, Hispanic or Latino",
                                         "Black or African American, All Origins",
                                         "Asian or Pacific Islander, All Origins",
                                         "American Indian or Alaska Native, All Origins"))
  
g <- ggplot(dem_agr1, aes(x = Year, y = mean)) +
  geom_line(aes(color = Ethnicity), size = 1) + 
  ylab(paste("pm exposure")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))+
  ggtitle("population-weighted mean")

ggsave("C:/Users/Daniel/Desktop/paper2021/data/test/pm/pm_mean_ethn.png", plot = g)

g <- ggplot(dem_agr1, aes(x = Year, y = median)) +
  geom_line(aes(color = Ethnicity), size = 1) + 
  ylab(paste("pm exposure")) +
  xlab("Year") +
  xlim(2000, 2016) +
  ylim(0, NA) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
  ggtitle("population-weighted median")

ggsave("C:/Users/Daniel/Desktop/paper2021/data/test/pm/pm_median_ethn.png", plot = g)

dem_agr2 <- dem_agr %>% filter(Education != 666)
replaces3 <- data.frame(
  Education = c(1:7, 666),
  Education2 = c(
    "Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative",
    "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"
  )
)
dem_agr2 <- dem_agr2 %>% left_join(replaces3, by = "Education")

g <- ggplot(dem_agr2, aes(x = Year, y = mean)) +
  geom_line(aes(color = Education2), size = 1) + 
  ylab(paste("pm exposure")) +
  xlab("Year") +
  xlim(2000, 2016) +
  ylim(0, NA) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE))+
  ggtitle("population-weighted mean")

ggsave("C:/Users/Daniel/Desktop/paper2021/data/test/pm/pm_mean_educ.png", plot = g)

g <- ggplot(dem_agr2, aes(x = Year, y = median)) +
  geom_line(aes(color = Education2), size = 1) + 
  ylab(paste("pm exposure")) +
  xlab("Year") +
  xlim(2000, 2016) +
  ylim(0, NA) +
  # scale_color_viridis(discrete = TRUE) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 4, byrow = TRUE)) +
  ggtitle("population-weighted median")

ggsave("C:/Users/Daniel/Desktop/paper2021/data/test/pm/pm_median_educ.png", plot = g)
