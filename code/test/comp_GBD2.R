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
plotDir <- file.path(testDir, "GBD_compare_plots")
dir.create(plotDir, recursive = T, showWarnings = F)
### ---- load calculated attributable data-----
attr_burd <- lapply(2000:2016, function(year) {
  attr_burd <- paste0("~/Desktop/paper2021/data/10_attr_burd/nation/attr_burd_", toString(year), ".csv") %>% read.csv()
  attr_burd2 <- attr_burd %>%
    filter(Hispanic.Origin == "All Origins") %>%
    group_by(Year, label_cause) %>%
    summarise(
      Deaths = sum(Deaths),
      YLL = sum(YLL),
      attrDeaths = sum(attrDeaths),
      attrYLL = sum(attrYLL)
    )
}) %>% do.call(rbind, .)

attr_burd <- attr_burd %>%
  pivot_longer(
    cols = -c(Year, label_cause),
    names_to = "measure_name",
    values_to = "val"
  ) %>%
  mutate(
    measure = sapply(measure_name, function(measure_name) {
      if (measure_name %in% c("Deaths", "attrDeaths")) {
        "Deaths"
      } else {
        "YLL"
      }
    }),

    attributable = sapply(measure_name, function(measure_name) {
      if (measure_name %in% c("attrDeaths", "attrYLL")) {
        "directly attributable to pm exposure"
      } else {
        "total"
      }
    }),
    measure_name = NULL
  )
attr_burd$source <- "my code"

attr_burd <- attr_burd %>% rename(
  cause_name = label_cause,
  year = Year,
  measure_name = measure
)
## -- load summary----
Code_overall <- read.csv("~/Desktop/paper2021/data/12_summary/nation/attr_burd.csv") %>%
  filter(Hispanic.Origin == "All Origins") %>%
  group_by(Year) %>%
  summarise(
    allYLL = sum(allYLL),
    allDeaths = sum(allDeaths)
  ) %>%
  pivot_longer(
    cols = -c(Year),
    names_to = "measure_name",
    values_to = "val"
  )

Code_overall[Code_overall == "allYLL"] <- "YLL"
Code_overall[Code_overall == "allDeaths"] <- "Deaths"
Code_overall$attributable <- "overall"
Code_overall$source <- "my code"
Code_overall$cause_name <- "All causes"

Code_overall <- Code_overall %>% rename(year = Year)
## ---- load GBD data -----
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/30168d6453ca5d5037b5b377b448a055
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/946736cbf0b2ea57e0511e9931ec5602
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/ecf4adb8bcde1a5ef37337ffb6b2e51f

GBD_attributable <- read.csv("~/Desktop/paper2021/data/test/IHME/GBD_attributable.csv")
if ("rei_name" %in% colnames(GBD_attributable)) GBD_attributable$attributable <- "directly attributable to pm exposure"
GBD_attributable <- GBD_attributable %>%
  filter(
    metric_name == "Number",
    measure_name %in% c("Deaths", "YLLs (Years of Life Lost)")
  ) %>%
  select(cause_name, year, measure_name, val, attributable)
GBD_attributable$source <- "GBD"

GBD_total <- read.csv("~/Desktop/paper2021/data/test/IHME/GBD_total.csv")
# if(!("rei_name" %in% colnames(GBD_overall)))
GBD_total$attributable <- "total"
GBD_total <- GBD_total %>%
  filter(
    metric_name == "Number",
    measure_name %in% c("Deaths", "YLLs (Years of Life Lost)")
  ) %>%
  select(cause_name, year, measure_name, val, attributable)
GBD_total$source <- "GBD"

GBD_overall <- read.csv("~/Desktop/paper2021/data/test/IHME/GBD_overall.csv") %>%
  filter(
    metric_name == "Number",
    measure_name %in% c("Deaths", "YLLs (Years of Life Lost)")
  ) %>%
  select(cause_name, year, measure_name, val)
GBD_overall$attributable <- "overall"
GBD_overall$source <- "GBD"
#### ---- replace ----
burden <- rbind(
  attr_burd,
  GBD_attributable,
  GBD_total,
  GBD_overall,
  Code_overall
)

burden[burden == "cvd_ihd"] <- "Ischemic heart disease"
burden[burden == "cvd_stroke"] <- "Stroke"
burden[burden == "lri"] <- "Lower respiratory infections"
burden[burden == "neo_lung"] <- "Tracheal, bronchus, and lung cancer"
burden[burden == "resp_copd"] <- "Chronic obstructive pulmonary disease"
burden[burden == "t2_dm"] <- "Diabetes mellitus type 2"

burden[burden == "YLL"] <- "YLLs (Years of Life Lost)"
#### ------ compare/plot-----
i <- 1
# plot not disease-specific
for (measure in unique(burden$measure_name)) {
  for (attr in unique(burden$attributable)) {
    burden_sub <- burden %>%
      filter(
        measure_name == measure,
        attributable == attr
      ) %>%
      group_by(year, source) %>%
      summarise(val = sum(val))

    g <- ggplot(burden_sub, aes(x = year, y = val)) +
      geom_line(aes(linetype = source), size = 1) +
      ylab(paste(measure)) +
      xlab("Year") +
      xlim(2000, 2016) +
      ylim(0, NA) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      ggtitle(paste(measure, attr, "all causes", sep = "; "))
    g

    ggsave(paste0(plotDir, "/plot", i, ".png"), plot = g)
    i <- i + 1
  }
}

# plot disease specific
for (measure in unique(burden$measure_name)) {
  for (attr in c("total", "directly attributable to pm exposure")) {
    causes <- setdiff(unique(burden$cause_name), "All causes")
    for (cause in causes) {
      burden_sub <- burden %>%
        filter(
          measure_name == measure,
          attributable == attr,
          cause_name == cause
        )

      g <- ggplot(burden_sub, aes(x = year, y = val)) +
        geom_line(aes(linetype = source), size = 1) +
        ylab(paste(measure)) +
        xlab("Year") +
        xlim(2000, 2016) +
        ylim(0, NA) +
        theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
        ggtitle(paste(measure, attr, cause, sep = "; "))
      g

      ggsave(paste0(plotDir, "/plot", i, ".png"), plot = g)
      i <- i + 1
    }
  }
}

# plot effective PAF
# burden_sub <- burden %>% filter(attributable %in% c("total", "directly attributable to pm exposure"))
#TODO
for (measure in unique(burden$measure_name)) {
  causes <- setdiff(unique(burden$cause_name), "All causes")
  for (cause in causes) {
    burden_sub <- burden %>%
      filter(
        measure_name == measure,
        cause_name == cause,
        attributable %in% c("total", "directly attributable to pm exposure")
      ) %>%
      group_by(year, cause_name, source) %>%
      summarise(val = min(val) / max(val))

    g <- ggplot(burden_sub, aes(x = year, y = val)) +
      geom_line(aes(linetype = source), size = 1) +
      ylab(paste(measure)) +
      xlab("Year") +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      ggtitle(paste("effective PAF", measure, cause, sep = "; "))
    g

    ggsave(paste0(plotDir, "/plot", i, ".png"), plot = g)
    i <- i + 1
  }
}

#plot proportion
for (measure in unique(burden$measure_name)) {
  burden_sub <- burden %>%
    filter(
      measure_name == measure,
      attributable %in% c("overall", "directly attributable to pm exposure")
    ) %>%
    group_by(attributable, source, year) %>%
    summarise(val = sum(val))
    
  burden_sub <- burden_sub %>%
    group_by(source, year) %>%
    summarise(prop = min(val) / max(val) *100)
  
    g <- ggplot(burden_sub, aes(x = year, y = prop)) +
      geom_line(aes(linetype = source), size = 1) +
      ylab("%") +
      xlab("Year") +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      ggtitle(paste("proportion overall health burden attributabel", measure, sep = "; "))
    g
    
    ggsave(paste0(plotDir, "/plot", i, ".png"), plot = g)
    i <- i + 1
}
