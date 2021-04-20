#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/23/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc", "viridis", "hrbrthemes")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
agr_by <- args[2]
summaryDir <- args[6]
plotDir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_attr_burd"
  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/11_summary"
  # plotDir <- "C:/Users/Daniel/Desktop/paper2021/data/12_plot"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  plotDir <- "/Users/default/Desktop/paper2021/data/15_plot"
}
summaryDir <- file.path(summaryDir, agr_by)
plotDir <- file.path(plotDir, agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)

attrBurden <- fread(file.path(summaryDir, "attr_burd.csv"))
allBurden <- fread(file.path(summaryDir, "all_burd.csv"))

attrBurden <- attrBurden %>% filter(Gender.Code == "A")
allBurden <- allBurden %>% filter(Gender.Code == "A")

agr_by_replace <- c(
  "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
  "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "nation", "county" = "County.Code"
)
agr_by_new <- agr_by_replace[[agr_by]]
## -----by race -----
for (location in attrBurden[, get(agr_by_new)] %>% unique()) {
  for (sourceX in attrBurden$source %>% unique()) {
    plotDirX <- file.path(plotDir, sourceX)
    dir.create(plotDirX, recursive = T, showWarnings = F)

    ### -------plot 1 ---------
    allBurdenX <- allBurden %>%
      filter(Ethnicity %in% c(
        "White, Not Hispanic or Latino",
        "White, Hispanic or Latino",
        "Black or African American, All Origins",
        "Asian or Pacific Islander, All Origins",
        "American Indian or Alaska Native, All Origins"
      )) %>%
      filter(get(agr_by_new) == location, source == sourceX)
    allBurden1 <- allBurdenX %>% filter(measure1 == "YLL", measure2 == "crude rate")

    g <- ggplot(allBurden1, aes(x = Year, y = overall_value)) +
      geom_line(aes(color = Ethnicity), size = 1) +
      ylab(paste("YLL per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("crude rate YLL from all causes in", location))

    ggsave(file.path(plotDirX, paste0(location, "_race_plot1.png")), plot = g)

    ### -------plot 2 ---------
    allBurden2 <- allBurdenX %>% filter(measure1 == "Deaths", measure2 == "age-adjusted rate")

    g <- ggplot(allBurden2, aes(x = Year, y = overall_value)) +
      geom_line(aes(color = Ethnicity), size = 1) +
      ylab(paste("Deaths per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("age-adjusted Deaths from all causes in", location))

    ggsave(file.path(plotDirX, paste0(location, "_race_plot2.png")), plot = g)

    rm(allBurdenX, allBurden1, allBurden2)
    ### -------plot 3 ---------
    attrBurdenX <- attrBurden %>%
      filter(Ethnicity %in% c(
        "White, Not Hispanic or Latino",
        "White, Hispanic or Latino",
        "Black or African American, All Origins",
        "Asian or Pacific Islander, All Origins",
        "American Indian or Alaska Native, All Origins"
      )) %>%
      filter(get(agr_by_new) == location, source == sourceX)

    attrBurden1 <- attrBurdenX %>%
      filter(
        measure1 == "YLL",
        measure2 == "crude rate",
        attr == "attributable"
      )

    g <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Ethnicity)) +
      geom_line(size = 1) +
      ylab(paste("YLL per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("crude rate YLL directly attributable to PM exposure", location))

    ggsave(file.path(plotDirX, paste0(location, "_race_plot3.png")), plot = g)

    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    ggsave(file.path(plotDirX, paste0(location, "_race_plot3_conf.png")), plot = g)

    ### -------plot 4 ---------
    attrBurden2 <- attrBurdenX %>%
      filter(measure1 == "Deaths", measure2 == "age-adjusted rate")

    g <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Ethnicity)) +
      geom_line(size = 1) +
      ylab(paste("Deaths per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("age-adjusted Deaths directly attributable to PM exposure", location))

    ggsave(file.path(plotDirX, paste0(location, "_race_plot4.png")), plot = g)

    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    ggsave(file.path(plotDirX, paste0(location, "_race_plot4_conf.png")), plot = g)


    ### -------plot 5 -------
    attrBurden3 <- attrBurdenX %>%
      filter(
        measure1 == "YLL",
        measure2 == "prop. of overall burden",
        attr == "attributable"
      )

    g <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Ethnicity)) +
      geom_line(size = 1) +
      ylab(paste("%")) +
      xlab("Year") +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle("proportion of all YLL directly attributable to PM exposure")

    ggsave(file.path(plotDirX, paste0(location, "_race_plot5.png")), plot = g)
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    ggsave(file.path(plotDirX, paste0(location, "_race_plot5_conf.png")), plot = g)

    rm(attrBurdenX, attrBurden1, attrBurden2, attrBurden3)
  }
}

## -----by Education -----
for (location in attrBurden[, get(agr_by_new)] %>% unique()) {
  plotDirX <- file.path(plotDir, sourceX)
  dir.create(plotDirX, recursive = T, showWarnings = F)

  allBurdenX <- allBurden %>%
    filter(Education != 666) %>%
    filter(get(agr_by_new) == location, source == "nvss")

  ### -------plot 1 ---------
  allBurden1 <- allBurdenX %>% filter(measure1 == "YLL", measure2 == "crude rate")

  # https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
  # https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
  g <- ggplot(allBurden1, aes(x = Year, y = overall_value)) +
    geom_line(aes(color = Education), size = 1) +
    ylab(paste("YLL per 100.000")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("crude rate YLL from all causes in", location))

  ggsave(file.path(plotDirX, paste0(location, "_educ_plot1.png")), plot = g)

  ### -------plot 2 ---------
  allBurden2 <- allBurdenX %>% filter(measure1 == "Deaths", measure2 == "age-adjusted rate")

  g <- ggplot(allBurden2, aes(x = Year, y = overall_value)) +
    geom_line(aes(color = Education), size = 1) +
    ylab(paste("Deaths per 100.000")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("age-adjusted Deaths from all causes in", location))

  ggsave(file.path(plotDirX, paste0(location, "_educ_plot2.png")), plot = g)

  ### -------plot 3 ---------
  attrBurdenX <- attrBurden %>%
    filter(Education != 666) %>%
    filter(get(agr_by_new) == location, source == "nvss")

  attrBurden1 <- attrBurdenX %>%
    filter(
      measure1 == "YLL",
      measure2 == "crude rate",
      attr == "attributable"
    )

  g <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Education)) +
    geom_line(size = 1) +
    ylab(paste("YLL per 100.000")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("crude rate YLL directly attributable to PM exposure", location))

  ggsave(file.path(plotDirX, paste0(location, "_educ_plot3.png")), plot = g)

  g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
  ggsave(file.path(plotDirX, paste0(location, "_educ_plot3_conf.png")), plot = g)

  ### -------plot 4 ---------
  attrBurden2 <- attrBurdenX %>%
    filter(measure1 == "Deaths", measure2 == "age-adjusted rate")

  g <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Education)) +
    geom_line(size = 1) +
    ylab(paste("Deaths per 100.000")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("age-adjusted Deaths directly attributable to PM exposure", location))

  ggsave(file.path(plotDirX, paste0(location, "_educ_plot4.png")), plot = g)

  g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
  ggsave(file.path(plotDirX, paste0(location, "_educ_plot4_conf.png")), plot = g)


  ### -------plot 5 -------
  attrBurden3 <- attrBurdenX %>%
    filter(
      measure1 == "YLL",
      measure2 == "prop. of overall burden",
      attr == "attributable"
    )

  g <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Education)) +
    geom_line(size = 1) +
    ylab(paste("%")) +
    xlab("Year") +
    xlim(2000, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle("proportion of all YLL directly attributable to PM exposure")

  ggsave(file.path(plotDirX, paste0(location, "_educ_plot5.png")), plot = g)
  g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
  ggsave(file.path(plotDirX, paste0(location, "_educ_plot5_conf.png")), plot = g)
}
