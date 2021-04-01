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
summaryDir <- args[7]
plotDir <- args[8]

# TODO delete
if (rlang::is_empty(args)) {
  agr_by <- "nation"

  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  # attrBurdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/09_attr_burd"
  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/11_summary"
  # plotDir <- "C:/Users/Daniel/Desktop/paper2021/data/12_plot"

  tmpDir <- "/Users/default/Desktop/paper2021/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  attrBurdenDir <- "/Users/default/Desktop/paper2021/10_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/12_summary"
  plotDir <- "/Users/default/Desktop/paper2021/data/13_plot"
}
summaryDir <- file.path(summaryDir, agr_by)
plotDir <- file.path(plotDir, agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)

attrBurden <- fread(file.path(summaryDir, "attr_burd.csv"))
attrBurden <- attrBurden %>%
  mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
  filter(Ethnicity %in% c(
    "White, Not Hispanic or Latino",
    "White, Hispanic or Latino",
    "Black or African American, All Origins",
    "Asian or Pacific Islander, All Origins",
    "American Indian or Alaska Native, All Origins"
  )) #

agr_by_replace <- c(
  "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
  "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "Nation", "county" = "County.Code"
)
agr_by_new <- agr_by_replace[[agr_by]]

for (location in attrBurden[, get(agr_by_new)] %>% unique()) {
  for (sourceX in attrBurden$source %>% unique()) {
    plotDirX <- file.path(plotDir, sourceX)
    dir.create(plotDirX, recursive = T, showWarnings = F)
    attrBurdenX <- attrBurden %>% filter(
      get(agr_by_new) == location,
      source == sourceX
    )

    ### -------plot 1 ---------
    attrBurden1 <- attrBurdenX %>%
      filter(
        measure == "YLL",
        #measure == "Deaths",
        measure2 == "crude rate"
      )

    # https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
    # https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
    g <- ggplot(attrBurden1, aes(x = Year, y = overall_value)) +
      geom_line(aes(color = Ethnicity), size = 1) +
      # scale_color_manual(values = c("green","orange", "steelblue", "purple"))+
      # scale_linetype_manual(values = c("dashed", "solid")) +
      ylab(paste("YLL per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("YLL from all causes in", location))

    ggsave(file.path(plotDirX, paste0(location, "_plot1.png")), plot = g)

    ### -------plot 2 ---------
    attrBurden2 <- attrBurdenX %>%
      filter(
        measure == "YLL",
        measure2 == "crude rate",
        attr == "attributable"
      )

    # https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
    # https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot

    g <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Ethnicity)) +
      geom_line(size = 1) +
      # scale_color_manual(values = c("green","yellow", "steelblue"))+
      # scale_linetype_manual(values = c("dashed", "solid")) +
      ylab(paste("YLL per 100.000")) +
      xlab("Year") +
      ylim(0, NA) +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle(paste("YLL directly attributable to PM exposure", location))

    ggsave(file.path(plotDirX, paste0(location, "_plot2.png")), plot = g)

    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    ggsave(file.path(plotDirX, paste0(location, "_plot2_conf.png")), plot = g)
    ### -------plot 3 -------
    # attrBurden_gr2 <- attrBurden_gr %>% select(Year,Ethnicity, crudeAllYLL,crudeAttrYLL)
    # attrBurden_gr3<-inner_join(attrBurden_gr2,attrBurden_gr2,by = "Year") %>%
    #  mutate(test1 = (crudeAttrYLL.x-crudeAttrYLL.y),
    #         test2 = (crudeAllYLL.x-crudeAllYLL.y),
    #         test3 = 100*(crudeAttrYLL.x-crudeAttrYLL.y)/(crudeAllYLL.x-crudeAllYLL.y))

    # attrBurden_gr3 <- attrBurden_gr3 %>%
    #  filter(Ethnicity.x == "American Indian or Alaska Native, All Origins",
    #         (Ethnicity.y %in% c("White, Not Hispanic or Latino",
    #                             "White, Hispanic or Latino",
    #                             "Black or African American, All Origins",
    #                             "Asian or Pacific Islander, All Origins"#,
    #                             #"American Indian or Alaska Native, All Origins"
    #         )))

    # g <- ggplot(attrBurden_gr3, aes(x = Year, y = test3)) +
    #  geom_line(aes(color = Ethnicity.y), size = 1) +
    #  ylab(paste("%")) +
    #  xlab("Year") +
    #  xlim(2000, 2016) +
    #  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    #  guides(col = guide_legend(nrow = 2, byrow = TRUE))
    # g
    # ggsave(file.path(plotDirX, paste0("plot3.png")), plot = g)

    ### -------plot 4 -------
    attrBurden4 <- attrBurdenX %>%
      filter(
        measure == "YLL",
        measure2 == "crude rate",
        attr == "attributable"
      ) %>%
      mutate(
        mean = 100 * mean / overall_value,
        lower = 100 * lower / overall_value,
        upper = 100 * upper / overall_value
      )

    g <- ggplot(attrBurden4, aes(x = Year, y = mean, color = Ethnicity)) +
      geom_line(size = 1) +
      ylab(paste("%")) +
      xlab("Year") +
      xlim(2000, 2016) +
      theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
      guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
      ggtitle("proportion of all YLL directly attributable to PM exposure")

    ggsave(file.path(plotDirX, paste0(location, "_plot4.png")), plot = g)
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    ggsave(file.path(plotDirX, paste0(location, "_plot4_conf.png")), plot = g)
  }
}
