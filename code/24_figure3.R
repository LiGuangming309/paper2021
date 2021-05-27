#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "MALDIquant",
  "dplyr", "tidyr", "matrixStats", "truncnorm", "triangle", "directlabels"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"
  totalBurdenDir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
}
theme_set(theme_classic())
#--- reading total burden data---
files <- list.files(file.path(totalBurdenDir, "nation", "nvss"))
total_burden <- lapply(files, function(file) {
  total_burden <- fread(file.path(totalBurdenDir, "nation", "nvss", file))
  total_burden <- total_burden %>%
    filter(Gender.Code == "A" & measure1 == "Deaths" & measure2 == "absolute number" & source == "nvss" &
      Education == 666) %>%
    mutate(
      age_group_id = seq(25, 95, 5)[
        findInterval(
          max_age,
          seq(25, 90, 5),
          left.open =  TRUE
        )
      ]
    ) %>%
    group_by(Year, label_cause, age_group_id) %>%
    dplyr::summarise(value = sum(value))
  return(total_burden)
}) %>% rbindlist()
rm(files)

total_burden <- total_burden %>% filter(Year == 2016)

## --- GBD estimate----
gbd <- function(pms) {
  example_exp_rr <- file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv()
  pm_levels <- example_exp_rr$exposure_spline
  causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()

  attr_burd <- apply(causes_ages, 1, function(cause_age) {
    # subset rows of total burden
    label_causeX <- cause_age[["label_cause"]]
    age_group_idX <- cause_age[["age_group_id"]]
    total_burden <- total_burden %>% filter(label_cause == label_causeX)
    if (age_group_idX != "all ages") {
      total_burden <- total_burden %>% filter(age_group_id == as.numeric(cause_age[["age_group_id"]]))
    }
    total_burden <- total_burden %>%
      group_by(Year, label_cause) %>%
      dplyr::summarise(overall_value = sum(value))


    exp_rr <- ifelse(age_group_idX == "all ages",
      paste0(label_causeX, ".csv"),
      paste0(label_causeX, "_", age_group_idX, ".csv")
    ) %>%
      file.path(exp_rrDir, .) %>%
      fread()

    pm_matched <- sapply(pms, function(x) pm_levels[match.closest(x, pm_levels)])

    exp_rr <- as.matrix(exp_rr[, -1])
    rownames(exp_rr) <- pm_levels
    exp_rr <- exp_rr[as.character(pm_matched), ]
    exp_paf <- apply(exp_rr, 1:2, function(x) (x - 1) / x)
    exp_paf <- data.frame(pm = row.names(exp_paf) %>% as.numeric(), exp_paf)
    exp_paf <- exp_paf %>%
      pivot_longer(
        cols = colnames(exp_paf) %>% grep("draw", ., value = TRUE),
        names_to = "draw",
        values_to = "paf"
      )

    attr_burd <- merge(total_burden, exp_paf) %>%
      mutate(attr = paf * overall_value)
    return(attr_burd)
  }) %>% rbindlist()

  attr_burd <- attr_burd %>%
    group_by(Year, pm, draw) %>%
    summarise(attr = sum(attr)) %>%
    group_by(Year, pm) %>%
    summarize(
      mean = mean(attr),
      lower = quantile(attr, p = .025),
      upper = quantile(attr, p = .975),
      method = "GBD"
    )
  return(attr_burd)
}

# gbd_curve <- gbd(X)

## ---burnett estimate----
# 29 https://www.pnas.org/content/115/38/9592
burnett <- function(pms) {
  total_burden <- total_burden %>%
    dplyr::filter(label_cause == "ncd_lri") %>%
    group_by(Year) %>%
    dplyr::summarise(overall_value = sum(value))
  # burnett mortality for ncd+lri
  burnett_gemm <- function(X, theta, alpha, mu, v) {
    X <- pmax(0, X - 2.4)
    one <- log(1 + (X / alpha))
    two <- 1 / (1 + exp(-(X - mu) / v))
    Y <- exp(theta * one * two)
    return(Y)
  }

  thetas <- c(0.1430 - 2 * 0.01807, 0.1430, 0.1430 + 2 * 0.01807)
  paf_burnett <- outer(
    pms,
    thetas,
    function(pm, theta) {
      1 - 1 / burnett_gemm(pm, theta, 1.6, 15.5, 36.8)
    }
  )
  paf_burnett <- data.frame(
    pm = pms,
    lower = paf_burnett[, 1],
    mean = paf_burnett[, 2],
    upper = paf_burnett[, 3]
  )

  attr_burd <- merge(total_burden, paf_burnett) %>%
    mutate(
      mean = overall_value * mean,
      lower = overall_value * lower,
      upper = overall_value * upper,
      overall_value = NULL,
      method = "Burnett"
    )
  return(attr_burd)
}

### ----epa----
epa <- function(pms) {
  total_burden <- total_burden %>%
    dplyr::filter(label_cause == "all-cause") %>%
    group_by(Year) %>%
    dplyr::summarise(overall_value = sum(value))

  expa <- rtruncnorm(1000, a = 0, mean = 1.42, sd = 0.89)
  expc <- rtruncnorm(1000, a = 0, mean = 1.2, sd = 0.49)
  expd <- triangle::rtriangle(1000, 0.1, 1.6, 0.95)
  expe <- rtruncnorm(1000, a = 0, mean = 2, sd = 0.61)
  expg <- rtruncnorm(1000, a = 0, mean = 1, sd = 0.19)
  expi <- rtruncnorm(1000, a = 0, b = 2.273, mean = 1.25, sd = 0.53)
  expj <- rweibull(1000, 2.21, 1.41)

  betas <- c(expa, expc, expd, expe, expg, expi, expj) / 100

  paf_epa <- outer(
    pms,
    betas,
    function(pm, beta) {
      1 - exp(-beta * pm) # probably the right one
    }
  )
  paf_epa <- data.frame(
    pm = pms,
    lower = matrixStats::rowQuantiles(paf_epa, probs = 0.25),
    mean = rowMeans(paf_epa),
    upper = matrixStats::rowQuantiles(paf_epa, probs = 0.75)
  )

  attr_burd <- merge(total_burden, paf_epa) %>%
    mutate(
      mean = overall_value * mean,
      lower = overall_value * lower,
      upper = overall_value * upper,
      overall_value = NULL,
      method = "EPA"
    )
  return(attr_burd)
}
### ---plot---
pms <- seq(0, 30, by = 0.5)
data <- rbind(
  gbd(pms),
  burnett(pms),
  epa(pms)
)

g1 <- ggplot(data, aes(x = pm, y = mean, color = method)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.2, show.legend = FALSE)+
  #geom_dl(aes(label = method), method = list(dl.combine("last.points")), cex = 0.8) 
  #geom_text(aes(label = method), position = "dodge")
  #geom_text("a")
  theme(
    legend.title = element_blank(),
    legend.position = c(0.3, 0.88),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "transparent")
  ) 
g1
#
g2 <- ggplot(data, aes(x = pm, y = mean, color = method)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.2, show.legend = FALSE)+
  annotate("text", x = 26, y = 400000, label = "Burnett", size = 8, colour = hue_pal()(3)[1])+
  annotate("text", x = 26, y = 830000, label = "EPA", size = 8, colour = hue_pal()(3)[2])+
  annotate("text", x = 26, y = 170000, label = "GBD", size = 8, colour = hue_pal()(3)[3])+
  xlab("μg/m^3") +
  ylab("deaths")+ 
  theme(legend.position = "none")
g2
ggsave(file.path(figuresDir, "app_figure2.png"), g2, height = 9, width = 8)
