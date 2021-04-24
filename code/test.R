summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
attrBurden <- fread(file.path(summaryDir, "attr_burd.csv"))

attrBurdenX <- attrBurden %>%
  filter(Education == 666) %>%
  filter(Region == "United States", source == "National Vital Statistics System", measure3 == "value")

attrBurden1 <- attrBurdenX %>%
  filter(
    measure1 == "Years of Life Lost (YLL)",
    measure2 == "age-adjusted rate per 100,000",
    attr == "attributable",
    Gender.Code == "All genders"
  )

test <- read.csv("~/Desktop/test.csv")

g <- ggplot(test, aes(x = Year, y = mean, color = Ethnicity)) +
  geom_line(size = 1) +
  ylab(paste("YLL per 100.000")) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE)) 

g

g <- g + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
g
