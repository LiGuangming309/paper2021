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
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "ggrepel",
  "dplyr", "ggExtra", "gridExtra", "grid", "tidyverse"
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
scenarioI <- args[10]
methodI <- args[11]

# TODO delete
if (rlang::is_empty(args)) {
  scenarioI <- "A"
  methodI <- "burnett"
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
  figuresDir <- "/Users/default/Desktop/paper2021/data/15_figures"

  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
  # figuresDir <-  "C:/Users/Daniel/Desktop/paper2021/data/15_figures"
}

theme_set(theme_classic())
### ----- read stuff----
all_burden <- fread(file.path(summaryDir, "all_burd.csv")) %>% as.data.frame()
file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist()
rm(file_list)

# filter
all_burden <- all_burden %>% filter(attr == "overall")
attr_burd <- attr_burd %>% filter(attr == "attributable" & measure3 == "value" & scenario == scenarioI)
### ---- population ranking----
pop_sum <- fread(file.path(summaryDir, "pop_summary.csv"))
pop_sum <- pop_sum %>% filter(Gender.Code == "All genders" & source2 == "Official Bridged-Race Population Estimates" &
  Education == "666" & Ethnicity == "All, All Origins")
pop_sum <- pop_sum %>%
  group_by(Year, Region) %>%
  summarize(population_state_all = sum(Population))

most_pop_states <- pop_sum %>%
  group_by(Region) %>%
  summarise(population_state_all = mean(population_state_all)) %>%
  arrange(desc(population_state_all)) %>%
  head(31) %>%
  select(Region) %>%
  unlist()

## --filter more---
joined_all_attr <- inner_join(all_burden, attr_burd, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
joined_all_attr <- inner_join(joined_all_attr, pop_sum, by = c("Year", "Region"))
joined_all_attr[joined_all_attr == "United States"] <- "national"
joined_all_attr <- joined_all_attr %>%
  filter(Region %in% most_pop_states & Year == 2016 &
    Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System") %>%
  group_by_at(vars(all_of(setdiff(colnames(joined_all_attr), c("Year", "overall_value", "mean", "lower", "upper", "population_state_all"))))) %>%
  summarise(
    overall_value = mean(overall_value),
    mean = mean(mean),
    lower = mean(lower),
    upper = mean(upper),
    population_state_all = mean(population_state_all)
  ) %>%
  ungroup() %>%
  as.data.frame()

## ---region size ---
test1 <- joined_all_attr %>% filter(Region != "national")
test <- data.frame(
  x = c(min(test1$population_state_all), max(test1$population_state_all)),
  y = c(2.5, 3)
)
model.lm <- lm(y ~ x, data = test)
joined_all_attr$point_size <- predict(model.lm, newdata = data.frame(x = joined_all_attr$population_state_all))
joined_all_attr$point_size <- pmin(3, joined_all_attr$point_size)

rm(test1, test, model.lm)
## --- seperate three factors----
joined_all_attr1 <- joined_all_attr %>%
  filter(Ethnicity %in% c("Black or African American", "White, Not Hispanic or Latino") &
    Education == 666 & Ethnicity != "All, All Origins" & rural_urban_class == "All" & method == "di_gee" ) 

joined_all_attr2 <- joined_all_attr %>%
  filter(Education %in% c("4-year college graduate or higher", "high school graduate or lower") &
    Education != 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All" & method == "di_gee") #TODO

joined_all_attr3 <- joined_all_attr %>%
  filter(rural_urban_class %in% c("large metro", "non metro") &
    Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & method == "di_gee") #TODO
# joined_all_attr <- joined_all_attr
# joined_all_attr <- joined_all_attr[1:51, ]

## ---get convex hull----
convex_hull1 <- joined_all_attr1 %>%
  select(overall_value, mean) %>%
  as.matrix() %>%
  chull()

convex_hull2 <- joined_all_attr2 %>%
  select(overall_value, mean) %>%
  as.matrix() %>%
  chull()

convex_hull3 <- joined_all_attr3 %>%
  select(overall_value, mean) %>%
  as.matrix() %>%
  chull()

convex_regions1 <- joined_all_attr1[convex_hull1, "Region"]
convex_regions2 <- joined_all_attr2[convex_hull2, "Region"]
convex_regions3 <- joined_all_attr3[convex_hull3, "Region"]
rm(convex_hull1, convex_hull2, convex_hull3)
## ---colors---
group.colors1 <- hue_pal()(5)[c(1, 3)] # TODO
names(group.colors1) <- c("White, Not Hispanic or Latino", "Black or African American")

group.colors2 <- hue_pal()(3)[c(1, 3)] # TODO
names(group.colors2) <- c("4-year college graduate or higher", "high school graduate or lower")

group.colors3 <- hue_pal()(3)[c(1, 3)] # TODO
names(group.colors3) <- c("non metro", "large metro")

#--plots----
add_stuff <- function(g, convex_regions, group.colors){
  g <- g +
    geom_abline(
      intercept = layer_scales(g)$y$range$range[1] - 0.5 * layer_scales(g)$x$range$range[1],
      slope = 0.5,
      linetype = 3
    ) +
    # annotate("text", label = "m = 0.5", x = 885, y = 160, size = 3.5)+
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.3, width = 10) +
    
    #theme(
    #  legend.title = element_blank(),
    #  legend.position = c(0.8, 0.2),
    #  legend.text = element_text(size = 9),
    #    legend.box.background = element_rect(colour = "black"),
    #    legend.background = element_rect(fill = "transparent")
    #  ) +
    theme(legend.position = "none")+
    ylab("burden attributable to PM2.5 exposure") +
    xlab("all-cause burden") +
    geom_label_repel(
      aes(label = ifelse(Region %in% c(convex_regions, "national"), as.character(Region), "")), # ,shape = agr_by
      size = 2,
      box.padding = 0.2, # 0.35
      point.padding = 0.5,
      segment.color = "grey50"
    ) +
    scale_colour_manual(values = group.colors) +
    guides(size = "none")  # , alpha =
  
  # https://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2
  # https://cran.r-project.org/web/packages/ggExtra/readme/README.html
  g <- ggExtra::ggMarginal(g, groupColour = TRUE, groupFill = TRUE, size = 15)
  return(g)
}

g1 <- ggplot(joined_all_attr1, aes(x = overall_value, y = mean))
g2 <- ggplot(joined_all_attr2, aes(x = overall_value, y = mean))
g3 <- ggplot(joined_all_attr3, aes(x = overall_value, y = mean))

## --set range---
min_y <- min(c(joined_all_attr1$lower, joined_all_attr2$lower, joined_all_attr3$lower))
max_y <- max(c(joined_all_attr1$upper, joined_all_attr2$upper, joined_all_attr3$upper))
min_x <- min(c(joined_all_attr1$overall_value, joined_all_attr2$overall_value, joined_all_attr3$overall_value))
max_x <- max(c(joined_all_attr1$overall_value, joined_all_attr2$overall_value, joined_all_attr3$overall_value))
if(TRUE){
  g1 <- g1 + ylim(min_y, max_y) + xlim(min_x, max_x)
  g2 <- g2 + ylim(min_y, max_y) + xlim(min_x, max_x)
  g3 <- g3 + ylim(min_y, max_y) + xlim(min_x, max_x)
}

###-----add information----
g1 <- g1 +geom_point(aes(size = point_size, color = Ethnicity)) 
g1 <- add_stuff(g1, convex_regions1, group.colors1) 

g2 <- g2 +geom_point(aes(size = point_size, color = Education)) 
g2 <- add_stuff(g2, convex_regions2,  group.colors2)

g3 <- g3 +geom_point(aes(size = point_size, color = rural_urban_class)) 
g3 <- add_stuff(g3, convex_regions3,  group.colors3)
##--- legend ---
group.colors <- c(group.colors1, group.colors2, group.colors3)

legend_df <- data.frame(
  colour = group.colors,
  label = names(group.colors),
  column = c(rep("Ethnicity", 2), rep("Education", 2), rep("rural_urban_class", 2))
)

legend_df <- legend_df %>%
  complete(colour, column, fill = list(label = "")) %>%
  group_by(column) %>%
  mutate(label = str_pad(label, max(nchar(label)), "right"))

legend_df <- legend_df %>%
  pivot_wider(
    names_from = column,
    values_from = label # ,
    # values_fill = ""
  ) %>%
  as.data.frame() %>%
  unite("labels", c("Ethnicity", "Education", "rural_urban_class"), sep = " | ")


color_legend <- legend_df$colour
names(color_legend) <- legend_df$labels

legend_plot <- get_legend(ggplot(
  data = legend_df %>% mutate(test = 1),
  aes(color = labels, x = test, y = test)
) +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(4, "mm"),
    legend.text = element_text(family = "mono", size = 5)
  ) +
  #  guides(color = guide_legend(label.position = "left"))+
  geom_point() +
  scale_colour_manual(values = color_legend))
as_ggplot(legend_plot)

###---- arrange-----
lay <- rbind(
  c(1,NA,2),
  c(NA,NA,NA),
  c(3, NA, 4)
)

gs <- list(g1, g2, g3, legend_plot)


blank_space <- 0.05
figure_width <- 1.3
figure_hight <- 1

g_combined <- grid.arrange(
  grobs = gs,
  widths = c(figure_width, blank_space, figure_width),
  heights = c(figure_width, blank_space, figure_hight),
  layout_matrix = lay
)

ggsave(file.path(figuresDir, "figure3.png"), g_combined,
       height = 6, width = 9
)
