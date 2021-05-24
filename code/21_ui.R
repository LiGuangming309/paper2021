#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)

#load calculated data
summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
#summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/14_summary"
#if not downloaded, load from github
if(!file.exists(summaryDir)) summaryDir <- 'https://raw.github.com/FridljDa/paper2021/master/data/14_summary'

attrBurden <- rbind(fread(file.path(summaryDir, "attr_burd.csv")), 
                    fread(file.path(summaryDir, "attr_burd_prop.csv")))
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pop_summary <- fread(file.path(summaryDir, "pop_summary.csv"))

colnames(all_burden)
# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Explore data"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(
        inputId = "raceOrEduc",
        label = "Aggregate by race or education?",
        choices = c("race", "education")
      ),
      selectInput(
        inputId = "Gender.Code",
        label = "Gender",
        choices = unique(all_burden$Gender.Code)
      ),
      selectInput(
        inputId = "Region",
        label = "Region",
        choices = unique(all_burden$Region)
      ),
      selectInput(
        inputId = "measure1",
        label = "YLL or Deaths",
        choices = unique(all_burden$measure1)
      ),
      selectInput(
        inputId = "measure2",
        label = "rate",
        choices = unique(all_burden$measure2)
      ),
      selectInput(
        inputId = "source",
        label = "source burden data",
        choices = unique(all_burden$source)
      ),
      checkboxInput(
        inputId = "conf",
        label = "Display confidence interval",
        value = FALSE, width = NULL
      ),
      selectInput(
        inputId = "method",
        label = "which method used to calculate attributable burden",
        choices = unique(attrBurden$method)
      ),
      selectInput(
        inputId = "pm_metric",
        label = "median or mean PM2.5 exposure",
        choices = unique(pm_summ$pm_metric)
      ),
      selectInput(
        inputId = "source2",
        label = "source population data",
        choices = unique(pop_summary$source2)
      ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output:  ----
      plotOutput(outputId = "plot1", height = "1200px")
    )
  )
)
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  output$plot1 <- renderPlot({
    # get Input
    Gender.CodeI <- input$Gender.Code
    RegionI <- input$Region
    measure1I <- input$measure1
    measure2I <- input$measure2
    sourceI <- input$source
    methodI <- input$method
    pm_metricI <- input$pm_metric
    source2I <- input$source2

    # filter data accordingly
    allBurden1 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "overall")
    allBurden2 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "total")
    attrBurden1 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "value")
    attrBurden2 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "prop. of overall burden")
    attrBurden3 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method 
                                         & measure3 %in% c("proportion of disparity to White, Not Hispanic attributable", "proportion of disparity to Graduate or professional degree attributable"))
    attrBurden4 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "prop. of total burden")
    
    pm_summ1 <- pm_summ %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & pm_metric == pm_metricI)
    pop_summary1 <- pop_summary %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & source2 == source2I)
#TODO All Origins?
    if (input$raceOrEduc == "race") {
      allBurden1 <- allBurden1 %>% filter(Education == 666)
      allBurden2 <- allBurden2 %>% filter(Education == 666)
      attrBurden1 <- attrBurden1 %>% filter(Education == 666)
      attrBurden2 <- attrBurden2 %>% filter(Education == 666)
      attrBurden3 <- attrBurden3 %>% filter(Education == 666)
      attrBurden4 <- attrBurden4 %>% filter(Education == 666)
      pm_summ1 <- pm_summ1 %>% filter(Education == 666)
      pop_summary1 <- pop_summary1 %>% filter(Education == 666)

      g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Ethnicity))
      g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Ethnicity))
      g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Ethnicity))
      g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Ethnicity))
      g5 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity))
      g6 <- ggplot(pop_summary1, aes(x = Year, y = Population, color = Ethnicity))
      g7 <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Ethnicity))
      g8 <- ggplot(attrBurden4, aes(x = Year, y = mean, color = Ethnicity))
    } else {
      allBurden1 <- allBurden1 %>% filter(Education != 666)
      allBurden2 <- allBurden2 %>% filter(Education != 666)
      attrBurden1 <- attrBurden1 %>% filter(Education != 666)
      attrBurden2 <- attrBurden2 %>% filter(Education != 666)
      attrBurden3 <- attrBurden3 %>% filter(Education != 666)
      attrBurden4 <- attrBurden4 %>% filter(Education != 666)
      pm_summ1 <- pm_summ1 %>% filter(Education != 666)
      pop_summary1 <- pop_summary1 %>% filter(Education != 666)

      g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Education))
      g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Education))
      g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Education))
      g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Education))
      g5 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Education))
      g6 <- ggplot(pop_summary1, aes(x = Year, y = Population, color = Education))
      g7 <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Education))
      g8 <- ggplot(attrBurden4, aes(x = Year, y = mean, color = Education))
    }

    g1 <- g1 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("all-cause burden")
    g2 <- g2 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("total burden from causes associated with PM2.5 exposure ",
      subtitle = "(resp_copd, lri, neo_lung, t2_dm, cvd_ihd, cvd_stroke)"
    )
    g3 <- g3 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("directly attributable to PM2.5 exposure")
    g4 <- g4 + geom_line(size = 1) + xlab("Year") + ylab("%")  + xlim(2000, 2016) + ggtitle("proportion of all-cause burden directly attributable to PM2.5 exposure")

    g5 <- g5 + geom_line(size = 1) + xlab("Year") + ylab("μg/m3") + xlim(2000, 2016) +ggtitle(paste("population-weighted", pm_metricI, "of PM2.5  exposure"))
    g6 <- g6 + geom_line(size = 1) + xlab("Year") + ylab("Population") + ylim(0, NA) + xlim(2000, 2016) + ggtitle("Population size")
    g7 <- g7 + geom_line(size = 1) + xlab("Year") + ylab("%") + xlim(2000, 2016) + ggtitle(paste(unique(attrBurden3$measure3)))
    g8 <- g8 + geom_line(size = 1) + xlab("Year") + ylab("%") + ylim(0, NA) + xlim(2000, 2016) + ggtitle("proportion of total burden directly attributable to PM2.5 exposure")
    
    if (input$conf) {
      g3 <- g3 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
      g4 <- g4 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    }
    #https://stackoverflow.com/questions/17180115/manually-setting-group-colors-for-ggplot2
    group.colors <- c(hue_pal()(5),hue_pal()(7))
    names(group.colors) <- c("White, Not Hispanic or Latino",
                               "White, Hispanic or Latino",
                               "Black or African American",
                               "Asian or Pacific Islander",
                               "American Indian or Alaska Native",
                               "Less than 9th grade", 
                               "9th to 12th grade, no diploma", 
                               "High school graduate, GED, or alternative",
                               "Some college, no degree", 
                               "Associate's degree",
                               "Bachelor's degree", 
                               "Graduate or professional degree"
                             )

    g1 <- g1+  scale_colour_manual(values=group.colors)
    g2 <- g2+  scale_colour_manual(values=group.colors)
    g3 <- g3+  scale_colour_manual(values=group.colors)
    g4 <- g4+  scale_colour_manual(values=group.colors)
    g5 <- g5+  scale_colour_manual(values=group.colors)
    g6 <- g6+  scale_colour_manual(values=group.colors)
    g7 <- g7+  scale_colour_manual(values=group.colors)
    g8 <- g8+  scale_colour_manual(values=group.colors)

    g_comb <- ggarrange(g1, #g2, 
                        g3, g4, g5, g6, g7,#g8,
                        ncol = 2, nrow = 4,
              common.legend = TRUE, legend = "top", 
              labels = "AUTO") 
    g_comb
    #ggsave("/Users/default/Desktop/race_burden.png", g1)
    #ggsave("/Users/default/Desktop/race_pop.png", g6)
  })
}
shinyApp(ui = ui, server = server)
