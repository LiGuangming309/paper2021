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
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "viridis", "hrbrthemes", "shiny", "ggplot2", "ggpubr")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
# Pass in arguments
args <- commandArgs(trailingOnly = T)
summaryDir <- args[6]

if (rlang::is_empty(args)) {
  # change directory here!
  summaryDir <- "/Users/default/Desktop/paper2021/data/14_summary"
}

attrBurden <- fread(file.path(summaryDir, "attr_burd.csv"))
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))

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
        label = "measure2",
        choices = unique(all_burden$measure2)
      ),
      selectInput(
        inputId = "source",
        label = "source",
        choices = unique(all_burden$source)
      ),
      checkboxInput(
        inputId = "conf",
        label = "Display confidence interval",
        value = FALSE, width = NULL
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output:  ----
      plotOutput(outputId = "plot1", height = "700px")
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

    # filter data accordingly
    allBurden1 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "overall")
    allBurden2 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "total")
    attrBurden1 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & measure3 == "value")
    attrBurden2 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & measure3 == "prop. of overall burden")

    if (input$raceOrEduc == "race") {
      allBurden1 <- allBurden1 %>% filter(Education == 666)
      allBurden2 <- allBurden2 %>% filter(Education == 666)
      attrBurden1 <- attrBurden1 %>% filter(Education == 666)
      attrBurden2 <- attrBurden2 %>% filter(Education == 666)
      
      g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Ethnicity))
      g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Ethnicity))
      g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Ethnicity))
      g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Ethnicity))
    } else {
      allBurden1 <- allBurden1 %>% filter(Education != 666)
      allBurden2 <- allBurden2 %>% filter(Education != 666)
      attrBurden1 <- attrBurden1 %>% filter(Education != 666)
      attrBurden2 <- attrBurden2 %>% filter(Education != 666)
      
      g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Education))
      g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Education))
      g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Education))
      g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Education))
    }

    g1 <- g1 + geom_line(size = 1)+xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("all-cause burden")
    g2 <- g2 + geom_line(size = 1)+xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("total burden from causes associated with PM exposure ",
                                                                                                                                      subtitle = "(resp_copd, lri, neo_lung, t2_dm, cvd_ihd, cvd_stroke)")
    g3 <- g3 +geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) + ylim(0, NA) + xlim(2000, 2016) + ggtitle("directly attributable to PM exposure")
    g4 <- g4 +geom_line(size = 1) + xlab("Year") + ylab("%") + ylim(0, NA) + xlim(2000, 2016) + ggtitle("proportion of all-cause burden directly attributable to PM exposure")

    if (input$conf) {
      g3 <- g3 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
      g4 <- g4 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
    }
    ggarrange(g1, g2, g3, g4, common.legend = TRUE, legend = "bottom", labels="AUTO")
  })
}
shinyApp(ui = ui, server = server)
