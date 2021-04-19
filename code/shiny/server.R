#
#library(shiny)
#setwd("/Users/default/Desktop/paper2021/code/shiny")
shinyServer(
  pageWithSidebar(
    headerPanel("My First Shiny App"),
    
    sidebarPanel("Side Bar"),
    
    mainPanel("Main Panel")
  )
)
#runApp()
library(shiny)
#runExample("02_text")
