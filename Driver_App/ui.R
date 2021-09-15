library(shiny)
library(reactable)
library(shinydashboard)
library(shinythemes)

shinyUI(
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(  
      titlePanel("Formula 1 Driver Performance "),
      reactableOutput("table")
    ))
)
