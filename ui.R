library(shinydashboard)
library(shiny)
library(highcharter)

header <- dashboardHeader(title = "ETFR")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbm",   # crucial to name sidebarMenu 
              menuItem("Vanguard", tabName = "vanguard", icon = icon("dashboard")),
              uiOutput("dateSliderUi")
  )
)

body <- dashboardBody(
  tags$head(includeCSS("United.css")),
  
  fluidRow(
    DT::dataTableOutput("return_table"),
    highchartOutput("return_treemap"),
    highchartOutput("candlestick_chart")
    
  )
  
  
)

dashboardPage(
  header,
  sidebar,
  body
)
