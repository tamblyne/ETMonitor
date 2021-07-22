library(shinydashboard)
library(openxlsx)
library(DT)
library(tidyverse)


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("SPH", tabName = "SPH", icon = icon("vial")),
  menuItem("MSJ", tabName = "MSJ", icon = icon("vial"))
))

# body contents
body <- dashboardBody(
  # SPH tab
  tabItems(
    tabItem(tabName = "SPH",
            h2("SPH Log"),
            fluidRow(box(dataTableOutput("sphdatatable"
            ), width = 'auto'))
    ),
    # MSJ tab
    tabItem(tabName = "MSJ",
            h2("MSJ Log"),
            fluidRow(box(dataTableOutput("msjdatatable"
            ), width = "auto"))
    )
  )
)


ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "ET Monitor"),
  sidebar = sidebar,
  body = body)