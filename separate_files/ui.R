library(shinydashboard)
library(openxlsx)
library(DT)
library(tidyverse)


# Sidebar with SPH and MSj tabs
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("SPH", tabName = "SPH", icon = icon("vial")),
  menuItem("MSJ", tabName = "MSJ", icon = icon("vial"))
))

# body contents
body <- dashboardBody(
  # SPH tab body
  tabItems(
    tabItem(tabName = "SPH",
            h2("SPH Log"),
            fluidRow(box(dataTableOutput("sphdatatable"
            ), width = 'auto')),
            fluidRow('last updated:', textOutput('sphlatestupdate'))
    ),
    # MSJ tab body
    tabItem(tabName = "MSJ",
            h2("MSJ Log"),
            fluidRow(box(dataTableOutput("msjdatatable"
            ), width = "auto")),
            fluidRow('last updated:', textOutput('msjlatestupdate'))
    )
  )
)


ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "ET Monitor"),
  sidebar = sidebar,
  body = body)