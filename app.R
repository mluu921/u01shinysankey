library(shiny)
library(shinydashboard)
library(tidyverse)
library(networkD3)
library(tidygraph)
library(igraph)
library(labelled)

options(shiny.autoreload = T)

ae_data <- read_rds('data/processed_condensed_data_labelled.rds')

qol_data <- read_rds('data/processed_full_qol_pro_data.rds')

dashboard_sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    'AE Sankey',
    tabName = 'ae_analysis_tab',
    icon = icon("th"),
    startExpanded = F
  ),
  menuItem(
    'QOL Sankey',
    tabName = 'qol_analysis_tab',
    icon = icon("th"),
    startExpanded = F
  ),
  menuItem(
    'Contact',
    tabName = 'contacts_tab',
    icon = icon('envelope-o')
  )
))

dashboard_header <- dashboardHeader(
  title = 'PRO-CTCAE'
)

dashboard_body <-
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'ae_analysis_tab',
        ae_analysis_output('ae_analysis')
      ),
      tabItem(
        tabName = 'qol_analysis_tab',
        qol_analysis_output('qol_analysis', qol_data)
      ),
      tabItem(
        tabName = 'contacts_tab',
        fluidRow(
          box(
            title = 'Contact',
            width = 12,
            h3('Michael Luu, MPH'),
            a('michael.luu@cshs.org', href = 'mailto: michael.luu@cshs.org')
          )
        )
      )
    )
  )

ui <- dashboardPage(skin = 'blue',
                    dashboard_header,
                    dashboard_sidebar,
                    dashboard_body)


server <- function(input, output, session) {

  ae_analysis_server('ae_analysis', ae_data)
  
  qol_analysis_server('qol_analysis', qol_data)
  
}

shinyApp(ui, server)

