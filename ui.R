library(shiny)
library(data.table)
library(quantmod)
library(RCurl)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(plotly)

source('moduleCluster.R')
source('Functions.R')

shinyUI(dashboardPage(skin="blue",
                      
                      # Dashboard Header            
                      dashboardHeader(title = "Stock Clustering (v1.0)"),
                      
                      # Sidebar content
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem('ASX', icon = icon("dollar"),  startExpanded = TRUE,
                              menuSubItem(text = 'Overview', tabName = "Overview", icon = icon('th')),
                              menuSubItem('Industry', tabName = 'Industry', icon = icon('industry')),
                              menuSubItem('Clustering', tabName = 'Clustering', icon = icon('object-group')),
                              menuSubItem('Price Validation', tabName = 'Validate', icon = icon('line-chart'))
                          ),
                          menuItem('Nasdaq(under working)', tabName = 'nasdaq', icon = icon('dollar'))
                        )
                      ),
                      
                      # Dashboard body
                      dashboardBody(
                        
                        tabItems(
                          tabItem(tabName = 'Overview', mod_ASXUI('mod_asx', section = 'overview')),
                          tabItem(tabName = 'Industry', mod_ASXUI('mod_asx', section = 'industry')),
                          tabItem(tabName = "Clustering", mod_ASXUI('mod_asx', section = 'clustering')),
                          tabItem(tabName = "Validate", mod_ASXUI('mod_asx', section = 'validate'))
                        )
                      )
)
)
    
