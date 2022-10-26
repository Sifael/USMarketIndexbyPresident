
"===================================================================
            US PRESIDENTS AND THE STOCK MARKET
==================================================================="

library(shiny)
library(shinydashboard)
library(fullPage)
library(highcharter)
library(tidyverse)
library(shinyWidgets)
library(highcharter)
library(data.table)

"=====================================
         DATA FUNCTIONS
======================================"

# UNIQUE PRESIDENTS
get_presidents = function( data_df )
{
    data_df %>% distinct(President) %>% pull(President)
}

# UNIQUE MARKETS
get_markets = function( data_df ) 
{
    data_df %>% distinct(Market) %>% pull(Market)
}


"======================================
             UI SECTION
======================================"

ui = pagePiling(
    sections.color = c("#F6EADF", "#F6EADF"),
    menu = c("Markets" = "markets", "Data" = "data"),
    pageSection( center = TRUE, menu = "markets", uiOutput("markets")),
    pageSection( center = TRUE, menu = "data", uiOutput("dataset"))
)


"=======================================
            SERVER SECTION
======================================="

server = function(input, output)
{
    
    # RETURN DATA
    presidents_market_data = read.csv("presidents_and_markets.csv")
    presidents_data = read.csv('us_presidents_data.csv')
    markets = get_markets(presidents_market_data)
    presidents = get_presidents(presidents_market_data)
    
    # SELECT MARKET
    output$select_market = renderUI({
        radioGroupButtons( inputId = "market",
                           label = "Select Market Index:",
                           choices = markets,
                           selected = sample(markets, 1),
                           checkIcon = list( yes = icon("ok", lib = "glyphicon")))
    })

    # SELECT PRESIDENT
    output$select_president = renderUI({
        selectizeInput( inputId = "president",
                        label = "Type and Presidents:",
                        multiple = TRUE,
                        choices = presidents,
                        selected = sample(presidents, 2))
    })

        
    # OUTPUT CHART
    output$trend_chart = renderHighchart({
        presidents_market_data %>% filter( President %in% input$president,
                                           Market %in% input$market, 
                                           MonthEnd == 'True') %>%
            group_by(President, Market, MonthEnd) %>%
            mutate(MonthlyClosePrice_PctChange = (Close/lag(Close) - 1) * 100,
                   Month = row_number()) %>%
            hchart(., 'line', hcaes( x = Month, y = MonthlyClosePrice_PctChange , group = President ) )
    })
    
    output$markets = renderUI({
        pageContainer(  h3("US Presidents and the Stock Market"), br(),br(),
                        fluidRow(
                            column( width = 6, uiOutput("select_market"), align="left", style='padding-left:2px;' ),
                            column( width = 6, uiOutput("select_president", align="right"))
                        ), br(),
                        highchartOutput("trend_chart", height = "50vh"),
                        
        )
    })
    
    
    # OUTPUT DATASET
    output$market_data = renderDataTable({ presidents_market_data })
    output$presidents_data = renderDataTable({ presidents_data  })
    
    output$dataset = renderUI({ div(   style = "padding-top:80px;", 
                                       pageContainer(
                                               tabsetPanel ( 
                                                   tabPanel("US Market Data Daily", dataTableOutput("market_data")),
                                                   tabPanel("US Presidents Data", dataTableOutput("presidents_data"))))
                                                  )})

}


"================================
         SHINY APP
================================="
shinyApp( ui = ui, server = server )