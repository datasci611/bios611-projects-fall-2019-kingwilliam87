#app!

library(tidyverse)
library(shiny)
library(shinydashboard)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)

source("helper_functions.R",local=environment())


ui <- fluidPage(
  titlePanel("Summary of UMD Services Provided, 2000-2019"),
    sidebarPanel(
      h4("Annual Service Trends"),
      helpText("Select service and time period you wish to view."),
      radioButtons('plot1_y', 'Service:',
                   choices =
                     c('Bus Tickets'='Bus',
                       'Clothing'='Clothing',
                       'Diapers'='Diapers',
                       'Financial Support'='Financial',
                       'Food Pounds'='Food',
                       'Hygiene Kits'='Hygienekits',
                       'School Kits'='Schoolkits'
                       )),
      
      sliderInput('plot1_x',
                  'Years:',
                  min = 2000,
                  max = 2019,
                  value = c(2000,2019),
                  sep = ''),
      
      hr(),
      h4(helpText("Selected Client Details")),
      tableOutput(outputId="Table")
      ),
  mainPanel(
    plotOutput(outputId="Plot1a"),
    hr(),
    helpText("Use mouse cursor to capture points of interest."),
    plotOutput(outputId="Plot1b", brush = brushOpts(id = "plot1b_brush")),
    hr(),
    plotOutput(outputId="Plot2"),
    helpText("Data source: https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv")
    )
  )

server <- function(input,output) {
  
  #Sum of annual service by year plot
  output$Plot1a <- renderPlot({
    plot1a(data,input$plot1_y,input$plot1_x) 
  })
  
  #All times service provided plot
  output$Plot1b <- renderPlot({
    plot1b(data,input$plot1_y,input$plot1_x)
  })
  
  output$Plot2 <- renderPlot({
    plot2(data,input$plot1b_brush,input$plot1_y,input$plot1_x)
  })

  output$Table <- renderTable({
    tab(data,input$plot1b_brush,input$plot1_y,input$plot1_x)
    
    })
}


shinyApp(ui, server)

