library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(RColorBrewer)
library(countrycode)
library(visNetwork)
library(data.table)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)

rm(list = ls())


################################################################################
# Trends graph
################################################################################

trends_data_graph <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")


  
  ui <- fluidPage(
    
    plotlyOutput("coolplot"),
    selectizeInput("techbroad", "Choose a technology field",
                   options = list(placeholder = 'select a technology'),
                   choices = c(trends_data_graph$techbroad), multiple = TRUE, selected = "Chemistry"),
    selectizeInput("ctry_leg_owner", "Choose a country",
                   options = list(placeholder = 'select a country'),
                   choices = c(trends_data_graph$ctry_leg_owner), multiple = TRUE, selected = list("CH", "DE", "US"))
    
  )
  
  server <- function(input, output,session) {
    
    output$coolplot <- renderPlotly({
      
      dr <- reactive({
        filtered <- trends_data_graph %>%
          filter(
            techbroad %in% input$techbroad,
            ctry_leg_owner %in% input$ctry_leg_owner
          )
      }) 
      
      output$coolplot <- renderPlotly({
        

fig <- plot_ly(
          dr(), x = ~foreign, y = ~world_class_90,
          frame=~interval,
          color=~`ctry_leg_owner`, type = "scatter",
          mode="markers", size=0.1,
          marker = list(symbol = 'circle', sizemode = 'diameter', opacity=0.4),
          text = ~paste(sep='','<br>Share foreign:', round(`foreign`,1),'%',
                        '<br>Share WC patents:',round(`world_class_90`,1),
                        '%', '<br>Owner:', `ctry_leg_owner`)) %>%
          layout(
            title="Foreign collaboration and citation",
            
            xaxis = list(title = '% with foreign inventors',
                         gridcolor = 'rgb(255, 255, 255)',
                         
                         range=c(0,80),
                         zerolinewidth = 1,
                         ticklen = 5,
                         gridwidth = 2),
            yaxis = list(title = '% world class patents',
                         gridcolor = 'rgb(255, 255, 255)',
                         range=c(0,20),
                         zerolinewidth = 1,
                         ticklen = 5,
                         gridwith = 2),
            paper_bgcolor = 'rgb(243, 243, 243)',
            plot_bgcolor = 'rgb(243, 243, 243)'
          )%>%
          animation_opts(
            2000, redraw = FALSE
          ) %>%
          
          animation_slider(
            currentvalue = list(prefix = "YEAR ", font = list(color="black"))
          )
    fig <- fig %>% layout(title = 'Foreign collaboration and citation',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))
      })
    })  
  }

shinyApp(ui = ui, server = server)
