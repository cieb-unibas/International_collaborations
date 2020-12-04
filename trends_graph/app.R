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

rm(list = ls())


################################################################################
# Trends graph
################################################################################

trends_data_graph <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")


  
  ui <- fluidPage(
    
    plotlyOutput("coolplot"),
    selectizeInput("tech_name", "Choose a technology field",
                   options = list(placeholder = 'select technology'),
                   choices = c(trends_data_graph$tech_name), multiple = FALSE)
    
  )
  
  server <- function(input, output,session) {
    
    output$coolplot <- renderPlotly({
      
      dr <- reactive({
        filtered <- trends_data_graph %>%
          filter(
            tech_name %in% input$tech_name
          )
      }) 
      
      output$coolplot <- renderPlotly({
        

  plot_ly(
    dr(), x = ~share_foreign, y = ~share_wc,
    frame=~interval,
    color=~`owner_ctry`, type = "scatter",
    mode="markers", size=~share_wc,
    marker = list(symbol = 'circle', sizemode = 'diameter',
                  line = list(width = 2, color = '#FFFFFF'), opacity=0.4),
    text = ~paste(sep='','share_foreign:', round(`share_foreign`,1),'%',
                  '<br>Share WC patents:',round(`share_wc`,1),'%',
                  '%', '<br>Owner:', `owner_ctry`)) %>%
    layout(
      title="Foreign collaboration and citation",
      
      xaxis = list(title = '% Foreign scientists',
                   gridcolor = 'rgb(255, 255, 255)',
                   
                   range=c(0,80),
                   zerolinewidth = 1,
                   ticklen = 5,
                   gridwidth = 2),
      yaxis = list(title = '% World class patents',
                   gridcolor = 'rgb(255, 255, 255)',
                   range=c(0,30),
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
      currentvalue = list(prefix = "YEAR ", font = list(color="red"))
        )
      })
    })  
  }

shinyApp(ui = ui, server = server)