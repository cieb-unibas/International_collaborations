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
    
    plotlyOutput("coolplot")
    
  )
  
  server <- function(input, output,session) {
    
    output$coolplot <- renderPlotly({
      
      dr <- reactive({
        filtered <- trends_data_graph 
      }) 
      
      output$coolplot <- renderPlotly({
        

fig <- ggplot(dr(), aes(x = foreign, y = world_class_90)) + 
    geom_point(aes(color = ctry_leg_owner, shape = techbroad), alpha = 0.5) +
     theme(legend.position = "none") +
  geom_abline() +  xlim(0, 20) +  ylim(0, 20)
      })
    })  
  }

shinyApp(ui = ui, server = server)
