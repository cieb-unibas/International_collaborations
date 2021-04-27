# library(dplyr)
# library(purrr)
# library(igraph)
# library(ggplot2)
# library(ggraph)
# library(RColorBrewer)
# library(countrycode)
# library(visNetwork)
# library(data.table)
# library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(viridis)

rm(list = ls())


################################################################################
# Swiss invention locations graph
################################################################################

top_pat <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/toppat_share_domoestic.rds")

ui <- fluidPage(
  
  plotlyOutput("coolplot"),
  selectizeInput("techbroad", "Choose a technology field",
                 options = list(placeholder = 'Technology fields'),
                 choices = c(top_pat$techbroad), multiple = F, selected = "Total")
  
  
)

server <- function(input, output,session) {
  
  output$coolplot <- renderPlotly({
    
    dr <- reactive({
      filtered <- top_pat %>%
        filter(
          techbroad %in% input$techbroad
        )
    }) 
    
    output$coolplot <- renderPlotly({
      
      if(nrow(dr())!= 0){   
        
          ggplot(dr(), aes(x = reorder(ctry_leg_owner, -world_class_90), y = world_class_90)) + geom_bar(stat = "identity")
          
          
 
      } else {}
    })
  })  
}

shinyApp(ui = ui, server = server)