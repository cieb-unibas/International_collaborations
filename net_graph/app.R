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
# Network graph
################################################################################

# Inserting network data
network_data_foreign <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_ch_networkdata.rds")

# Creating CH var as node
  network_data_foreign$origin <- "CH"  

# Drooping countries with less than 1%
  network_data_foreign <- filter(network_data_foreign, share_foreign > 0.5)
  
  ui <- fluidPage(
    
    visNetworkOutput("coolplot"),
    selectizeInput("tech_name", "Choose technology",
                   options = list(placeholder = 'select technology'),
                   choices = c(network_data_foreign$tech_name), multiple = FALSE)
    
  )
  
  server <- function(input, output,session) {
    
    output$coolplot <- renderVisNetwork({
      
      dr <- reactive({
        filtered <- network_data_foreign %>%
          filter(
            tech_name %in% input$tech_name
          )
      }) 
      
      #Creating NODES from the reactive (filtered) data
      nodes <- data.frame(id = dr()$ctry_inventor,
                          size = 4,
                          label = paste(dr()$ctry_inventor))                 
      
      
      #Creating EDGES from the reactive (filtered) data    
      edges <- subset(dr(), select = c("ctry_inventor" , "origin","share_foreign"))
      colnames(edges) <- c("from", "to", "value")
      
      #Creating Network Plot    
      visNetwork(nodes, edges, width = "100%")%>%
        visEdges(
          value="value",
          shadow = FALSE,
          selfReferenceSize = F
        )
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  