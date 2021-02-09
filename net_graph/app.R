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
  network_data_foreign <- filter(network_data_foreign, share_foreign > 1)
  
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
      
      edges$title <- paste0('Share inventors:', round(edges$value, 1),'%')
      
      #Creating Network Plot    
      visNetwork(nodes, edges, width = "100%")%>%
        visIgraphLayout(layout = "layout_nicely") %>%
        visEvents(type = "once", startStabilizing = "function() {
              this.moveTo({scale:0.5})}") %>%
        visPhysics(stabilization = FALSE) %>%
        visNodes(
          fixed = TRUE,
          shape = "dot",
          color = "grey80",
          shadow = list(enabled = F, size = 20)
        ) %>%
        visEdges(
          value="value",
          shadow = FALSE,
          selfReferenceSize = F,
          smooth = FALSE
        ) %>%
      visOptions(highlightNearest = list(enabled = F, degree = 1, hover = T))
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  