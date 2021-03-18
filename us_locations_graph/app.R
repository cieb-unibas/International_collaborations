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
# Swiss invention locations graph
################################################################################

us_locations <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_us_networkdata.rds")

#us_locations<-us_locations %>%
# filter(location !="US")


ui <- fluidPage(
  
  plotlyOutput("coolplot"),
  selectizeInput("techbroad", "Choose technology",
                 options = list(placeholder = 'select technology'),
                 choices = c(us_locations$techbroad), multiple = FALSE)
  
  
)

server <- function(input, output,session) {
  
  output$coolplot <- renderPlotly({
    
    dr <- reactive({
      filtered <- us_locations %>%
        filter(
          techbroad %in% input$techbroad
        )
    }) 
    
    output$coolplot <- renderPlotly({
      
      fig <- plot_ly(
        dr(), x = ~location, y = ~share, frame=~interval,
        color=~techbroad, type = "bar",
        mode="markers", 
        text = ~paste(sep='','<br>Share of inventors:', round(`share`,1),'%',
                      '<br>Location:', `location_name`)) %>%
        layout(
          title="Foreign collaboration and citation",
          
          yaxis = list(title = '% of inventors',
                       gridcolor = 'white',
                       
                       range=c(0,20),
                       zerolinewidth = F,
                       ticklen = 5,
                       gridwidth = 2),
          xaxis = list(title = 'Locations',
                       gridcolor = 'white',
                       zerolinewidth = F,
                       type = 'location',
                       range = c('location'),
                       ticklen = 5,
                       tickvals = dr()$location,
                       gridwith = 1),
          paper_bgcolor = 'white',
          plot_bgcolor = 'white') %>%
        animation_opts(
          2500, redraw = FALSE
        ) %>%
        
        animation_slider(
          currentvalue = list(prefix = "", font = list(color="black"))
        )
      
      fig <- fig %>% layout(title = 'Locations of US patent inventors',
                            xaxis = list(showgrid = TRUE),
                            legend.position = "none",
                            yaxis = list(showgrid = TRUE))
    })
  })  
}

shinyApp(ui = ui, server = server)