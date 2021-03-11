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

ch_locations <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_ch_networkdata.rds")

ch_locations<-ch_locations %>%
  filter(location !="CH")


ui <- fluidPage(
  
  plotlyOutput("coolplot"),
  selectizeInput("techbroad", "Choose technology",
                 options = list(placeholder = 'select technology'),
                 choices = c(ch_locations$techbroad), multiple = FALSE)
  
  
)

server <- function(input, output,session) {
  
  output$coolplot <- renderPlotly({
    
    dr <- reactive({
      filtered <- ch_locations %>%
        filter(
          techbroad %in% input$techbroad
        )
    }) 
    
    output$coolplot <- renderPlotly({

fig <- plot_ly(
  dr(), x = ~share, y = ~location, frame=~interval,
  color=~techbroad, type = "scatter",
  mode="markers", 
  text = ~paste(sep='','<br>Share of inventors:', round(`share`,1),'%',
                '%', '<br>Location:', `location`)) %>%
  layout(
    title="Foreign collaboration and citation",
    
    xaxis = list(title = '% of inventors',
                 gridcolor = 'white',
                 
                 range=c(0,22),
                 zerolinewidth = 1,
                 ticklen = 5,
                 gridwidth = 2),
    yaxis = list(title = 'Locations',
                 gridcolor = 'white',
                 zerolinewidth = 1,
                 type = 'location',
                 range = c('location'),
                 ticklen = 5,
                 tickvals = dr()$location,
                 gridwith = 1),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white') %>%
  animation_opts(
    2000, redraw = FALSE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Period", font = list(color="red"))
  )

fig <- fig %>% layout(title = 'Locations of Swiss patent inventors',
                      xaxis = list(showgrid = TRUE),
                      legend.position = "none",
                      yaxis = list(showgrid = TRUE))
    })
  })  
}

shinyApp(ui = ui, server = server)