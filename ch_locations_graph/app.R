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

ch_locations <- readRDS("inv_coll_ch_networkdata.rds")

ch_locations <- subset(ch_locations, location !="CH")
ch_locations <- ch_locations[order(ch_locations$location_name), ]

ui <- fluidPage(
  
  selectizeInput("techbroad", "Choose a technology field",
                 options = list(placeholder = 'Technology fields'),
                 choices = c(ch_locations$techbroad), multiple = T, selected = "Total"),
  plotlyOutput("coolplot"),
  
  
  
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

  if(nrow(dr())!= 0){   
        
  fig <- plot_ly(
  dr(), x = ~location, y = ~share, frame=~interval,
  color=~techbroad, type = "bar",
  colors = viridis_pal(option = "D", begin = 0, end = 1, direction = -1)(length(unique(dr()$techbroad))),
  mode="markers", 
  text = ~paste('Location:', `location_name`,
                '<br>Share of inventors:', round(`share`,1),'%'), 
  hoverinfo = "text"
  ) %>%
  layout(
    title="Foreign collaboration and citation",
    
    yaxis = list(title = '% of inventors\n of Swiss-based firms',
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
    plot_bgcolor = 'white',
    margin = list(r = 25)) %>%
  animation_opts(
    1200, redraw = FALSE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "", font = list(color="black"))
  ) %>% config(displayModeBar = F)

fig <- fig %>% layout(title = F,
                      xaxis = list(showgrid = TRUE, fixedrange = TRUE),
                      legend.position = "none",
                      yaxis = list(showgrid = TRUE, fixedrange = TRUE))
} else {}
    })
  })  
}

shinyApp(ui = ui, server = server)