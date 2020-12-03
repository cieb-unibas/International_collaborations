print("International collaborations and quality of patents COOPERATION TRENDS GRAPH")
# Last modified 02.12.2020 / DF

require(data.table)
require(plyr)
library(dplyr)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
library(stringr)
library("rio")
library(fastDummies)
library(splitstackshape)
library(stringr)
rm(list = ls())
library(plotly)
library(tidyverse)

################################################################################
# Preparing inventors data
################################################################################

trends_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_final.rds")

trends_data <- trends_data %>%
  group_by(owner_ctry, p_year) %>% 
  summarise_at(vars("share_foreign", "tot_cit"), mean)

#Filter data,vYear is filtered from 1990.
trends_data_graph<-trends_data %>% 
  filter(p_year >=1990, `owner_ctry`!='NA')
  
r3 <- plot_ly(
  trends_data_graph, x = ~`share_foreign`, y = ~`tot_cit`,
  frame=~p_year,
  color = ~`owner_ctry`, type = "scatter",
  mode="markers", colors=~colors, size=~`tot_cit`,
  marker = list(symbol = 'circle', sizemode = 'diameter',
                line = list(width = 2, color = '#FFFFFF'), opacity=0.4),
  text = ~paste(sep='','share_foreign:', round(`share_foreign`,1),'%',
                '<br>Citation:',`tot_cit`,
                '%', '<br>Owner:', `owner_ctry`)) %>%
  layout(
    title="Foreign collaboration and citation",
    
    xaxis = list(title = '% Foreign scientists',
                 gridcolor = 'rgb(255, 255, 255)',
                 
                 range=c(1,80),
                 zerolinewidth = 1,
                 ticklen = 5,
                 gridwidth = 2),
    yaxis = list(title = 'Citation',
                 gridcolor = 'rgb(255, 255, 255)',
                 range=c(1,11000),
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

r3
  