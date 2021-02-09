print("International collaborations and quality of patents COOPERATION TRENDS GRAPH")
# Last modified 09.02.2021 / DF

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

#trends_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_final.rds")

#trends_data[is.na(trends_data)] = 0

#Filter data,vYear is filtered from 1990.
#trends_data_graph<-trends_data %>%
# filter(p_year >=1990 & p_year <2015)

#setDT(trends_data_graph)[p_year>=1990 & p_year<1995, interval := "1990-1995"]
#trends_data_graph[p_year>=1995 & p_year<2000, interval := "1995-2000"]
#trends_data_graph[p_year>=2000 & p_year<2005, interval := "2000-2005"]
#trends_data_graph[p_year>=2005 & p_year<2010, interval := "2005-2010"]
#trends_data_graph[p_year>=2010 & p_year<2015, interval := "2010-2015"]


#trends_data_graph <- trends_data_graph %>%
# group_by(owner_ctry, interval) %>% 
# summarise_at(vars("share_foreign", "share_wc"), mean,na.rm=TRUE)


#worldclass <- trends_data_graph %>% 
#  filter(p_year ==1990 | p_year ==1995 | p_year ==2000 | p_year ==2005 | p_year ==2010)
#worldclass <- subset(worldclass, select = c("owner_ctry", "tech_field", "share_wc", "interval"))


#trends_data_graph<-trends_data_graph%>%
#   group_by(owner_ctry, tech_field,interval)%>%
#  summarise(Mean_share_foreign=mean(share_foreign,na.rm=TRUE))

#trends_data_graph <- subset(trends_data_graph, select = c("owner_ctry", "tech_field", "Mean_share_foreign", "interval"))

#trends_data_graph <-merge(trends_data_graph,worldclass)        

#trends_data_graph <- trends_data_graph %>%
#  filter(tech_field==1)

trends_data_graph <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")

trends_data_graph <- filter(trends_data_graph, techbroad %in% c("Chemistry"))

#trends_data_graph <- trends_data_graph %>% 
# filter(interval =="1990-1995" | interval =="1995-2000" | interval =="2000-2005" | interval =="2005-2010" | interval =="2010-2015")


r3 <- plot_ly(
  trends_data_graph, x = ~foreign, y = ~world_class_75,
  frame=~interval,
  color=~`ctry_leg_owner`, type = "scatter",
  mode="markers", size=~world_class_75,
  marker = list(symbol = 'circle', sizemode = 'diameter',
                line = list(width = 2, color = '#FFFFFF'), opacity=0.4),
  text = ~paste(sep='','foreign:', round(`foreign`,1),'%',
                '<br>Share WC patents:',round(`world_class_75`,1),'%',
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
                 range=c(0,25),
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
  
