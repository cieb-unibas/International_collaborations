require(data.table)
library(dplyr)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
library(stringr)
library(rio)
library(fastDummies)
library(splitstackshape)
library(stringr)
library(dotwhisker)
library(broom)
library(dummies)
library(tidyr)
library(lmtest)
library(sandwich)
library(fixest)
library(plotly)

rm(list = ls())

dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")

 toppat_share <- dataregNoNA %>%
  group_by(ctry_leg_owner) %>% 
  summarise_at(vars("world_class_90"), mean,na.rm=TRUE)
 
 # Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 toppat_share <- filter(toppat_share, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 
 ggplot(toppat_share, aes(x = reorder(ctry_leg_owner, -world_class_90), y = world_class_90)) + geom_bar(stat = "identity")
 
 
 # Use only subset of home patents 
 ###############################################################################
 dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
 
 toppat_share_domoestic <- filter(dataregNoNA, d_inv==1)  
 toppat_share_domoestic <- filter(toppat_share_domoestic, p_year>2008)  
 
 toppat_share_domoestic <- toppat_share_domoestic %>%
   group_by(ctry_leg_owner) %>% 
   summarise_at(vars("world_class_90"), mean,na.rm=TRUE)
 
 # Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 toppat_share_domoestic <- filter(toppat_share_domoestic, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 
 ggplot(toppat_share_domoestic, aes(x = reorder(ctry_leg_owner, -world_class_90), y = world_class_90)) + 
    geom_bar(stat="identity") +
    scale_fill_viridis(discrete = T) +
    xlab("")
 
 # Use only subset of home patents adding tech and time dimension 
 ############################################################################### 
 dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
 
 toppat_share_domoestic <- filter(dataregNoNA, d_inv==1)  

 toppat_share_domoestic <- toppat_share_domoestic %>%
    group_by(ctry_leg_owner, time) %>% 
    summarise_at(vars("world_class_90"), mean,na.rm=TRUE)
 
 # Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 toppat_share_domoestic <- filter(toppat_share_domoestic, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 
 
 r3 <- plot_ly(
    toppat_share_domoestic, x = ~ctry_leg_owner, y = ~world_class_90, frame=~time,
    color=~ctry_leg_owner, type = "scatter",
    mode="markers", 
    text = ~paste('Share of inventors:', round(`world_class_90`,1),'%',
                  '<br>Location:', `ctry_leg_owner`), 
    hoverinfo = "text"
 ) %>%
    layout(
       title="Foreign collaboration and citation",
       
       xaxis = list(title = '% of inventors',
                    gridcolor = 'white',
                    
                    range=c(0,20),
                    zerolinewidth = F,
                    ticklen = 5,
                    gridwidth = 2),
       yaxis = list(title = 'ctry_leg_owner',
                    gridcolor = 'white',
                    zerolinewidth = F,
                    type = 'ctry_leg_owner',
                    range = c('ctry_leg_owner'),
                    ticklen = 5,
                    gridwith = 1),
       paper_bgcolor = 'white',
       plot_bgcolor = 'white') %>%
    animation_opts(
       2500, redraw = FALSE
    ) %>%
    
    animation_slider(
       currentvalue = list(prefix = "", font = list(color="black"))
    ) %>% config(displayModeBar = F)
 
 r3
 
 
 
 
 
 
 
 
 # toppat_count <- dataregNoNA %>%
 #   group_by(ctry_leg_owner) %>% 
 #   mutate(count_ctry = sum(world_class_90 == 1)) 
 # 
 # toppat_count2 <- toppat_count %>%
 #   mutate(count_tot = nrow(toppat_count)) 
 # 
 # toppat_count3 <- subset(toppat_count2, select = c("ctry_leg_owner", "count_ctry", "count_tot"))
 # 
 # toppat_share2 <- toppat_count3 %>%
 #   group_by(ctry_leg_owner) %>% 
 #   summarise_at(vars("count_ctry", "count_tot"), mean,na.rm=TRUE)
 # 
 # toppat_share2 <- filter(toppat_share2, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 # 
 # setDT(toppat_share2)[,share_top:=count_ctry/count_tot*100,by=list(ctry_leg_owner)]
 