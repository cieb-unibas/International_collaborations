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
 toppat_share_domoestic <- filter(toppat_share_domoestic, p_year>=2010)  

 toppat_share_domoestic <- toppat_share_domoestic %>%
   group_by(ctry_leg_owner, techbroad) %>% 
   summarise_at(vars("world_class_90"), mean,na.rm=TRUE)
 
 setDT(toppat_share_domoestic)[,world_class_90:=world_class_90*100]
 
 # Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 toppat_share_domoestic <- filter(toppat_share_domoestic, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 
 # Saving data for regression
 toppat_share_domoestic %>% saveRDS(file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/toppat_share_domoestic.rds")
 

