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
rm(list = ls())

dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")

 toppat_share <- dataregNoNA %>%
  group_by(ctry_leg_owner) %>% 
  summarise_at(vars("world_class_90"), mean,na.rm=TRUE)
 
 # Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 toppat_share <- filter(toppat_share, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
 
 ggplot(toppat_share, aes(x = reorder(ctry_leg_owner, -world_class_90), y = world_class_90)) + geom_bar(stat = "identity")
 
 
 
 
 
 
 
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
 