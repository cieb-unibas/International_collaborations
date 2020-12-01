print("International collaborations and quality of patents NET GRAPH")
# Last modified 01.12.2020 / DF

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

################################################################################
# Preparing network data
################################################################################

# Inserting data
  inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")

# Filter out patents where (at least one) legal owner comes from CH
  inv_data_ch <-  inv_data %>%
                filter(str_detect(ctry_leg_owner, "CH"))

# Show countries of inventors for each Swiss owned patent in "long format"
  s <- strsplit(inv_data_ch$ctry_inventor, split = "_")
  inv_data_ch_split <- data.frame(patent_id = rep(inv_data_ch$patent_id, sapply(s, length)), ctry_inventor = unlist(s))

# Adding tech fields to long format "long format"
  inv_ch_techf <- subset(inv_data_ch, select = c("patent_id", "tech_field"))
  network_coll_data_ch <- merge(inv_data_ch_split, inv_ch_techf)
  
# Calculating the number of foreign scientists in Swiss patents, per technology
  
  inv_ch_techf_coun <- subset(network_coll_data_ch, select = c("ctry_inventor", "tech_field"))
  
  inv_ch_techf_coun_f <- inv_ch_techf_coun %>% 
    group_by(ctry_inventor, tech_field) %>%
    summarise(number = n())

# Calculating and leaving only the share of foreign scientists in Swiss patents, per technology
  
  setDT(inv_ch_techf_coun_f)[,totnumber:=sum(number),by=list(tech_field)]
  setDT(inv_ch_techf_coun_f)[,share_foreign:=number/totnumber,by=list(ctry_inventor, tech_field)]
  options(scipen=999)
  inv_ch_techf_coun_f$share_foreign <- inv_ch_techf_coun_f$share_foreign*100  
  
  inv_ch_techf_coun_f <- subset(inv_ch_techf_coun_f, select = c("ctry_inventor", "tech_field", "share_foreign"))
  
# Inserting labels for technology fields
  techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/02_section_I_data/oecd_tech_field.RDS")
  colnames(techlab) <- c("tech_field", "tech_name")
  
  inv_ch_techf_coun_final<-merge(inv_ch_techf_coun_f, techlab)
  
  
# Saving data for network graph as RDS in Scicore
  saveRDS(object=inv_ch_techf_coun_final, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_ch_networkdata.rds")
  