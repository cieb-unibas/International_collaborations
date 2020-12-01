print("International collaborations and quality of patents COOPERATION TRENDS GRAPH")
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
# Preparing data
################################################################################

# Inserting network data
inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")

# Show countries of inventors for each owned patent in "long format"
s <- strsplit(inv_data$ctry_inventor, split = "_")
inv_data_split <- data.frame(patent_id = rep(inv_data$patent_id, sapply(s, length)), ctry_inventor = unlist(s))

# Adding tech fields to long format "long format"
inv_techf <- subset(inv_data, select = c("p_key", "patent_id", "tech_field", "ctry_leg_owner"))

inv_techf_4ctry <- inv_techf %>%
            filter(str_detect(ctry_leg_owner, "CN|US|DE|CH"))

network_coll_data <- merge(inv_data_split, inv_techf)

# Calculating the number of foreign scientists in Swiss patents, per technology

inv_techf_coun <- subset(network_coll_data, select = c("ctry_inventor", "tech_field"))

inv_techf_coun_f <- inv_techf_coun %>% 
  group_by(ctry_inventor, tech_field) %>%
  summarise(number = n())

# Calculating and leaving only the share of foreign scientists in Swiss patents, per technology

setDT(inv_techf_coun_f)[,totnumber:=sum(number),by=list(tech_field)]
setDT(inv_techf_coun_f)[,share_foreign:=number/totnumber,by=list(ctry_inventor, tech_field)]
options(scipen=999)
inv_techf_coun_f$share_foreign <- inv_techf_coun_f$share_foreign*100  
