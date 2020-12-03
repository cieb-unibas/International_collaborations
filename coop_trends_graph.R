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

################################################################################
# Preparing inventors data
################################################################################

# Inserting network data
  inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")

# Show countries of inventors for each owned patent in "long format"
  s <- strsplit(inv_data$ctry_inventor, split = "_")
  inv_data_split <- data.frame(patent_id = rep(inv_data$patent_id, sapply(s, length)), ctry_inventor = unlist(s))

# Adding all fields to long format "long format"
  inv_techf <- subset(inv_data, select = c("p_key", "patent_id", "tech_field", "ctry_leg_owner"))

  graph_coll_data <- merge(inv_data_split, inv_techf)


# ISOLATING CN US DE CH owned patents

  own_ctry_CN <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "CN"))
  own_ctry_CN$owner_ctry <- "CN"
  
  own_ctry_US <- graph_coll_data %>%
     filter(str_detect(ctry_leg_owner, "US"))
  own_ctry_US$owner_ctry <- "US"
  
  own_ctry_DE <- graph_coll_data %>%
     filter(str_detect(ctry_leg_owner, "DE"))
  own_ctry_DE$owner_ctry <- "DE"
  
  own_ctry_CH <- graph_coll_data %>%
     filter(str_detect(ctry_leg_owner, "CH"))
  own_ctry_CH$owner_ctry <- "CH"
  
  own_4ctry_sample <- rbind(own_ctry_CN, own_ctry_US, own_ctry_DE,own_ctry_CH)
  
  own_4ctry_sample <- subset(own_4ctry_sample, select = c("p_key", "patent_id", "owner_ctry"))
  

# Merging 4 country samples with the main sample to keep only those 4
  
  graph_coll_data_4ctry<-merge(graph_coll_data, own_4ctry_sample)
  graph_coll_data_4ctry <- subset(graph_coll_data_4ctry, select = c("p_key", "patent_id", "tech_field", "ctry_inventor", "owner_ctry"))
  
  
# Isolating years for each patent
  owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
  pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year"))
  colnames(pyear_data) <- c("p_key", "patent_id", "p_year")
  
  
# Merging years with main sample
  graph_coll_data_4ctry_year <- merge(graph_coll_data_4ctry, pyear_data)
  
  
# Calculating total number of scientists in patents, per technology and owner
  
  graph_coll_data_4ctry_year_totn <- graph_coll_data_4ctry_year %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    mutate(totnumber_inv = n()) %>%
    ungroup()

# Calculating foreign number of scientists in patents, per technology and owner
  
  graph_coll_data_4ctry_year_totnforn <- graph_coll_data_4ctry_year_totn %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    mutate(totnumber_foreign_inv = sum(ctry_inventor != owner_ctry)) %>%
    ungroup()
 
# Calculating share of foreign scientists in patents, per technology and owner
  
  setDT(graph_coll_data_4ctry_year_totnforn)[,share_foreign:=totnumber_foreign_inv/totnumber_inv,by=list(owner_ctry, tech_field, p_year)]
  options(scipen=999)

# leaving only first observation for each patent (excluding multiple inventors rows actually)

  coll_trends_invshare <- graph_coll_data_4ctry_year_totnforn %>% 
    group_by(p_key, patent_id) %>% 
    slice(1) %>%
    ungroup()

  coll_trends_invshare$share_foreign <- coll_trends_invshare$share_foreign*100 

  coll_trends_invshare <- subset(coll_trends_invshare, select = c("p_key", "patent_id", "p_year", "tech_field", "owner_ctry", "share_foreign"))


  saveRDS(object=coll_trends_invshare, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_invshare.rds")


################################################################################
# Preparing cit data
################################################################################

  forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
  forcit_data <-subset(forcit_data, select = c("p_key","pub_nbr_oecd","p_year", "fwd_cits7"))
  colnames(forcit_data) <- c("p_key", "patent_id","p_year", "fwd_cits7")
  forcit_data <-subset(forcit_data, select = c("p_key", "patent_id", "fwd_cits7"))
  
  saveRDS(object=forcit_data, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/forcit_data.rds")
  
################################################################################
# Merging cit data with prepared inventors data
################################################################################
  
   coll_trends_f <- merge(coll_trends_invshare,forcit_data)
  
# Calculating citations of patents, per technology and owner
  
  coll_trends_final <- coll_trends_f %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    mutate(tot_cit =sum(fwd_cits7)) %>%
    ungroup()
  
  coll_trends_final <-subset(coll_trends_final, select = c("p_year", "tech_field", "owner_ctry", "share_foreign", "tot_cit"))
  
#Transforming into long format  
  # coll_trends_final <-melt(setDT(coll_trends_final), id.vars = c("p_year", "tech_field", "owner_ctry"), measure.vars = c("share_foreign", "tot_cit"))
  
  saveRDS(object=coll_trends_final, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_final.rds")
  