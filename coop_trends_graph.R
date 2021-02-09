print("International collaborations and quality of patents COOPERATION TRENDS GRAPH")
# Last modified 09.02.2021 / DF

require(data.table)
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

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")

# LEAVE ONLY PATENTS ONE OWNER
datareg$num_owners <- str_count(datareg$ctry_leg_owner, '_')
datareg$num_owners <- datareg$num_owners+1

datareg_one_owner <- datareg %>%
  filter(num_owners==1)

datareg_one_owner=subset(datareg_one_owner,select = -c(num_owners))

datareg <- datareg_one_owner

# Variable capturing number of scientist involved
datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
datareg$num_tot_scient <- datareg$num_tot_scient+1

# Variable capturing number of domestic scientist involved
 datareg$num_dom_scient <- str_count(datareg$ctry_inventor, datareg$ctry_leg_owner) 

# Calculate "foreign" scientists
 datareg$num_for_scient <- datareg$num_tot_scient-datareg$num_dom_scient

# Create "foreign scientists" dummy
 datareg$foreign <- ifelse(datareg$num_for_scient>0,1,0)
 
## Create time periods
 # Filter data, Year is filtered from 1980 to 2015 and creating 5 year intervals
 datareg<-datareg %>%
   filter(p_year >=1990 & p_year <2015)
 
 #setDT(datareg)[p_year>=1980 & p_year<1985, interval := "1980-1985"]
 #setDT(datareg)[p_year>=1985 & p_year<1990, interval := "1985-1990"]
 setDT(datareg)[p_year>=1990 & p_year<1995, interval := "1990-1995"]
 setDT(datareg)[p_year>=1995 & p_year<2000, interval := "1995-2000"]
 setDT(datareg)[p_year>=2000 & p_year<2005, interval := "2000-2005"]
 setDT(datareg)[p_year>=2005 & p_year<2010, interval := "2005-2010"]
 setDT(datareg)[p_year>=2010 & p_year<2015, interval := "2010-2015"]
 

# Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 datareg <- filter(datareg, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  

# Aggregating data (collapsing) to technology, year and owner country and taking means
 #datareg_agregated <- datareg %>%
 # group_by(techbroad, ctry_leg_owner, interval) %>% 
 #summarise_at(vars("foreign", "world_class_90"), mean, na.rm = TRUE) 
 
 datareg_agregated <- datareg %>%
    group_by(techbroad, ctry_leg_owner, interval) %>% 
    summarise_at(vars("foreign", "world_class_90"), mean, na.rm = TRUE) 
 
 
 datareg_agregated$foreign <- datareg_agregated$foreign*100 
 datareg_agregated$world_class_90 <- datareg_agregated$world_class_90*100 
 

 # SAVING FINAL DATA FOR TRENDS GRAPH
 
 saveRDS(object=datareg_agregated, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")
 




























 ################################################################################
 #*******FORMER CODE******
 ################################################################################

################################################################################
# Preparing inventors data
################################################################################

# # Inserting investor data
#   inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")
# 
# # FILTER 10 patent owner countries
#   own_ctry_CN <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "CN"))
#   own_ctry_CN$owner_ctry <- "CN"
#   
#   own_ctry_US <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "US"))
#   own_ctry_US$owner_ctry <- "US"
#   
#   own_ctry_DE <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "DE"))
#   own_ctry_DE$owner_ctry <- "DE"
#   
#   own_ctry_CH <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "CH"))
#   own_ctry_CH$owner_ctry <- "CH"
#   
#   own_ctry_JP <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "JP"))
#   own_ctry_JP$owner_ctry <- "JP"
#   
#   own_ctry_AU <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "AU"))
#   own_ctry_AU$owner_ctry <- "AU"
#   
#   own_ctry_GB <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "GB"))
#   own_ctry_GB$owner_ctry <- "GB"
#   
#   own_ctry_FR <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "FR"))
#   own_ctry_FR$owner_ctry <- "FR"
#   
#   own_ctry_ES <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "ES"))
#   own_ctry_ES$owner_ctry <- "ES"
#   
#   own_ctry_IT <- inv_data %>%
#     filter(str_detect(ctry_leg_owner, "IT"))
#   own_ctry_IT$owner_ctry <- "IT"
#   
#   own_10ctry_sample <- rbind(own_ctry_CN, own_ctry_US, own_ctry_DE,own_ctry_CH,own_ctry_JP ,own_ctry_AU, own_ctry_GB, own_ctry_FR,own_ctry_ES,own_ctry_IT)
#   
#   own_10ctry_sample <- subset(own_10ctry_sample, select = c("p_key", "patent_id", "owner_ctry"))
# 
# # Merging 10 country samples with the main sample to keep only those 10
# 
#   inv_data_10_own_ctry<-merge(inv_data, own_10ctry_sample)
#   inv_data_10_own_ctry <- subset(inv_data_10_own_ctry, select = c("p_key", "patent_id", "tech_field", "ctry_inventor", "owner_ctry"))
# 
# # Isolating years for each patent from another data set and leave only one observation per patent
#   owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
#   pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year"))
#   colnames(pyear_data) <- c("p_key", "patent_id", "p_year")
#   
#   pyear_data <- pyear_data %>% 
#     group_by(p_key, patent_id, p_year) %>% 
#     slice(1) %>%
#     ungroup()
# 
# # Merging years with main sample
#   inv_data_10_own_ctry_f <- merge(inv_data_10_own_ctry, pyear_data)
#   
# # SAVING INVENTORS DATA WITH 10 patent owner countries
#   
#    saveRDS(object=inv_data_10_own_ctry_f, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_10_own_ctry_f.rds")
#   
# # LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
#   
#   inv_data_10_own_ctry_f <- inv_data_10_own_ctry_f %>% 
#     group_by(p_key, patent_id, p_year,tech_field, owner_ctry) %>% 
#     dplyr::mutate(inv_from_own = grepl(owner_ctry,ctry_inventor)) %>%
#     ungroup()
#   
#   inv_data_10_own_ctry_f <- inv_data_10_own_ctry_f %>% 
#       filter(inv_from_own ==TRUE)
# 
# # Counting "domestic" scientists (if their residence match owners country)   
#   inv_data_10_own_ctry_f$num_dom <- str_count(inv_data_10_own_ctry_f$ctry_inventor, inv_data_10_own_ctry_f$owner_ctry)
#   
# # Counting "total" scientists    
#   inv_data_10_own_ctry_f$num_total <- str_count(inv_data_10_own_ctry_f$ctry_inventor, '_')
#   inv_data_10_own_ctry_f$num_total <- inv_data_10_own_ctry_f$num_total+1
#   
# # Calculate "foreign" scientists    
#   inv_data_10_own_ctry_f$num_foreign <- inv_data_10_own_ctry_f$num_total-inv_data_10_own_ctry_f$num_dom
#   
#   inv_data_10_own_ctry_f <- subset(inv_data_10_own_ctry_f, select = c(p_key, patent_id, p_year,tech_field, ctry_inventor, owner_ctry, num_foreign, num_total))
#   
#   
#   # SAVING FINAL INVENTORS DATA WITH 10 patent owner countries
#   
#   saveRDS(object=inv_data_10_own_ctry_f, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_10_own_ctry_f.rds")
#   
#   inv_data_10_own_ctry_f <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_10_own_ctry_f.rds")
#   
#   
# ################################################################################
# # Preparing cit data
# ################################################################################
#   
#   forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
#   forcit_data <-subset(forcit_data, select = c("p_key","pub_nbr_oecd","p_year", "world_class_90"))
#   colnames(forcit_data) <- c("p_key", "patent_id","p_year", "world_class_90")
#   forcit_data <-subset(forcit_data, select = c("p_key", "patent_id", "world_class_90"))
# 
# ################################################################################
# #Merging citation data with inventors data
# ################################################################################
#   
#   inv_cit_data_10_own_ctry <- merge(inv_data_10_own_ctry_f,forcit_data)
#   saveRDS(object=inv_cit_data_10_own_ctry, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_cit_data_10_own_ctry.rds")
#   inv_cit_data_10_own_ctry <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_cit_data_10_own_ctry.rds")
#   
# # Creating broader tech groups
#   
#   setDT(inv_cit_data_10_own_ctry)[tech_field>=1 & tech_field<=8, techbroad := "Electrical engineering"]
#   setDT(inv_cit_data_10_own_ctry)[tech_field>=9 & tech_field<=13, techbroad := "Instruments"]
#   setDT(inv_cit_data_10_own_ctry)[tech_field>=14 & tech_field<=24, techbroad := "Chemistry"]
#   setDT(inv_cit_data_10_own_ctry)[tech_field>=25 & tech_field<=32, techbroad := "Mechanical engineering"]
#   setDT(inv_cit_data_10_own_ctry)[tech_field>=33 & tech_field<=35, techbroad := "Other fields"]
#   
# # Filter data, Year is filtered from 1980 to 2015 and creating 5 year intervals
#   inv_cit_data_10_own_ctry<-inv_cit_data_10_own_ctry %>%
#     filter(p_year >=1980 & p_year <2015)
#   
#   setDT(inv_cit_data_10_own_ctry)[p_year>=1980 & p_year<1985, interval := "1980-1985"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=1985 & p_year<1990, interval := "1985-1990"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=1990 & p_year<1995, interval := "1990-1995"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=1995 & p_year<2000, interval := "1995-2000"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=2000 & p_year<2005, interval := "2000-2005"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=2005 & p_year<2010, interval := "2005-2010"]
#   setDT(inv_cit_data_10_own_ctry)[p_year>=2010 & p_year<2015, interval := "2010-2015"]
#   
#   
# # Calculating total number of patents, per broad technology, interval and owner
#   
#   inv_cit_data_10_own_ctry <- inv_cit_data_10_own_ctry %>% 
#     group_by(techbroad, owner_ctry, interval) %>%
#     mutate(totnumber_pat = n()) %>%
#     ungroup()
#   
# # Calculating share of world class patents, per broad technology, interval and owner
#   
#   inv_cit_data_10_own_ctry <- inv_cit_data_10_own_ctry %>% 
#     group_by(techbroad, owner_ctry, interval) %>% 
#     mutate(totnumber_wcpat = sum(world_class_90 == 1,na.rm=TRUE)) %>%
#     ungroup()
#   
# # Calculating total number of scientists in patents, per broad technology, interval and owner
#   
#   inv_cit_data_10_own_ctry <- inv_cit_data_10_own_ctry %>% 
#     group_by(techbroad, owner_ctry, interval) %>% 
#     dplyr::mutate(totnumber_inv = sum(num_total,na.rm=TRUE)) %>%
#     ungroup()
#   
# # Calculating foreign number of scientists in patents, per broad technology, interval and owner
#   
#   inv_cit_data_10_own_ctry <- inv_cit_data_10_own_ctry %>% 
#     group_by(techbroad, owner_ctry, interval) %>% 
#     dplyr::mutate(totnumber_foreign_inv = sum(num_foreign,na.rm=TRUE)) %>%
#     ungroup()
#  
# # Aggregating data (collapsing) to technology, year and owner country and taking means
#   inv_cit_data_10_own_ctry_agregated <- inv_cit_data_10_own_ctry %>%
#                           group_by(techbroad, owner_ctry, interval) %>% 
#                           summarise_at(vars("totnumber_foreign_inv", "totnumber_inv", "totnumber_wcpat", "totnumber_pat"), mean) 
#   
# # Calculate shares of foreign inventors and world class patents
#   setDT(inv_cit_data_10_own_ctry_agregated)[,share_wc:=totnumber_wcpat/totnumber_pat,by=list(techbroad, owner_ctry, interval)]
#   options(scipen=999)
#   inv_cit_data_10_own_ctry_agregated$share_wc <- inv_cit_data_10_own_ctry_agregated$share_wc*100 
#   
#   setDT(inv_cit_data_10_own_ctry_agregated)[,share_foreign:=totnumber_foreign_inv/totnumber_inv,by=list(techbroad, owner_ctry, interval)]
#   options(scipen=999)
#   inv_cit_data_10_own_ctry_agregated$share_foreign <- inv_cit_data_10_own_ctry_agregated$share_foreign*100 
#   
# # SAVING FINAL DATA FOR TRENDS GRAPH
#   
#   saveRDS(object=inv_cit_data_10_own_ctry_agregated, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")
  

  
  
  
  
  
  
  
  
  
  
  




