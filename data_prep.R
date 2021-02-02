print("International collaborations and quality of patents")
# Last modified 01.02.2020 / DF

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
rm(list = ls())

################################################################################
# Creating data from multiple datasets # 
################################################################################

# Preparing inventor data
################################################################################
  inventors_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_reg_CHcommute_adj.rds")
#CR: Use ctry_pat and regio_pat in order to consider Swiss cross-border commuters 
  inventors_data2 <-subset(inventors_data, select = c("p_key", "patent_id", "regio_pat", "ctry_pat", "ipc_main", "tech_field"))
#CR: I rename ctry_pat and regio_pat in order to keep the rest of the code unchanged
  inventors_data2 <- dplyr::rename(inventors_data2, ctry_inv = ctry_pat, regio_inv = regio_pat)
  
  
# Making var containing all inventor countries by patents
  inventors_data3 <- inventors_data2 %>%
                      group_by(p_key, patent_id) %>%
                      mutate(ctry_inventor = paste(ctry_inv, collapse = "_")) %>%
                      mutate(regio_inventor = paste(regio_inv, collapse = "_"))

# Keeping only first observation
  inventors_data_final <- inventors_data3 %>% 
                        group_by(p_key, patent_id) %>% 
                        slice(1) %>%
                        ungroup()
  
  # drop data on region of inventor
  
  inventors_data_final=subset(inventors_data_final,select=-c(regio_inv,regio_inventor))
  

# Preparing legal owner data
################################################################################
  owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
  owner_data2 <-subset(owner_data, select = c("p_key","pub_nbr", "country"))
  colnames(owner_data2) <- c("p_key", "patent_id", "ctry_owner")

# Making var containing all legal owner countries by patents
  owner_data3 <- owner_data2 %>%
                 group_by(p_key, patent_id) %>%
                 mutate(ctry_leg_owner = paste(ctry_owner, collapse = "_")) 

# Keeping only first observation
  owner_data_final <- owner_data3 %>% 
                      group_by(p_key, patent_id) %>% 
                      slice(1) %>%
                      ungroup()

# Preparing citation data
################################################################################
  forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
  forcit_data2 <-subset(forcit_data, select = c("p_key","pub_nbr_oecd", "world_class_50", "world_class_75", "world_class_90", "world_class_99"))
  colnames(forcit_data2) <- c("p_key", "patent_id", "world_class_50", "world_class_75", "world_class_90", "world_class_99")
  
# Merging all three datasets together
################################################################################
  
  inv_owner_dat <- merge(inventors_data_final, owner_data_final)  
  int_collab_dat <- merge(inv_owner_dat, forcit_data2)  
  
  int_collab_dat_final <-subset(int_collab_dat, select = c("p_key","patent_id", "ipc_main", "tech_field", "ctry_inventor", "ctry_leg_owner", "world_class_50", "world_class_75", "world_class_90", "world_class_99"))
  
 #SAVING THE FINAL DATASET
  saveRDS(object=int_collab_dat_final, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")

  
# Test commit MN/30.11.2020
    