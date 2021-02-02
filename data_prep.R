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
  inventors_data2 <-subset(inventors_data, select = c("p_key", "regio_pat", "ctry_pat", "ipc_main", "tech_field"))
#CR: I rename ctry_pat and regio_pat in order to keep the rest of the code unchanged
  inventors_data2 <- dplyr::rename(inventors_data2, ctry_inv = ctry_pat, regio_inv = regio_pat)
  
  
# Making var containing all inventor countries by patents
  inventors_data3 <- inventors_data2 %>%
                      group_by(p_key) %>%
                      mutate(ctry_inventor = paste(ctry_inv, collapse = "_")) %>%
                      mutate(regio_inventor = paste(regio_inv, collapse = "_"))

# Keeping only first observation
  inventors_data_final <- inventors_data3 %>% 
                        group_by(p_key) %>% 
                        slice(1) %>%
                        ungroup()
  
  # drop data on region of inventor
  inventors_data_final=subset(inventors_data_final,select=-c(regio_inv,regio_inventor))

# Preparing legal owner data
################################################################################
  owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
  
# Making uni dummy to denote the presence of at least one university per patent (so fat it is for each applicant)
  owner_data <- setDT(owner_data)[, c("uni_dummy", "num") := list(sum(uni), .N), .(p_key)]
  owner_data <- mutate(owner_data, uni_priv = ifelse(uni_dummy/num != 1 & uni_dummy > 0, 1, 0),
                                   uni = ifelse(uni_dummy/num == 1, 1, 0))
  
  owner_data2 <-  distinct(owner_data, p_key, country, uni, uni_priv)
  colnames(owner_data2) <- c("p_key", "ctry_owner", "uni", "uni_priv")
  
  
# Making var containing all legal owner countries by patents
  owner_data3 <- owner_data2 %>%
                 group_by(p_key) %>%
                 mutate(ctry_leg_owner = paste(ctry_owner, collapse = "_")) 

# Keeping only first observation
  owner_data_final <- owner_data3 %>% 
                      group_by(p_key) %>% 
                      slice(1) %>%
                      ungroup()
  


# Preparing citation data / keep only USPTO data
################################################################################
  forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
  forcit_data <- filter(forcit_data, pat_off == "US")
  forcit_data <- setDT(forcit_data)[order(pub_date), .SD[1], .(p_key, pat_off, tech_field)]
  forcit_data2 <-subset(forcit_data, select = c("p_key", "p_year", "world_class_50", "world_class_75", "world_class_90", "world_class_99", "claims", "tri_pat_fam", "originality", "radicalness", "tech_field"))

# Add broad technology field
################################################################################
  setDT(forcit_data2)[tech_field>=1 & tech_field<=8, techbroad := "Electrical engineering"]
  setDT(forcit_data2)[tech_field>=9 & tech_field<=13, techbroad := "Instruments"]
  setDT(forcit_data2)[tech_field>=14 & tech_field<=24, techbroad := "Chemistry"]
  setDT(forcit_data2)[tech_field>=25 & tech_field<=32, techbroad := "Mechanical engineering"]
  setDT(forcit_data2)[tech_field>=33 & tech_field<=35, techbroad := "Other fields"]  
 
  techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/oecd_tech_field.RDS")
  forcit_data2 <- left_join(forcit_data2, techlab, by = "tech_field")
  
# Merging all three datasets together
################################################################################
  inv_owner_dat <- merge(inventors_data_final, owner_data_final, by = "p_key")  
  inv_owner_dat <- dplyr::select(inv_owner_dat, -tech_field)
  int_collab_dat <- merge(inv_owner_dat, forcit_data2, by = "p_key")  
  
  int_collab_dat_final <-subset(int_collab_dat, select = c("p_key", "p_year", "ipc_main", "tech_field", "ctry_inventor", "ctry_leg_owner", "world_class_50", "world_class_75", "world_class_90", "world_class_99", "claims", "tri_pat_fam", "originality", "radicalness", "techbroad", "tech_name", "uni", "uni_priv"))

# Saving data for regression
  int_collab_dat_final %>% saveRDS(file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")
  
  
  
