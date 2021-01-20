print("International collaborations and quality of patents PREPARING DATA FOR REGRESSIONS")
# Last modified 04.01.2021 / DF

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

################################################################################
# Preparing inventors data
################################################################################

inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")
colnames(inv_data)
inv_data <- subset(inv_data, select = c("p_key", "patent_id", "ipc_main", "tech_field", "ctry_inventor", "world_class_90", "world_class_99"))

# Isolating years and uni indicator for each patent from another data set and leave only one observation per patent
owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year", "country", "uni"))
colnames(pyear_data) <- c("p_key", "patent_id", "p_year", "ctry_leg_owner", "uni")

# Making uni dummy to denote the presence of at least one university per patent (so fat it is for each applicant)

pyear_data <- pyear_data %>%
  group_by(patent_id) %>%
  mutate(uni_max = max(as.numeric(uni)))

# Making var containing all legal owner countries by patents
pyear_data <- pyear_data %>%
  group_by(p_key, patent_id, p_year, uni_max) %>%
  mutate(ctry_leg_owner = paste(ctry_leg_owner, collapse = "_")) 

# Keeping only first observation
pyear_data <- pyear_data %>% 
  group_by(p_key, patent_id, p_year, uni_max, ctry_leg_owner) %>% 
  slice(1) %>%
  ungroup()

colnames(pyear_data) <- c("p_key", "patent_id", "p_year" , "ctry_leg_owner", "uni" ,"uni_max")
pyear_data <- subset(pyear_data, select = c("p_key", "patent_id", "p_year", "ctry_leg_owner", "uni_max"))

# Merging years with main sample
inv_data_year <- merge(pyear_data, inv_data)
colnames(inv_data_year) <- c("p_key", "patent_id", "ctry_leg_owner", "p_year", "uni" ,"ipc_main","tech_field","ctry_inventor","world_class_90","world_class_99")

# Saving data where a patent is separately visible for each legal owner country (in case of more than 1 owners)
saveRDS(object=inv_data_year, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_year.rds")

################################################################################
# Preparing cit data
################################################################################

forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
colnames(forcit_data)
forcit_data <- subset(forcit_data, select = c("p_key", "pub_nbr_oecd", "claims", "tri_pat_fam", "originality", "radicalness"))
colnames(forcit_data) <- c("p_key", "patent_id", "claims", "tri_pat_fam", "originality", "radicalness")

# Merging years with main sample
collab_reg_data <- merge(inv_data_year, forcit_data)

setDT(collab_reg_data)[tech_field>=1 & tech_field<=8, techbroad := "Electrical engineering"]
setDT(collab_reg_data)[tech_field>=9 & tech_field<=13, techbroad := "Instruments"]
setDT(collab_reg_data)[tech_field>=14 & tech_field<=24, techbroad := "Chemistry"]
setDT(collab_reg_data)[tech_field>=25 & tech_field<=32, techbroad := "Mechanical engineering"]
setDT(collab_reg_data)[tech_field>=33 & tech_field<=35, techbroad := "Other fields"]

# Merging tech field labels with main sample

techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/oecd_tech_field.RDS")

collab_reg_data <- merge(collab_reg_data, techlab)

# Saving data for regression
saveRDS(object=collab_reg_data, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")








# Preparing cit data **** PREVIOUS CODE WHERE I LEFT THAT PATENT REPEATS IF MORE THAN ONE LEGAL OWNER


# # Inserting investor data
# inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")
# colnames(inv_data)
# inv_data <- subset(inv_data, select = c("p_key", "patent_id", "tech_field", "ctry_inventor","world_class_90", "world_class_99"))
# 
# 
# # Isolating years and uni indicator for each patent from another data set and leave only one observation per patent
# owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
# pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year", "country", "uni"))
# colnames(pyear_data) <- c("p_key", "patent_id", "p_year", "ctry_leg_owner", "uni")
# 
# # Merging years with main sample
# inv_data_year <- merge(pyear_data, inv_data)
# 
# # Saving data where a patent is separately visible for each legal owner country (in case of more than 1 owners)
# saveRDS(object=inv_data_year, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_year.rds")
# 
# ################################################################################
# # Preparing cit data
# ################################################################################
# 
# forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/pat_dat.rds")
# colnames(forcit_data)
# forcit_data <- subset(forcit_data, select = c("p_key", "pub_nbr_oecd", "claims", "tri_pat_fam", "originality", "radicalness"))
# colnames(forcit_data) <- c("p_key", "patent_id", "claims", "tri_pat_fam", "originality", "radicalness")
# 
# # Merging years with main sample
# collab_reg_data <- merge(inv_data_year, forcit_data)
# 
# setDT(collab_reg_data)[tech_field>=1 & tech_field<=8, techbroad := "Electrical engineering"]
# setDT(collab_reg_data)[tech_field>=9 & tech_field<=13, techbroad := "Instruments"]
# setDT(collab_reg_data)[tech_field>=14 & tech_field<=24, techbroad := "Chemistry"]
# setDT(collab_reg_data)[tech_field>=25 & tech_field<=32, techbroad := "Mechanical engineering"]
# setDT(collab_reg_data)[tech_field>=33 & tech_field<=35, techbroad := "Other fields"]
# 
# # Merging tech field labels with main sample
# 
# techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/oecd_tech_field.RDS")
# 
# collab_reg_data <- merge(collab_reg_data, techlab)
# 
# # Saving data for regression
# saveRDS(object=collab_reg_data, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")
# 
