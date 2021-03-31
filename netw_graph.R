print("International collaborations and quality of patents NET GRAPH")
# Last modified 17.02.2021 / DF

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

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")

# LEAVE ONLY PATENTS ONE OWNER
datareg$num_owners <- str_count(datareg$ctry_leg_owner, '_')
datareg$num_owners <- datareg$num_owners+1

datareg_one_owner <- datareg %>%
  filter(num_owners==1)

datareg_one_owner=subset(datareg_one_owner,select = -c(num_owners))

datareg <- datareg_one_owner

# Filter data, Year is filtered from 1980 to 2015 and creating 5 year intervals
datareg<-datareg %>%
  filter(p_year >1990 & p_year <=2015)

 setDT(datareg)[p_year>1990 & p_year<=1995, interval := "1991-1995"]
 setDT(datareg)[p_year>=1996 & p_year<=2000, interval := "1996-2000"]
 setDT(datareg)[p_year>=2001 & p_year<=2005, interval := "2001-2005"]
 setDT(datareg)[p_year>=2006 & p_year<=2010, interval := "2006-2010"]
 setDT(datareg)[p_year>=2011 & p_year<=2015, interval := "2011-2015"]
 

# Filter out patents where legal owner comes from CH
 datareg <-  datareg %>%
  filter(str_detect(ctry_leg_owner, "CH"))

# Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
 # datareg <- filter(datareg, ctry_inventor %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  

# Variable capturing number of scientist involved
 datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
 datareg$num_tot_scient <- datareg$num_tot_scient+1
 
 # Variable capturing number of number of foreign scientist involved by residence
 datareg$num_AT_scient <- str_count(datareg$ctry_inventor, "AT") 
 datareg$num_CH_scient <- str_count(datareg$ctry_inventor, "CH") 
 datareg$num_IL_scient <- str_count(datareg$ctry_inventor, "IL") 
 datareg$num_DK_scient <- str_count(datareg$ctry_inventor, "DK") 
 datareg$num_BE_scient <- str_count(datareg$ctry_inventor, "BE") 
 datareg$num_FI_scient <- str_count(datareg$ctry_inventor, "FI") 
 datareg$num_CA_scient <- str_count(datareg$ctry_inventor, "CA") 
 datareg$num_US_scient <- str_count(datareg$ctry_inventor, "US") 
 datareg$num_SE_scient <- str_count(datareg$ctry_inventor, "SE") 
 datareg$num_IT_scient <- str_count(datareg$ctry_inventor, "IT") 
 datareg$num_KR_scient <- str_count(datareg$ctry_inventor, "KR") 
 datareg$num_GB_scient <- str_count(datareg$ctry_inventor, "GB") 
 datareg$num_DE_scient <- str_count(datareg$ctry_inventor, "DE") 
 datareg$num_FR_scient <- str_count(datareg$ctry_inventor, "FR") 
 datareg$num_JP_scient <- str_count(datareg$ctry_inventor, "JP") 
 datareg$num_NO_scient <- str_count(datareg$ctry_inventor, "NO") 
 datareg$num_ES_scient <- str_count(datareg$ctry_inventor, "ES") 
 datareg$num_NL_scient <- str_count(datareg$ctry_inventor, "NL") 
 datareg$num_IE_scient <- str_count(datareg$ctry_inventor, "IE") 
 datareg$num_SG_scient <- str_count(datareg$ctry_inventor, "SG") 

# # Show countries of inventors for each Swiss owned patent in "long format"
# s <- strsplit(inv_data_ch$ctry_inventor, split = "_")
# inv_data_ch_split <- data.frame(p_key = rep(inv_data_ch$p_key, sapply(s, length)), ctry_inventor = unlist(s))
# 
# # Adding tech fields to long format "long format"
# inv_ch_techf <- subset(inv_data_ch, select = c("p_key", "tech_field"))
# network_coll_data_ch <- merge(inv_data_ch_split, inv_ch_techf)

# # Calculating the number of foreign scientists in Swiss patents, per technology
# 
# inv_ch_techf_coun <- subset(network_coll_data_ch, select = c("ctry_inventor", "tech_field"))
 
 
 inv_ch_techf_coun_f <- datareg %>% 
   group_by(techbroad, interval) %>%
   summarise_at(vars("num_tot_scient", "num_AT_scient" , "num_CH_scient" , "num_IL_scient" , "num_DK_scient", "num_BE_scient", "num_FI_scient", "num_CA_scient", "num_US_scient", "num_SE_scient", "num_IT_scient", "num_KR_scient", "num_GB_scient", "num_DE_scient", "num_FR_scient", "num_JP_scient", "num_NO_scient", "num_ES_scient", "num_NL_scient", "num_IE_scient", "num_SG_scient"), sum, na.rm = TRUE)

# Creating a total category 
 inv_ch_techf_coun_f_total <- datareg %>% 
   group_by(interval) %>%
   summarise_at(vars("num_tot_scient", "num_AT_scient" , "num_CH_scient" , "num_IL_scient" , "num_DK_scient", "num_BE_scient", "num_FI_scient", "num_CA_scient", "num_US_scient", "num_SE_scient", "num_IT_scient", "num_KR_scient", "num_GB_scient", "num_DE_scient", "num_FR_scient", "num_JP_scient", "num_NO_scient", "num_ES_scient", "num_NL_scient", "num_IE_scient", "num_SG_scient"), sum, na.rm = TRUE)
 
 setDT(inv_ch_techf_coun_f_total)[, techbroad := "Total"]
 
 inv_ch_techf_coun_f <- rbind(inv_ch_techf_coun_f,inv_ch_techf_coun_f_total)
   
# Calculating the share of foreign scientists in Swiss patents, per country of inventors

 options(scipen=999)
 
setDT(inv_ch_techf_coun_f)[,share_AT:=num_AT_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_CH:=num_CH_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_IL:=num_IL_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_DK:=num_DK_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_BE:=num_BE_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_FI:=num_FI_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_CA:=num_CA_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_US:=num_US_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_SE:=num_SE_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_IT:=num_IT_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_KR:=num_KR_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_GB:=num_GB_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_DE:=num_DE_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_FR:=num_FR_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_JP:=num_JP_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_NO:=num_NO_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_ES:=num_ES_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_NL:=num_NL_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_IE:=num_IE_scient/num_tot_scient*100,by=list(techbroad, interval)]
setDT(inv_ch_techf_coun_f)[,share_SG:=num_SG_scient/num_tot_scient*100,by=list(techbroad, interval)]


inv_ch_techf_coun_f <- subset(inv_ch_techf_coun_f, select = c("techbroad", "interval", "share_AT", "share_CH", "share_IL", "share_DK", "share_BE", "share_FI", "share_CA", "share_US", "share_SE", "share_IT", "share_KR", "share_GB", "share_DE", "share_FR", "share_JP", "share_NO", "share_ES", "share_NL", "share_IE", "share_SG"))

inv_ch_techf_coun_f <-melt(setDT(inv_ch_techf_coun_f), id.vars = c("techbroad", "interval"), measure.vars = c("share_AT", "share_CH", "share_IL", "share_DK", "share_BE", "share_FI", "share_CA", "share_US", "share_SE", "share_IT", "share_KR", "share_GB", "share_DE", "share_FR", "share_JP", "share_NO", "share_ES", "share_NL", "share_IE", "share_SG"))

inv_ch_techf_coun_f$variable <- inv_ch_techf_coun_f$variable %>%
  str_replace("share_", "")

colnames(inv_ch_techf_coun_f) <- c("techbroad", "interval", "location", "share")

# Creating a column with full ctry names
inv_ch_techf_coun_f$'location_name'<-countrycode(sourcevar = inv_ch_techf_coun_f$'location', "iso2c", "country.name")


# Saving data for network graph as RDS in Scicore
saveRDS(object=inv_ch_techf_coun_f, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_ch_networkdata.rds")




















################################################################################
#*******FORMER CODE******
################################################################################


################################################################################
# Preparing network data
################################################################################

# # Inserting data
#   inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")
# 
# # Filter out patents where (at least one) legal owner comes from CH
#   inv_data_ch <-  inv_data %>%
#                 filter(str_detect(ctry_leg_owner, "CH"))
# 
# # Show countries of inventors for each Swiss owned patent in "long format"
#   s <- strsplit(inv_data_ch$ctry_inventor, split = "_")
#   inv_data_ch_split <- data.frame(patent_id = rep(inv_data_ch$patent_id, sapply(s, length)), ctry_inventor = unlist(s))
# 
# # Adding tech fields to long format "long format"
#   inv_ch_techf <- subset(inv_data_ch, select = c("patent_id", "tech_field"))
#   network_coll_data_ch <- merge(inv_data_ch_split, inv_ch_techf)
#   
# # Calculating the number of foreign scientists in Swiss patents, per technology
#   
#   inv_ch_techf_coun <- subset(network_coll_data_ch, select = c("ctry_inventor", "tech_field"))
#   
#   inv_ch_techf_coun_f <- inv_ch_techf_coun %>% 
#     group_by(ctry_inventor, tech_field) %>%
#     summarise(number = n())
# 
# # Calculating and leaving only the share of foreign scientists in Swiss patents, per technology
#   
#   setDT(inv_ch_techf_coun_f)[,totnumber:=sum(number),by=list(tech_field)]
#   setDT(inv_ch_techf_coun_f)[,share_foreign:=number/totnumber,by=list(ctry_inventor, tech_field)]
#   options(scipen=999)
#   inv_ch_techf_coun_f$share_foreign <- inv_ch_techf_coun_f$share_foreign*100  
#   
#   inv_ch_techf_coun_f <- subset(inv_ch_techf_coun_f, select = c("ctry_inventor", "tech_field", "share_foreign"))
#   
# # Inserting labels for technology fields
#   techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/02_section_I_data/oecd_tech_field.RDS")
#   colnames(techlab) <- c("tech_field", "tech_name")
#   
#   inv_ch_techf_coun_final<-merge(inv_ch_techf_coun_f, techlab)
#   
#   
# # Saving data for network graph as RDS in Scicore
#   saveRDS(object=inv_ch_techf_coun_final, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_coll_ch_networkdata.rds")
#   