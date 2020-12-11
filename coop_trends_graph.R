print("International collaborations and quality of patents COOPERATION TRENDS GRAPH")
# Last modified 11.12.2020 / DF

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

# Inserting investor data
  inv_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/int_collab_dat_final.rds")

# FILTER 10 patent owner countries
  own_ctry_CN <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "CN"))
  own_ctry_CN$owner_ctry <- "CN"
  
  own_ctry_US <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "US"))
  own_ctry_US$owner_ctry <- "US"
  
  own_ctry_DE <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "DE"))
  own_ctry_DE$owner_ctry <- "DE"
  
  own_ctry_CH <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "CH"))
  own_ctry_CH$owner_ctry <- "CH"
  
  own_ctry_JP <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "JP"))
  own_ctry_JP$owner_ctry <- "JP"
  
  own_ctry_AU <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "AU"))
  own_ctry_AU$owner_ctry <- "AU"
  
  own_ctry_GB <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "GB"))
  own_ctry_GB$owner_ctry <- "GB"
  
  own_ctry_FR <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "FR"))
  own_ctry_FR$owner_ctry <- "FR"
  
  own_ctry_ES <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "ES"))
  own_ctry_ES$owner_ctry <- "ES"
  
  own_ctry_IT <- inv_data %>%
    filter(str_detect(ctry_leg_owner, "IT"))
  own_ctry_IT$owner_ctry <- "IT"
  
  own_10ctry_sample <- rbind(own_ctry_CN, own_ctry_US, own_ctry_DE,own_ctry_CH,own_ctry_JP ,own_ctry_AU, own_ctry_GB, own_ctry_FR,own_ctry_ES,own_ctry_IT)
  
  own_10ctry_sample <- subset(own_10ctry_sample, select = c("p_key", "patent_id", "owner_ctry"))

# Merging 10 country samples with the main sample to keep only those 10

  inv_data_10_own_ctry<-merge(inv_data, own_10ctry_sample)
  inv_data_10_own_ctry <- subset(inv_data_10_own_ctry, select = c("p_key", "patent_id", "tech_field", "ctry_inventor", "owner_ctry"))

# Isolating years for each patent from another data set and leave only one observation per patent
  owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
  pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year"))
  colnames(pyear_data) <- c("p_key", "patent_id", "p_year")
  
  pyear_data <- pyear_data %>% 
    group_by(p_key, patent_id, p_year) %>% 
    slice(1) %>%
    ungroup()

# Merging years with main sample
  inv_data_10_own_ctry_f <- merge(inv_data_10_own_ctry, pyear_data)
  
# SAVING INVENTORS DATA WITH 10 patent owner countries
  
   saveRDS(object=inv_data_10_own_ctry_f, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_10_own_ctry_f.rds")
  
# LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
  
  inv_data_10_own_ctry_f <- inv_data_10_own_ctry_f %>% 
    group_by(p_key, patent_id, p_year,tech_field, owner_ctry) %>% 
    dplyr::mutate(inv_from_own = grepl(owner_ctry,ctry_inventor)) %>%
    ungroup()
  
  inv_data_10_own_ctry_f <- inv_data_10_own_ctry_f %>% 
      filter(inv_from_own ==TRUE)

# Counting "domestic" scientists (if their residence match owners country)   
  inv_data_10_own_ctry_f$num_dom <- str_count(inv_data_10_own_ctry_f$ctry_inventor, inv_data_10_own_ctry_f$owner_ctry)
  
# Counting "total" scientists    
  inv_data_10_own_ctry_f$num_total <- str_count(inv_data_10_own_ctry_f$ctry_inventor, '_')
  inv_data_10_own_ctry_f$num_total <- inv_data_10_own_ctry_f$num_total+1
  
# Calculate "foreign" scientists    
  inv_data_10_own_ctry_f$num_foreign <- inv_data_10_own_ctry_f$num_total-inv_data_10_own_ctry_f$num_dom
  
  inv_data_10_own_ctry_f <- subset(inv_data_10_own_ctry_f, select = c(p_key, patent_id, p_year,tech_field, ctry_inventor, owner_ctry, num_foreign, num_total))
  
  
  # SAVING FINAL INVENTORS DATA WITH 10 patent owner countries
  
  saveRDS(object=inv_data_10_own_ctry_f, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_data_10_own_ctry_f.rds")
  
  
  
  
  
  
  
  
  
  
    
  
# Show countries of inventors for each owned patent in "long format"
  # s <- strsplit(inv_data_10_own_ctry_f$ctry_inventor, split = "_")
  # inv_data_split <- data.frame(patent_id = rep(inv_data_10_own_ctry_f$patent_id, sapply(s, length)), ctry_inventor = unlist(s))

  #inv_data_temp <- subset(inv_data_10_own_ctry_f, select = c("p_key", "patent_id", "p_year", "tech_field", "owner_ctry"))

  
  # temp_data_tocount <- merge(inv_data_temp,inv_data_split)
  
  #inv_data_10_own_ctry_fin <- inv_data_10_own_ctry_f %>% 
  #  group_by(patent_id, p_year, tech_field, owner_ctry) %>% 
  #  mutate(num_foreign_inv = sum(ctry_inventor != owner_ctry)) %>%
  #  ungroup()
  
  
  
  
  
  
  
  
  
  
  
  




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
  
  own_ctry_JP <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "JP"))
  own_ctry_JP$owner_ctry <- "JP"
  
  own_ctry_AU <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "AU"))
  own_ctry_AU$owner_ctry <- "AU"
  
  own_ctry_GB <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "GB"))
  own_ctry_GB$owner_ctry <- "GB"
  
  own_ctry_FR <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "FR"))
  own_ctry_FR$owner_ctry <- "FR"
  
  own_ctry_ES <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "ES"))
  own_ctry_ES$owner_ctry <- "ES"
  
  own_ctry_IT <- graph_coll_data %>%
    filter(str_detect(ctry_leg_owner, "IT"))
  own_ctry_IT$owner_ctry <- "IT"
  
  own_10ctry_sample <- rbind(own_ctry_CN, own_ctry_US, own_ctry_DE,own_ctry_CH,own_ctry_JP ,own_ctry_AU, own_ctry_GB, own_ctry_FR,own_ctry_ES,own_ctry_IT)
  
  own_10ctry_sample <- subset(own_10ctry_sample, select = c("p_key", "patent_id", "owner_ctry"))
  

# Merging 4 country samples with the main sample to keep only those 10
  
  graph_coll_data_4ctry<-merge(graph_coll_data, own_10ctry_sample)
  graph_coll_data_4ctry <- subset(graph_coll_data_4ctry, select = c("p_key", "patent_id", "tech_field", "ctry_inventor", "owner_ctry"))
  
  
# Isolating years for each patent
  owner_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/firm_reg.rds")
  pyear_data <- subset(owner_data, select = c("p_key", "pub_nbr", "p_year"))
  colnames(pyear_data) <- c("p_key", "patent_id", "p_year")
  
  
# Merging years with main sample
  graph_coll_data_4ctry_year <- merge(graph_coll_data_4ctry, pyear_data)

  # SAVING DATA WITH 10 countries
  
  saveRDS(object=graph_coll_data_4ctry_year, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/graph_coll_data_4ctry_year.rds")
  
# INSERTING INVENTORS DATA FINAL
  
  graph_coll_data_4ctry_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/graph_coll_data_4ctry_year.rds")
 
# LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
  
  graph_coll_data_4ctry_year <- graph_coll_data_4ctry_year %>% 
    group_by(p_key, patent_id, p_year,tech_field, owner_ctry) %>% 
    dplyr::mutate(inv_from_own = str_detect(owner_ctry,ctry_inventor)) %>%
    ungroup()
  
  graph_coll_data_4ctry_year <- graph_coll_data_4ctry_year %>% 
    group_by(p_key, patent_id, p_year,tech_field, owner_ctry) %>%
    filter(any(inv_from_own == TRUE))
  
  
  graph_coll_data_4ctry_year_totn <- graph_coll_data_4ctry_year %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    dplyr::mutate(totnumber_inv = n()) %>%
    ungroup()
  

# Calculating total number of scientists in patents, per technology and owner
  
  graph_coll_data_4ctry_year_totn <- graph_coll_data_4ctry_year %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    dplyr::mutate(totnumber_inv = n()) %>%
    ungroup()

# Calculating foreign number of scientists in patents, per technology and owner
  
  graph_coll_data_4ctry_year_totnforn <- graph_coll_data_4ctry_year_totn %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    mutate(totnumber_foreign_inv = sum(ctry_inventor != owner_ctry)) %>%
    ungroup()
 
# leaving only first observation for each patent (excluding multiple inventors rows actually)
  
  graph_coll_data_4ctry_year_totnforn <- graph_coll_data_4ctry_year_totnforn %>% 
    group_by(p_key, patent_id) %>% 
    slice(1) %>%
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
  forcit_data <-subset(forcit_data, select = c("p_key","pub_nbr_oecd","p_year", "world_class_90"))
  colnames(forcit_data) <- c("p_key", "patent_id","p_year", "world_class_90")
  forcit_data <-subset(forcit_data, select = c("p_key", "patent_id", "world_class_90"))
  
  saveRDS(object=forcit_data, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/forcit_data.rds")
  
################################################################################
# Merging cit data with prepared inventors data
################################################################################
  
  coll_trends_invshare <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_invshare.rds")
  forcit_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/forcit_data.rds")
  
   coll_trends_f <- merge(coll_trends_invshare,forcit_data)
  
# Calculating total number of patents, per technology, year and NOT owner
  
  coll_trends_final <- coll_trends_f %>% 
     group_by(tech_field, owner_ctry, p_year) %>%
    mutate(totnumber_pat = n()) %>%
    ungroup()
  
  # Calculating share of world class patents, per technology, year and owner
  
  coll_trends_final2 <- coll_trends_final %>% 
    group_by(tech_field, owner_ctry, p_year) %>% 
    mutate(totnumber_wcpat = sum(world_class_90 == 1)) %>%
    ungroup()
  
  
  setDT(coll_trends_final2)[,share_wc:=totnumber_wcpat/totnumber_pat,by=list(tech_field, owner_ctry, p_year)]
  options(scipen=999)
  
  coll_trends_final2$share_wc <- coll_trends_final2$share_wc*100 
  
  
  coll_trends_final2 <-subset(coll_trends_final2, select = c("p_year", "tech_field", "owner_ctry", "share_foreign", "share_wc"))
  
#Transforming into long format  
  # coll_trends_final <-melt(setDT(coll_trends_final), id.vars = c("p_year", "tech_field", "owner_ctry"), measure.vars = c("share_foreign", "tot_cit"))
  
  saveRDS(object=coll_trends_final2, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_final.rds")
  
################################################################################
 # PREP FOR GRAPH
################################################################################
  
  trends_data <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/coll_trends_final.rds")
  
# Inserting labels for technology fields
  techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/02_section_I_data/oecd_tech_field.RDS")
  colnames(techlab) <- c("tech_field", "tech_name")
  
  trends_data<-merge(trends_data, techlab)
  
#   Creating broader tech groups
  
 setDT(trends_data)[tech_field>=1 & tech_field<=8, techbroad := "Electrical engineering"]
 setDT(trends_data)[tech_field>=9 & tech_field<=13, techbroad := "Instruments"]
 setDT(trends_data)[tech_field>=14 & tech_field<=24, techbroad := "Chemistry"]
 setDT(trends_data)[tech_field>=25 & tech_field<=32, techbroad := "Mechanical engineering"]
 setDT(trends_data)[tech_field>=33 & tech_field<=35, techbroad := "Other fields"]
  
  
  #Filter data, Year is filtered from 1990 to 2015.
  trends_data_graph<-trends_data %>%
    filter(p_year >=1990 & p_year <2015)
  
  setDT(trends_data_graph)[p_year>=1990 & p_year<1995, interval := "1990-1995"]
  setDT(trends_data_graph)[p_year>=1995 & p_year<2000, interval := "1995-2000"]
  setDT(trends_data_graph)[p_year>=2000 & p_year<2005, interval := "2000-2005"]
  setDT(trends_data_graph)[p_year>=2005 & p_year<2010, interval := "2005-2010"]
  setDT(trends_data_graph)[p_year>=2010 & p_year<2015, interval := "2010-2015"]
  
  #worldclass <- trends_data_graph %>% 
    #filter(p_year ==1990 | p_year ==1995 | p_year ==2000 | p_year ==2005 | p_year ==2010)
  worldclass <- subset(trends_data_graph, select = c("owner_ctry", "techbroad", "share_wc", "interval"))
  
  worldclass<-worldclass%>%
    group_by(owner_ctry,techbroad,interval)%>%
    summarise(share_wc=mean(share_wc,na.rm=TRUE))
  
  
  trends_data_graph<-trends_data_graph%>%
    group_by(owner_ctry,techbroad,interval)%>%
    summarise(share_foreign=mean(share_foreign,na.rm=TRUE))
  
  trends_data_graph <- subset(trends_data_graph, select = c("owner_ctry", "techbroad", "share_foreign", "interval"))
  
  trends_data_graph <-merge(trends_data_graph,worldclass)        
  
  saveRDS(object=trends_data_graph, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/trends_data_graph.rds")
  