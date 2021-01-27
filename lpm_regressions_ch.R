print("International collaborations and quality of patents PREPARING DATA FOR REGRESSIONS")
# Last modified 20.01.2021 / DF

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
library(lmtest)
library(sandwich)
library(dummies)
library(tidyr)
rm(list = ls())

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")

# LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
#datareg <- datareg %>%
  #dplyr::mutate(inv_foreign = str_detect(ctry_inventor, datareg$ctry_leg_owner))
  
#datareg <- datareg %>%
#filter(inv_foreign ==TRUE)

# LEAVE ONLY TRIADIC PATENTS

#datareg <- datareg %>%
#filter(tri_pat_fam ==1)

# LEAVE ONLY PATENTS ONE OWNER
datareg$num_owners <- str_count(datareg$ctry_leg_owner, '_')
datareg$num_owners <- datareg$num_owners+1

 # datareg_more_owners <- datareg %>%
  # filter(num_owners>1)
 
 datareg_one_owner <- datareg %>%
   filter(num_owners==1)
 
 # SETTING LEGAL OWNER COUNTRY TO EQUAL COUNTRY OF THE FIRST APPLICANT
 # datareg_more_owners$inv_main <- substr(datareg_more_owners$ctry_inventor, start = 1, stop = 2)
 #  
 # datareg_more_owners$ctry_leg_owner <- datareg_more_owners$inv_main #CR: why overwriting country of legal owner by country of first inventor?
 # 
 # datareg_more_owners=subset(datareg_more_owners,select = -c(inv_main))
 # datareg_more_owners=subset(datareg_more_owners,select = -c(num_owners))
 
 datareg_one_owner=subset(datareg_one_owner,select = -c(num_owners))
 
 
# COMBINE THE TWO SAMPLES (ONE AND MORE THAN ONE OWNERS)
# datareg <- rbind(datareg_one_owner,datareg_more_owners)

# FOCUS ONLY ON 1 ASSIGNEE
datareg <- rbind(datareg_one_owner)
 
# CREATE VARIABLE of inventors d_inv (only domestic); df_inv (domestic and foreign) or f_inv (only foreign)
datareg <- mutate(datareg, ctry_inventor_clean = gsub("_", " ", ctry_inventor)) 
inv_col <- data.frame(ctry_inventor_clean = sapply(lapply(strsplit(datareg$ctry_inventor_clean, " "), unique), paste, collapse = " "))
datareg <- dplyr::select(datareg, -ctry_inventor_clean)
datareg <- cbind(datareg, inv_col)

datareg <- datareg[is.na(datareg$ctry_inventor_clean) != T & nchar(datareg$ctry_inventor_clean) > 0, ]
datareg <- mutate(datareg, d_inv  = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) == 2, 1, 0),
                           df_inv = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) > 2, 1, 0),
                           f_inv  = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) != T, 1, 0))

# Variable capturing number of scientist involved
  datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
  datareg$num_tot_scient <- datareg$num_tot_scient+1

# Variable capturing number of domestic scientist involved
  datareg$num_dom_scient <- str_count(datareg$ctry_inventor, datareg$ctry_leg_owner) 
  
# Calculate "foreign" scientists
  datareg$num_for_scient <- datareg$num_tot_scient-datareg$num_dom_scient

# Create "foreign scientists" dummy
  datareg$foreign <- ifelse(datareg$num_for_scient>0,1,0)

# Saving data for regression
  saveRDS(object=datareg, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
  datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
  datareg$claims_log <- log(datareg$claims+1)
  datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
  # datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)
  dataregNoNA <- na.exclude(datareg)

# Create dummies for foreign inventor countries  
dataregNoNA <- separate_rows(dataregNoNA, ctry_inventor_clean)  
dataregNoNA <- mutate(dataregNoNA, ctry_inventor_clean = ifelse(ctry_inventor_clean == ctry_leg_owner, "", ctry_inventor_clean))

dummies <- dummy(dataregNoNA$ctry_inventor_clean)
colnames(dummies) <- substr(colnames(dummies), nchar(colnames(dummies))-1, nchar(colnames(dummies)))
dummies <- dummies[, 2:ncol(dummies)]
dummy_names <- colnames(dummies)
dummies <- cbind(dataregNoNA[, "patent_id"], dummies)
dummies <- setDT(dummies)[, lapply(.SD, sum), by = patent_id]

dataregNoNA <- distinct(dataregNoNA, patent_id, .keep_all = T)
dataregNoNA <- left_join(dataregNoNA, dummies, by = "patent_id")
  
# Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogenous sub-sample is meaningful 
dataregNoNA <- filter(dataregNoNA, ctry_leg_owner %in% c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
  
######################################
# A. Results for each broad tech field
######################################
right_var <- c("df_inv", "f_inv", "num_tot_scient_log", "claims_log", "originality", "p_year", "tech_name", "tech_name:p_year", "ctry_leg_owner", "ctry_leg_owner:p_year", paste0(dummy_names))
m_1 <- as.formula(paste("world_class_90", paste(paste(c(right_var), collapse = "+")), sep=" ~ "))

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
    by_tech <- dataregNoNA %>% filter(p_year %in% seq(1990, 2010)) %>%  
      group_by(techbroad) %>% # group data by tech field
      do(lm(m_1, data = .) %>%
                coeftest(vcov. = vcovHC, type = "HC0") %>% tidy()) %>% # run model on each grp
      rename(model=techbroad) %>%                                         # make model variable
      relabel_predictors(c(d_inv  = "domestic",      # relabel predictors
                           df_inv = "domestic and foreign",
                           f_inv  = "foreign",
                           num_tot_scient_log = "Size of the team",
                           claims_log = "Number of claims",
                           uni = "University participation")) 

# drop fe from subset for plotting    
  by_tech <- by_tech %>%
   filter(term %in% c("domestic", "domestic and foreign", "foreign", "Size of the team", "Number of claims", "University participation"))        
  
  dwplot(by_tech, 
         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    ggtitle("Quality of patents across technological fields by various factors") +
    theme(plot.title = element_text(face="bold"),
          legend.position = "bottom",
          legend.justification = c(0, 0),
          legend.background = element_rect(colour="grey80"),
          legend.title = element_blank()) 

######################################
# B. Results for certain patent owners
######################################

dataregNoNA_patown <- dataregNoNA %>% 
  filter(ctry_leg_owner =="CN" | ctry_leg_owner =="US" | ctry_leg_owner =="DE" | ctry_leg_owner =="CH" | ctry_leg_owner =="JP" | ctry_leg_owner =="AU" | ctry_leg_owner =="GB" | ctry_leg_owner =="FR" | ctry_leg_owner =="ES" | ctry_leg_owner =="IT")

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
by_owner <- dataregNoNA_patown %>% 
  group_by(ctry_leg_owner) %>%                                             # group data by tech field
  do(tidy(lm(world_class_90 ~ foreign + num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year, data = .))) %>% # run model on each grp
  rename(model=ctry_leg_owner) %>%                                         # make model variable
  relabel_predictors(c(foreign = "At least 1 foreign scientist",      # relabel predictors
                       num_tot_scient_log = "Size of the team",
                       claims_log = "Number of claims",
                       uni = "University participation"))



by_owner <- by_owner %>%
  filter(term =="At least 1 foreign scientist" | term =="Size of the team" | term =="Number of claims" | term =="University participation")        # drop fe from subset for plotting   

dwplot(by_owner, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of patents across patent owner countries") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

######################################
# C. Results for CH as a patent owner
######################################
colnames(datareg)

dataregNoNA_patownCH <- dataregNoNA %>% 
  filter(ctry_leg_owner =="CH")

# Create "foreign scientists" in some countries dummy
dataregNoNA_patownCH$foreignUS <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"US"))
dataregNoNA_patownCH$foreignGB <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"GB"))
dataregNoNA_patownCH$foreignJP <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"JP"))
dataregNoNA_patownCH$foreignDE <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"DE"))
dataregNoNA_patownCH$foreignFR <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"FR"))
dataregNoNA_patownCH$foreignIT <- as.integer(str_detect(dataregNoNA_patownCH$ctry_inventor,"IT"))


# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
by_techCH <- dataregNoNA_patownCH %>% 
  group_by(techbroad) %>%                                             # group data by tech field
  do(tidy(lm(world_class_90 ~ foreignUS + foreignGB + foreignJP + foreignDE + foreignFR + foreignIT + num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year
                , data = .))) %>% # run model on each grp
  rename(model=techbroad) %>%                                         
  relabel_predictors(c(foreignUS = "At least 1 scientist US",
                       foreignGB = "At least 1 scientist GB",
                       foreignJP = "At least 1 scientist JP",
                       foreignDE = "At least 1 scientist DE",
                       foreignFR = "At least 1 scientist FR",
                       foreignIT = "At least 1 scientist IT",
                       num_tot_scient_log = "Size of the team",
                       claims_log = "Number of claims",
                       uni = "University participation"))


by_techCH <- by_techCH %>%
  filter(term =="At least 1 scientist US" | term =="At least 1 scientist GB" | term =="At least 1 scientist JP" | term =="At least 1 scientist DE" | term =="At least 1 scientist FR" | term =="At least 1 scientist FR")        # drop fe from subset for plotting   

dwplot(by_techCH, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of Swiss patents") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
