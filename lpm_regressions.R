print("International collaborations and quality of patents PREPARING DATA FOR REGRESSIONS")
# Last modified 01.02.2021 / DF

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
library(dotwhisker)
library(broom)
require(foreign)
rm(list = ls())

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")

# LEAVE ONLY PATENTS WITH WITH ONE LEGAL OWNER
datareg$num_owners <- str_count(datareg$ctry_leg_owner, '_')
datareg$num_owners <- datareg$num_owners+1

#datareg_more_owners <- datareg %>%
#filter(num_owners>1)
 
 datareg <- datareg %>%
   filter(num_owners==1)
 
 # RESTRICT THE TIME SPAN TO 1990-2015
 datareg <- datareg %>%
   filter(p_year>=1990 & p_year<=2015)
 
 # LEAVE ONLY USPTO PATENTS
 #datareg <- datareg %>%
 #filter(str_detect(patent_id, "US"))
 
 # LEAVE ONLY TRIADIC PATENTS
 #datareg <- datareg %>%
 #filter(tri_pat_fam ==1)
 
 # LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
 # datareg <- datareg %>%
 #dplyr::mutate(inv_foreign = str_detect(ctry_inventor, datareg$ctry_leg_owner))
 
 #datareg <- datareg %>%
 # filter(inv_foreign ==TRUE)
 
 
 ############################################################################## 
 
 # LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
 #datareg <- datareg %>%
 #dplyr::mutate(inv_foreign = str_detect(ctry_inventor, datareg$ctry_leg_owner))
 
 #datareg <- datareg %>%
 #filter(inv_foreign ==TRUE)
 
 # LEAVE ONLY TRIADIC PATENTS
 
 #datareg <- datareg %>%
 #filter(tri_pat_fam ==1)
 
 # SETTING LEGAL OWNER COUNTRY TO EQUAL COUNTRY OF THE FIRST APPLICANT

 # datareg_more_owners$inv_main <- substr(datareg_more_owners$ctry_inventor, start = 1, stop = 2)
  
 # datareg_more_owners$ctry_leg_owner <- datareg_more_owners$inv_main #CR: why overwriting country of legal owner by country of first inventor?
 
 #datareg_more_owners=subset(datareg_more_owners,select = -c(inv_main))
 #datareg_more_owners=subset(datareg_more_owners,select = -c(num_owners))
 
 #datareg_one_owner=subset(datareg_one_owner,select = -c(num_owners))
 
 
 # COMBINE THE TWO SAMPLES (ONE AND MORE THAN ONE OWNERS)
 
 # datareg <- rbind(datareg_one_owner,datareg_more_owners)

  #CR: Problematic, since we use only first applicant. What if there are two firms located in two different countries and the inventors of a particular firm are all from the same country as the firm. 
  #In this case, we would count some as foreign, although they aren't...
 ############################################################################## 
 # Variable capturing number of domestic scientist involved
 datareg$num_dom_scient <- str_count(datareg$ctry_inventor, datareg$ctry_leg_owner) 
 
# Variable capturing number of scientist involved
  datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
  datareg$num_tot_scient <- datareg$num_tot_scient+1

# Calculate "foreign" scientists
  datareg$num_for_scient <- datareg$num_tot_scient-datareg$num_dom_scient

# Create "foreign scientists" dummy
  datareg$foreign <- ifelse(datareg$num_for_scient>0,1,0)

# Saving data for regression
  #saveRDS(object=datareg, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")

  #datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")
  
  datareg$claims_log <- log(datareg$claims+1)
  datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
  datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)
  
  dataregNoNA <- na.exclude(datareg)

  #export(dataregNoNA, "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dataregNoNA_stata.dta")

######################################
# A.i) Results for each broad tech field
######################################

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
    by_tech <- dataregNoNA %>%
    filter(ctry_leg_owner !="US") %>%
      group_by(techbroad) %>%                                             # group data by tech field
      do(tidy(lm(world_class_75 ~ foreign + num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year + ctry_leg_owner + ctry_leg_owner:p_year, data = .))) %>% # run model on each grp
      rename(model=techbroad) %>%                                         # make model variable
      relabel_predictors(c(foreign = "At least 1 foreign scientist",      # relabel predictors
                           num_tot_scient_log = "Size of the team",
                           claims_log = "Number of claims",
                           uni = "University participation")) 
  
  by_tech <- by_tech %>%
   filter(term =="At least 1 foreign scientist" | term =="Size of the team" | term =="Number of claims" | term =="University participation")        # drop fe from subset for plotting   
  
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
  # A.ii) Results for each broad tech field (Collab with US)
  ######################################
  
  # If there is at least one US scientist
  dataregNoNA$USinv <- as.integer(str_detect(dataregNoNA$ctry_inventor,"US"))
  
  # Foreign patents with at least one US scientists
  dataregNoNA$foreignUSA <- ifelse(dataregNoNA$num_for_scient>0 & dataregNoNA$USinv==1,1,0)
  # Foreign patents without US scientists
  dataregNoNA$foreignOTHER <- ifelse(dataregNoNA$num_for_scient>0 & dataregNoNA$USinv==0,1,0)
  
  
  
  
  # Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
  by_tech <- dataregNoNA %>%
    #filter(ctry_leg_owner !="US") %>%
    group_by(techbroad) %>%                                             # group data by tech field
    do(tidy(lm(world_class_75 ~ foreignUSA + foreignOTHER + num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year + ctry_leg_owner + ctry_leg_owner:p_year, data = .))) %>% # run model on each grp
    rename(model=techbroad) %>%                                         # make model variable
    relabel_predictors(c(foreignUSA = "At least 1 foreign USA scientist",
                         foreignOTHER = "At least 1 foreign non-USA scientist", # relabel predictors
                         num_tot_scient_log = "Size of the team",
                         claims_log = "Number of claims",
                         uni = "University participation")) 
  
  by_tech <- by_tech %>%
    filter(term =="At least 1 foreign USA scientist" | term =="At least 1 foreign non-USA scientist" | term =="Size of the team" | term =="Number of claims" | term =="University participation")        # drop fe from subset for plotting   
  
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
  do(tidy(lm(world_class_75 ~ foreign + num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year, data = .))) %>% # run model on each grp
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

# Foreign patents with OTHER scientists
dataregNoNA_patownCH$foreignOTHER <- ifelse(dataregNoNA_patownCH$num_for_scient>0 & dataregNoNA_patownCH$foreignUS==0 & dataregNoNA_patownCH$foreignUS==0 & dataregNoNA_patownCH$foreignGB==0 & dataregNoNA_patownCH$foreignJP==0 & dataregNoNA_patownCH$foreignFR==0 & dataregNoNA_patownCH$foreignIT==0,1,0)


# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
by_techCH <- dataregNoNA_patownCH %>% 
  group_by(techbroad) %>%                                             # group data by tech field
  do(tidy(lm(world_class_75 ~ foreignUS + foreignGB + foreignJP + foreignDE + foreignFR + foreignIT + foreignOTHER+ num_tot_scient_log + claims_log + uni + originality + p_year + tech_name + tech_name:p_year
                , data = .))) %>% # run model on each grp
  rename(model=techbroad) %>%                                         
  relabel_predictors(c(foreignUS = "At least 1 scientist US",
                       foreignGB = "At least 1 scientist GB",
                       foreignJP = "At least 1 scientist JP",
                       foreignDE = "At least 1 scientist DE",
                       foreignFR = "At least 1 scientist FR",
                       foreignIT = "At least 1 scientist IT",
                       foreignOTHER = "At least 1 other foreign scientist",
                       num_tot_scient_log = "Size of the team",
                       claims_log = "Number of claims",
                       uni = "University participation"))


by_techCH <- by_techCH %>%
  filter(term =="At least 1 scientist US" | term =="At least 1 scientist GB" | term =="At least 1 scientist JP" | term =="At least 1 scientist DE" | term =="At least 1 scientist FR" | term =="At least 1 scientist FR" | term =="At least 1 other foreign scientist")        # drop fe from subset for plotting   

dwplot(by_techCH, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of Swiss patents") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
