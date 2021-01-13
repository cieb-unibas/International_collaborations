print("International collaborations and quality of patents PREPARING DATA FOR REGRESSIONS")
# Last modified 13.01.2021 / DF

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
rm(list = ls())

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")

colnames(datareg)

# LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
datareg <- datareg %>%
  dplyr::mutate(inv_foreign = str_detect(ctry_inventor, datareg$ctry_leg_owner))

datareg <- datareg %>%
  filter(inv_foreign ==TRUE)

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
saveRDS(object=datareg, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")

datareg$claims_log <- log(datareg$claims+1)
datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)

dataregNoNA <- na.exclude(datareg)


######################################
# A. Results for each broad tech field
######################################

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
  by_tech <- dataregNoNA %>% 
    group_by(techbroad) %>%                                             # group data by tech field
    do(tidy(lm(world_class_90 ~ foreign + num_tot_scient_log + claims_log + uni + p_year + ctry_leg_owner + ctry_leg_owner:p_year, data = .))) %>% # run model on each grp
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
# B. Results for certain patent owners
######################################

dataregNoNA_patown <- dataregNoNA %>% 
  filter(ctry_leg_owner =="CN" | ctry_leg_owner =="US" | ctry_leg_owner =="DE" | ctry_leg_owner =="CH" | ctry_leg_owner =="JP" | ctry_leg_owner =="AU" | ctry_leg_owner =="GB" | ctry_leg_owner =="FR" | ctry_leg_owner =="ES" | ctry_leg_owner =="IT")

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
by_owner <- dataregNoNA_patown %>% 
  group_by(ctry_leg_owner) %>%                                             # group data by tech field
  do(tidy(lm(world_class_90 ~ foreign + num_dom_scient_log + claims_log + uni + tri_pat_fam + originality + p_year + tech_name + tech_name*p_year, data = .))) %>% # run model on each grp
  rename(model=ctry_leg_owner) %>%                                         # make model variable
  relabel_predictors(c(foreign = "At least 1 foreign scientist",      # relabel predictors
                       num_dom_scient_log = "Size of the team",
                       claims_log = "Number of claims",
                       uni = "University participation",
                       tri_pat_fam="Triadic"))


by_owner <- by_owner %>%
  filter(term =="At least 1 foreign scientist" | term =="Size of the team" | term =="Number of claims" | term =="University participation" | term =="Triadic")        # drop fe from subset for plotting   

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
  do(tidy(lm(world_class_90 ~ foreignUS + foreignGB + foreignJP + foreignDE + foreignFR + foreignIT + num_tot_scient_log + claims_log + uni + tri_pat_fam + originality + p_year + tech_name + tech_name:p_year
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
                       uni = "University participation",
                       tri_pat_fam="Triadic"))


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
