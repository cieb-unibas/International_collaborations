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
library(dotwhisker)
library(broom)
rm(list = ls())

# datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/collab_reg_data.rds")
# 
# colnames(datareg)
# 
# # LEAVE ONLY THOSE PATENTS WHERE THERE IS AT LEAST 1 INVENTOR FROM OWNER COUNTRY
# datareg <- datareg %>%
#   dplyr::mutate(inv_foreign = str_detect(ctry_inventor, datareg$ctry_leg_owner))
# 
# datareg <- datareg %>%
#   filter(inv_foreign ==TRUE)
# 
# # Variable capturing number of domestic scientist involved
# datareg$num_dom_scient <- str_count(datareg$ctry_inventor, datareg$ctry_leg_owner)
# 
# # Variable capturing number of scientist involved
# datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
# datareg$num_tot_scient <- datareg$num_tot_scient+1
# 
# # Calculate "foreign" scientists
# datareg$num_for_scient <- datareg$num_tot_scient-datareg$num_dom_scient
# 
# # Create "foreign scientists" dummy
# datareg$foreign <- ifelse(datareg$num_for_scient>0,1,0)
# 
# # Saving data for regression
# saveRDS(object=datareg, file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")

datareg <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg.rds")


# A. Results for each broad tech field
######################################
colnames(datareg)


datareg$claims_log <- log(datareg$claims+1)
datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)

dataregNoNA <- na.exclude(datareg)

# Run model on subsets of data, save results as tidy df, make a model variable, and relabel predictors
  by_tech <- dataregNoNA %>% 
    group_by(techbroad) %>%                                             # group data by tech field
    do(tidy(lm(world_class_99 ~ foreign + claims_log + uni + p_year + ctry_leg_owner + ctry_leg_owner*p_year, data = .))) %>% # run model on each grp
    rename(model=techbroad) %>%                                         # make model variable
    relabel_predictors(c(foreign = "At least 1 foreign scientist",      # relabel predictors
                         claims_log = "Number of claims",
                         uni = "University participation")) 

by_tech <- by_tech %>%
 filter(term =="At least 1 foreign scientist" | term =="Number of claims" | term =="University participation")        # drop fe from subset for plotting   

dwplot(by_tech, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of patents across technological fields by various factors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
