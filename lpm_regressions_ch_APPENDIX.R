print("International collaborations and quality of patents PREPARING DATA FOR REGRESSIONS")
# Last modified 19.02.2021 / CR
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
library(dummies)
library(tidyr)
library(lmtest)
library(sandwich)
library(fixest)
library(stargazer)
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

datareg <- datareg_one_owner

# CREATE VARIABLE of inventors d_inv (only domestic); df_inv (domestic and foreign) or f_inv (only foreign)

datareg <- mutate(datareg, ctry_inventor_clean = gsub("_", " ", ctry_inventor)) 
inv_col <- data.frame(ctry_inventor_clean = sapply(lapply(strsplit(datareg$ctry_inventor_clean, " "), unique), paste, collapse = " "))
datareg <- dplyr::select(datareg, -ctry_inventor_clean)
datareg <- cbind(datareg, inv_col)

# datareg <- datareg[is.na(datareg$ctry_inventor_clean) != T & nchar(datareg$ctry_inventor_clean) > 0, ]
# datareg <- mutate(datareg, d_inv  = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) == 2, 1, 0),
# df_inv = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) > 2, 1, 0),
# f_inv  = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) != T, 1, 0))

datareg <- mutate(datareg, d_inv  = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) == 2, 1, 0),
                  df_inv = ifelse(str_detect(ctry_inventor_clean, ctry_leg_owner) & nchar(ctry_inventor_clean) > 2 | str_detect(ctry_inventor_clean, ctry_leg_owner) != T, 1, 0))

# Variable capturing number of scientist involved
datareg$num_tot_scient <- str_count(datareg$ctry_inventor, '_')
datareg$num_tot_scient <- datareg$num_tot_scient+1

# Variable capturing number of domestic scientist involved
datareg$num_dom_scient <- str_count(datareg$ctry_inventor, datareg$ctry_leg_owner)

# Calculate "foreign" scientists
datareg$num_for_scient <- datareg$num_tot_scient-datareg$num_dom_scient

# Create "foreign scientists" dummy
# datareg$foreign <- ifelse(datareg$num_for_scient>0,1,0)

datareg$claims_log <- log(datareg$claims+1)
datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)
datareg$num_for_scient_log <- log(datareg$num_for_scient+1)
dataregNoNA <- na.exclude(datareg)

# Create dummies for foreign inventor countries  
dataregNoNA <- separate_rows(dataregNoNA, ctry_inventor_clean)  

## Set domestic country to NA and keep only one foreign country -> dummy only for foreign countries 
dataregNoNA <- distinct(dataregNoNA, p_key, ctry_inventor_clean, .keep_all = T) %>% 
  mutate(ctry_inventor_clean = ifelse(ctry_inventor_clean == ctry_leg_owner, "", ctry_inventor_clean)) 
dummies <- dummy(dataregNoNA$ctry_inventor_clean)
colnames(dummies) <- substr(colnames(dummies), nchar(colnames(dummies))-1, nchar(colnames(dummies)))
dummies <- dummies[, 2:ncol(dummies)]
dummy_names <- colnames(dummies)
dummies <- cbind(dataregNoNA[, "p_key"], dummies)
dummies <- setDT(dummies)[, lapply(.SD, sum), by = p_key]
dataregNoNA <- distinct(dataregNoNA, p_key, .keep_all = T)
dataregNoNA <- left_join(dataregNoNA, dummies, by = "p_key")

## Create time periods
dataregNoNA <- mutate(dataregNoNA, time = case_when(p_year %in% seq(1990, 1994, 1) ~ "1990_1994",
                                                    p_year %in% seq(1995, 1999, 1) ~ "1995_1999",
                                                    p_year %in% seq(2000, 2004, 1) ~ "2000_2004",
                                                    p_year %in% seq(2005, 2009, 1) ~ "2005_2009",
                                                    p_year %in% seq(2010, 2015, 1) ~ "2010_2015"))
## Create country dummy for rest
dataregNoNA <- mutate(dataregNoNA, REST = ifelse(AT + CH + IL + DK + BE + FI + CA + US + SE + IT + KR + GB + DE + FR + JP + NO + ES + NL + IE + SG + CN == 0 & df_inv == 1, 1, 0))
# Saving data for regression
dataregNoNA %>% saveRDS(file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
# Add some new broad technology field
# setDT(dataregNoNA)[tech_field %in% c(4, 6, 7, 8), techbroad := "IT Tech"]
# setDT(dataregNoNA)[tech_field == 16, techbroad := "Pharmaceuticals"]
# Use only subset of industrialized countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
# dataregNoNA <- filter(dataregNoNA, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  

################
# Create model #
################

base::set.seed(27)
model_estim <- function(t_field, years, data, model_form, model_name = "no_name", cluster_level = "ctry_leg_owner"){
  df <- data.frame(data)
  df <- filter(df, p_year %in% years & techbroad %in% t_field) 
  model_obj <- femlm(model_form, data = df, family = "gaussian", cluster = cluster_level)
  model_obj <- summary(model_obj, se = "cluster")
  coef <- data.frame(model_obj$coeftable)
  coef$term <- row.names(coef)
  conf <- data.frame(confint(model_obj, level = 0.95, cluster = cluster_level, se = "cluster"))
  conf$term <- row.names(conf)
  result   <- merge(coef, conf, by = "term")
  colnames(result) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  result$model     <- model_name
  result <- as_tibble(result)
  return(result)
}


#Testing the construction of dummies
colnames(dataregNoNA)
test_sample<-subset(dataregNoNA,select =c("p_key","p_year","ctry_inventor","ctry_leg_owner","AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))

############################
# Summary stat table MAIN sample#
############################
dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
colnames(dataregNoNA)
datasumm <- subset(dataregNoNA,select =c("world_class_90","num_tot_scient","claims","originality","df_inv","d_inv"))
stargazer(as.data.frame(datasumm[c("world_class_90","num_tot_scient","claims","originality","df_inv","d_inv")]),digits=1,title="Summary  Statistics, main sample", type = "latex", out="/scicore/home/weder/fildra00/Innovation/appendix_int_collab/summtable",
          covariate.labels=c("% top patents (90th %tile)","No. of inventors","No. of claims","Originality","% with foreign inventor","% only domestic inventors"),
                             summary.stat = c("n", "mean", "sd", "min", "max"))

############################
# A. DEP VAR:world_class_99#
############################
dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
left_var  <- c("world_class_99")
right_var <- c("claims_log", "originality", "num_tot_scient_log",
               #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST")))
fe        <- c("p_year + tech_name + ctry_leg_owner + ctry_leg_owner^tech_name + ctry_leg_owner^p_year")
m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, !(ctry_leg_owner %in% c("US"))), model_form = m_1, model_name = "Overall")

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))))        

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(face="bold"),
        # legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

############################
# B. DEP VAR:world_class_75#
############################
dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
left_var  <- c("world_class_75")
right_var <- c("claims_log", "originality", "num_tot_scient_log",
               #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST")))
fe        <- c("p_year + tech_name + ctry_leg_owner + ctry_leg_owner^tech_name + ctry_leg_owner^p_year")
m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, !(ctry_leg_owner %in% c("US"))), model_form = m_1, model_name = "Overall")

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))))        

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(face="bold"),
        # legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

############################
# C. LEAVE CH PATENTS#
############################

base::set.seed(27)
model_estim <- function(t_field, years, data, model_form, model_name = "no_name", cluster_level = "tech_name"){
  df <- data.frame(data)
  df <- filter(df, p_year %in% years & techbroad %in% t_field) 
  model_obj <- femlm(model_form, data = df, family = "gaussian", cluster = cluster_level)
  model_obj <- summary(model_obj, se = "cluster")
  coef <- data.frame(model_obj$coeftable)
  coef$term <- row.names(coef)
  conf <- data.frame(confint(model_obj, level = 0.95, cluster = cluster_level, se = "cluster"))
  conf$term <- row.names(conf)
  result   <- merge(coef, conf, by = "term")
  colnames(result) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  result$model     <- model_name
  result <- as_tibble(result)
  return(result)
}


dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
left_var  <- c("world_class_90")
right_var <- c("claims_log", "originality", "num_tot_scient_log",
               #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "REST")))
fe        <- c("p_year + tech_name")
m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, (ctry_leg_owner %in% c("CH"))), model_form = m_1, model_name = "Overall")

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign",
    # "Size of the team", "Number of claims", "University participation",
    paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "REST"))))

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(face="bold"),
        # legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

############################
# D. DEP VAR: CLAIMS       #
############################
dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
left_var  <- c("claims_log")
right_var <- c("originality", "num_tot_scient_log",
               #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST")))
fe        <- c("p_year + tech_name + ctry_leg_owner + ctry_leg_owner^tech_name + ctry_leg_owner^p_year")
m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, !(ctry_leg_owner %in% c("US"))), model_form = m_1, model_name = "Overall")

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))))        

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(face="bold"),
        # legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

############################
# E. DEP VAR: ORIGINALITY  #
############################
# dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
# dataregNoNA$originality_log <- log(dataregNoNA$originality+1)
# 
# left_var  <- c("originality_log")
# right_var <- c("claims_log", "num_tot_scient_log",
#                #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
#                paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST")))
# fe        <- c("p_year + tech_name + ctry_leg_owner + ctry_leg_owner^tech_name + ctry_leg_owner^p_year")
# m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
# by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, !(ctry_leg_owner %in% c("US"))), model_form = m_1, model_name = "Overall")
# 
# by_ctry_plot <- by_ctry %>%
#   filter(term %in% c(
#     # "domestic", "domestic and foreign", "foreign", 
#     # "Size of the team", "Number of claims", "University participation", 
#     paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))))        
# 
# dwplot(by_ctry_plot,
#        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
#   theme_bw() + xlab("Coefficient Estimate") + ylab("") +
#   ggtitle("") +
#   theme(plot.title = element_text(face="bold"),
#         # legend.position = "bottom",
#         legend.justification = c(0, 0),
#         legend.background = element_rect(colour="grey80"),
#         legend.title = element_blank()) 

############################
# F. Main sample with triadic#
############################
dataregNoNA<-mutate(dataregNoNA, p_year=as.factor(p_year))
dataregNoNA<-dataregNoNA %>%
  filter(tri_pat_fam==1)
left_var  <- c("world_class_90")
right_var <- c("claims_log", "originality", "num_tot_scient_log",
               #"f_inv", paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST")))
fe        <- c("p_year + tech_name + ctry_leg_owner + ctry_leg_owner^tech_name + ctry_leg_owner^p_year")
m_1 <- as.formula(paste(left_var, paste(paste(c(right_var), collapse = "+"), "|", fe), sep=" ~ "))
by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = filter(dataregNoNA, !(ctry_leg_owner %in% c("US"))), model_form = m_1, model_name = "Overall")

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH", "REST"))))        

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(face="bold"),
        # legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
