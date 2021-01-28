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
library(dummies)
library(tidyr)
library(lmtest)
library(sandwich)
library(fixest)
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
  datareg$claims_log <- log(datareg$claims+1)
  datareg$num_tot_scient_log <- log(datareg$num_tot_scient+1)
  # datareg$num_dom_scient_log <- log(datareg$num_dom_scient+1)
  dataregNoNA <- na.exclude(datareg)

# Create dummies for foreign inventor countries  
dataregNoNA <- separate_rows(dataregNoNA, ctry_inventor_clean)  
## Set domestic country to NA and keep only one foreign country -> dummy only for foreign countries 
dataregNoNA <- distinct(dataregNoNA, patent_id, ctry_inventor_clean, .keep_all = T) %>% 
               mutate(ctry_inventor_clean = ifelse(ctry_inventor_clean == ctry_leg_owner, "", ctry_inventor_clean)) 

dummies <- dummy(dataregNoNA$ctry_inventor_clean)
colnames(dummies) <- substr(colnames(dummies), nchar(colnames(dummies))-1, nchar(colnames(dummies)))
dummies <- dummies[, 2:ncol(dummies)]
dummy_names <- colnames(dummies)
dummies <- cbind(dataregNoNA[, "patent_id"], dummies)
dummies <- setDT(dummies)[, lapply(.SD, sum), by = patent_id]

dataregNoNA <- distinct(dataregNoNA, patent_id, .keep_all = T)
dataregNoNA <- left_join(dataregNoNA, dummies, by = "patent_id")
  
# Saving data for regression
dataregNoNA %>% saveRDS(file = "/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")
dataregNoNA <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/datareg_ch.rds")

# Use only subset of industrial countries as patent owners -> for policy advise to Switzerland I guess only such a rather homogeneous sub-sample is meaningful 
dataregNoNA <- filter(dataregNoNA, ctry_leg_owner %in% c("AT", "CH", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG"))  
dataregNoNA <- dplyr::select(dataregNoNA, world_class_90, techbroad, tech_field, df_inv, f_inv, num_tot_scient_log, claims_log, originality, p_year, tech_name, ctry_leg_owner, AT, CH, IL, DK, BE, FI, CA, US, SE, IT, KR, GB, DE, FR, JP, NO, ES, NL, IE, SG)  


# Create model: I have tried it with the previous (broom/tidy) approach, however I could not create clustered standard errors (the 
#function femlm is not compatible, with lm I don't know how to get clustered standard errors at a particular level. I also tried plm leading to an 
#error. Thus, I created a estimation function delivering the same output as before so it can be used e.g. with dwplot.
base::set.seed(27)

model_estim <- function(t_field, years, data, model_form, model_name = "no_name"){
  df <- data.frame(data)
  df <- filter(df, p_year %in% years & techbroad %in% t_field) 
  model_obj <- femlm(model_form, data = df, family = "gaussian", cluster = c("ctry_leg_owner"))
  model_obj <- summary(model_obj, se = "cluster")
  coef <- data.frame(model_obj$coeftable)
  coef$term <- row.names(coef)
  conf <- data.frame(confint(model_obj, level = 0.95, cluster = c("ctry_leg_owner"), se = "cluster"))
  conf$term <- row.names(conf)
  result   <- merge(coef, conf, by = "term")
  colnames(result) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  result$model     <- model_name
  result <- as_tibble(result)
  return(result)
}



######################################
# A. Results for each broad tech field
######################################
right_var <- c("df_inv", "f_inv", "num_tot_scient_log", "claims_log", "originality", "p_year", "tech_name", "tech_name:p_year", "ctry_leg_owner:p_year")
m_1 <- as.formula(paste("world_class_90", paste(paste(c(right_var), collapse = "+")), sep=" ~ "))

by_tech <- do.call(rbind, lapply(unique(dataregNoNA$techbroad), function(x) model_estim(x, years = seq(1990, 2015), data = dataregNoNA, model_form = m_1, model_name = x)))

# drop fe from subset for plotting    
by_tech_plot <- by_tech %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    "f_inv", "df_inv", paste0("df_inv:techbroad", unique(dataregNoNA$techbroad)), paste0("f_inv:techbroad", unique(dataregNoNA$techbroad))))        

dwplot(by_tech_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of patents across technological fields by various factors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

#####################################
# B. By tech and partnering country #
#####################################
right_var <- c("df_inv", "f_inv", "num_tot_scient_log", "claims_log", "originality", "p_year", "tech_name", "tech_name:p_year", "ctry_leg_owner:p_year",
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":df_inv"))
m_1 <- as.formula(paste("world_class_90", paste(paste(c(right_var), collapse = "+")), sep=" ~ "))

by_tech_ctry <- do.call(rbind, lapply(unique(dataregNoNA$techbroad), function(x) model_estim(x, years = seq(1990, 2015), data = dataregNoNA, model_form = m_1)))

# drop fe from subset for plotting    
by_tech_ctry_plot <- by_tech_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0("f_inv:", c("CA", "US", "IT", "KR", "GB", "DE", "FR", "JP", "CN", "CH")),
    paste0("df_inv:", c("CA", "US", "IT", "KR", "GB", "DE", "FR", "JP","CN", "CH"))))        

dwplot(arrange(by_tech_ctry_plot, model),
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of patents across technological fields by various factors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

############################
# C. By partnering country #
############################
right_var <- c("df_inv", "f_inv", "num_tot_scient_log", "claims_log", "originality", "p_year", "tech_name", "tech_name:p_year", "ctry_leg_owner:p_year",
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":f_inv"),
               paste0(c("AT", "IL", "DK", "BE", "FI", "CA", "US", "SE", "IT", "KR", "GB", "DE", "FR", "JP", "NO", "ES", "NL", "IE", "SG", "CN", "CH"), ":df_inv"))
m_1 <- as.formula(paste("world_class_90", paste(paste(c(right_var), collapse = "+")), sep=" ~ "))

by_ctry <- model_estim(unique(dataregNoNA$techbroad), years = seq(1990, 2015), data = dataregNoNA, model_form = m_1)

by_ctry_plot <- by_ctry %>%
  filter(term %in% c(
    # "domestic", "domestic and foreign", "foreign", 
    # "Size of the team", "Number of claims", "University participation", 
    paste0("f_inv:", c("CA", "US", "IT", "KR", "GB", "DE", "FR", "JP", "CN", "CH")),
    paste0("df_inv:", c("CA", "US", "IT", "KR", "GB", "DE", "FR", "JP","CN", "CH"))))        

dwplot(by_ctry_plot,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Quality of patents across technological fields by various factors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 
