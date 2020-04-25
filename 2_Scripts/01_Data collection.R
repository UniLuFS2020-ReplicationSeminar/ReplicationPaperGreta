# loading packages #
library(tidyverse)
library(rio)
library(foreign)
library(sjmisc)
library(sjlabelled)
library(stringr)

#################
## DATA IMPORT ##
#################

## IMPORT 1/10: American National Election Study 2010 - 2012 #----
anes1012 <- import(here::here("1_Data","1_Panel Datasets","anes_specialstudies_2012egss4.dta"))

## IMPORT 2/10: American National Election Study 2012 #----
anes12 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2012.dta"))

## IMPORT 3/10: American National Election Study 2016 #----
anes16 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2016.dta"))

## IMPORT 4/10: British Election Study #----
bes <- read.spss("1_Data/1_Panel Datasets/BES2017_W13_v1.6.sav", to.data.frame = TRUE, use.value.labels = FALSE)

## IMPORT 5/10: Swiss Household Panel #----
shp <- import(here::here("1_Data","1_Panel Datasets","shp09_p_user.dta"))

## IMPORT 6/10: Longitudinal Internet Studies in the Social Sciences #----
#variables: liss.b=background, liss.pe=personality, liss.po=political, liss.s=social#
liss.b <- import(here::here("1_Data","1_Panel Datasets","avars_200805_EN_2.0p.dta"))
liss.pe <- import(here::here("1_Data","1_Panel Datasets","cp08a_1p_EN.dta"))
liss.po <- import(here::here("1_Data","1_Panel Datasets","cv09b_2.1p_EN.dta")) 
liss.s <- import(here::here("1_Data","1_Panel Datasets","cs08a_2p_EN.dta"))

#Merging the LISS datasets of different variables
liss <- merge(liss.b, liss.pe, by="nomem_encr")
liss <- select(liss, -nohouse_encr.y, -nohouse_encr.x)
liss <- merge(liss, liss.po, by="nomem_encr")
liss <- merge(liss, liss.s, by="nomem_encr")
liss <- select(liss, -nohouse_encr.y, -nohouse_encr.x)

## IMPORT 7/10: Latin American Public Opinion Project #----
data_Suriname <- import(here::here("1_Data","1_Panel Datasets","132981347Suriname_LAPOP_AmericasBarometer 2010 data set  original v1.dta"))
data_Ecuador <- import(here::here("1_Data","1_Panel Datasets","152597287Ecuador_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))
data_Guatemala <- import(here::here("1_Data","1_Panel Datasets","305797627Guatemala_LAPOP_AmericasBarometer 2010 data set  approved V3.dta"))
data_ElSalvador <- import(here::here("1_Data","1_Panel Datasets","316969358El Salvador_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))
data_DominicanRep <- import(here::here("1_Data","1_Panel Datasets","381781810Dominican Rep_LAPOP_AmericasBarometer 2010 data set approved v3.dta"))
data_Trinidad <- import(here::here("1_Data","1_Panel Datasets","464873967Trinidad_LAPOP_AmericasBarometer 2010 data set  approved V3.dta"))
data_Peru <- import(here::here("1_Data","1_Panel Datasets","856894271Peru_LAPOP_AmericasBarometer 2010 data set  approved V3.dta"))
data_Argentina <- import(here::here("1_Data","1_Panel Datasets","924077670Argentina_LAPOP_AmericasBarometer 2010 data set approved v3.dta"))
data_Paraguay <- import(here::here("1_Data","1_Panel Datasets","988869591Paraguay_LAPOP_AmericasBarometer 2010 data set approved v4.dta"))
data_Nicaragua <- import(here::here("1_Data","1_Panel Datasets","1020120195Nicaragua_LAPOP_AmericasBarometer 2010 data set revised V3 approved.dta"))
data_CostaRica <- import(here::here("1_Data","1_Panel Datasets","1053578213Costa Rica_LAPOP_AmericasBarometer 2010 data set revised V4.dta"))
data_Uruguay <- import(here::here("1_Data","1_Panel Datasets","1109346770Uruguay_LAPOP_AmericasBarometer 2010 data set  APPROVED V3.dta"))
data_Canada <- import(here::here("1_Data","1_Panel Datasets","1183322032Canada_LAPOP_AmericasBarometer 2010 data set  original v2.dta"))
data_Chile <- import(here::here("1_Data","1_Panel Datasets","1217681709Chile_LAPOP_AmericasBarometer 2010 data set approved v4.dta"))
data_Belice <- import(here::here("1_Data","1_Panel Datasets","1332767016Belice_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))
data_Panama <- import(here::here("1_Data","1_Panel Datasets","1390794604Panama LAPOP AmericasBarometer 2010 V3 APPROVED FEB 17.dta"))
data_Venezuela <- import(here::here("1_Data","1_Panel Datasets","1451946926Venezuela_LAPOP_AmericasBarometer 2010 data set APPROVED v3.dta"))
data_Bolivia <- import(here::here("1_Data","1_Panel Datasets","1748144313Bolivia_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))
data_Jamaica <- import(here::here("1_Data","1_Panel Datasets","1813458450Jamaica_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))
data_Colombia <- import(here::here("1_Data","1_Panel Datasets","1827093199Colombia2010_Rev1_W.dta"))
data_US <- import(here::here("1_Data","1_Panel Datasets","1954363083US_LAPOP_AmericasBarometer 2010 data set  original V1.dta"))
data_Mexico <- import(here::here("1_Data","1_Panel Datasets","2054050000Mexico_LAPOP_AmericasBarometer 2010 data set  approved V5.dta"))
data_Guyana <- import(here::here("1_Data","1_Panel Datasets","2094321522Guyana_LAPOP_AmericasBarometer 2010 data set  approved_V3.dta"))
data_Brazil <- import(here::here("1_Data","1_Panel Datasets","7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set  approved v4.dta"))
# Additional Datasets (Haiti & Honduras) that have not been imported in original analysis (Total 26) #
data_Haiti <- import(here::here("1_Data","1_Panel Datasets","1934955590Haiti_LAPOP_AmericasBarometer 2010 data set  original v1 English.dta"))
data_Honduras <- import(here::here("1_Data","1_Panel Datasets","1418722138Honduras_LAPOP_AmericasBarometer 2010 data set  approved v3.dta"))

# Preparation for Merge LAPOP (original Code from author)#
data_Paraguay$fecha <- as.numeric(data_Paraguay$fecha)

#PATH 1 - AUTHORS PATH - tbc#
#Since Answers are not coded, a lot of NAs are generated#
#data_Canada$gi1 <- as.numeric(data_Canada$gi1)
#data_US$gi1 <- as.numeric(data_US$gi1)

#PATH 2 - WITH CODING#
#Coding raw data with answers in charakter (dataset CANADA and US)#
data_Canada$gi1 <- stringr::str_to_lower(data_Canada$gi1)
data_Canada$gi1 <- ifelse(str_detect(data_Canada$gi1,pattern = "obama"),1,2)
data_Canada$gi1 <- add_labels(data_Canada$gi1, labels = c("Correct" = 1, "Incorrect" = 2))

data_US$gi1 <- stringr::str_to_lower(data_US$gi1)
data_US$gi1 <- ifelse(str_detect(data_US$gi1,pattern = "biden"),1,2)
data_US$gi1 <- add_labels(data_US$gi1, labels = c("Correct" = 1, "Incorrect" = 2))
#ATTENTION: gi1 Canada: President of USA, gi1 US: Vice President of USA -> CAN WE MERGE THIS?#

data_Brazil$ti <- as.numeric(data_Brazil$ti)
data_Brazil$intid <- as.numeric(data_Brazil$intid)

lapop <- bind_rows(list(data_Suriname, 
                        data_Ecuador,
                        data_Guatemala,
                        data_ElSalvador,
                        data_DominicanRep,
                        data_Trinidad,
                        data_Peru,
                        data_Argentina,
                        data_Paraguay,
                        data_Nicaragua,
                        data_CostaRica,
                        data_Uruguay,
                        data_Canada,
                        data_Chile,
                        data_Belice,
                        data_Panama,
                        data_Venezuela,
                        data_Bolivia,
                        data_Jamaica,
                        data_Colombia,
                        data_US,
                        data_Mexico,
                        data_Guyana,
                        data_Brazil,
                        data_Haiti,
                        data_Honduras))

## IMPORT 8/10: Swiss Election Study ##----
sels <- import(here::here("1_Data","1_Panel Datasets", "828_Selects2015_PanelRCS_Data_v1.1.dta"))

## IMPORT 9/10: New Zealand Election Study ##----
nzes <- import(here::here("1_Data","1_Panel Datasets","NZES2014GeneralReleaseApril16.sav"))

## IMPORT 10/10: Canadian Election Study ##----
ces <- import(here::here("1_Data","1_Panel Datasets","CES2015_Combined_Stata14.dta"))



#####################
## DATA PROCESSING ##
#####################

## CREATING FUNCTION TO STANDARDISE VARIABLES divided by 2 SD##
two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}

### Recode: ANES 2010-2012 ###
anes1012 <- anes1012 %>%
  # Creating NAs for missing values
  mutate_at(vars(c4_p1, c4_pppa0035, c4_f1, c4_f2, c4_pppa0206, c4_pppa0207, c4_pppa0208, c4_pppa0209, 
                 c4_pppa0210, c4_pppa0211, c4_pppa0212, c4_pppa0213, c4_pppa0005, c4_pppa0220,
                 c4_be1, c4_be2, c4_be3, c4_be4, c4_be5, c4_zh1, c4_zh2, c4_zh3, c4_zh4,
                 c4_zf1, c4_zf2, c4_zf3, c4_zf4, c4_zf5, c4_zf6, c4_zf7, c4_zf8, c4_zf9, c4_zf10, 
                 c4_zg1, c4_zg2, c4_zg3, c4_zg4, c4_zg5, c4_zg6, c4_zg7, c4_zg8, c4_zg9, c4_zg10,
                 c4_pppa0079, c4_pppa0092, c4_pppa0093, c4_pppa0101, c4_pppa0095, c4_pppa0096, c4_pppa0097,
                 c4_bc1, c4_bc2, c4_a1), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>% 
  # Political knowledge questions, multiple choice: Creating binary Correct 1 / False 0 variable 
  mutate_at(vars(c4_zh1, c4_zh2, c4_zh3, c4_zh4), 
            function(x) case_when(x == 1 ~ 1, x == 2 | x == 3 | x == 4 ~ 0, TRUE ~ NA_real_)) %>%
  mutate(
    # Creating Variables which will identify survey + country after merge #
    dataset = "anes1012",
    cntry = "United States",
    
    # Information if student and if internet access #
    student = NA,
    internet = c4_ppnet,

    # OUTCOMES #
    ## Ideology - liberal to conservativ
    ideology = two_sd(c4_p1),
    
    ## Involvement
    c4_bc1_rc = 2 - c4_bc1,
    c4_bc2_rc = 2 - c4_bc2,
    involvement = two_sd( c4_pppa0079 + c4_pppa0206 + c4_pppa0207 + c4_pppa0208 + c4_pppa0209 + c4_pppa0210 + c4_pppa0211 + c4_pppa0212 + c4_pppa0213 + c4_pppa0092 + c4_pppa0093 + c4_pppa0101 + c4_pppa0095 + c4_pppa0096 + c4_pppa0097 + c4_bc1_rc + c4_bc2_rc),
    
    ## Knowledge
    knowledge = two_sd( c4_zh1 + c4_zh2 + c4_zh3 + c4_zh4 ),
    
    ## Efficacy
    poleff = two_sd( ((c4_f1-5)*-1) + ((c4_f2-5)*-1) ),
    
    ## Interest
    polintr = two_sd( 5 - c4_pppa0035 + 5 - c4_a1 ),
    
    ## Participation
    polpar = two_sd( 2 - c4_pppa0005 + 2 - c4_pppa0220 ),
    
    ## Satisfaction democracy
    stfdem = NA,
    
    ## Media use
    media = two_sd( c4_be1 + c4_be2 + c4_be3 + c4_be4 + c4_be5 ),
    
    ## Political trust
    poltr = NA,
    
    # Big Five traits
    agreeableness = two_sd( ifelse(is.na(c4_zf2), ((c4_zg2-8)*-1) + c4_zg7, ((c4_zf2-8)*-1) + c4_zf7) ),
    extraversion = two_sd( ifelse(is.na(c4_zf1), ((c4_zg6-8)*-1) + c4_zg1, ((c4_zf6-8)*-1) + c4_zf1) ),
    conscientiousness = two_sd( ifelse(is.na(c4_zf3), ((c4_zg8-8)*-1) + c4_zg3, ((c4_zf8-8)*-1) + c4_zf3) ),
    neuroticism = two_sd( ifelse(is.na(c4_zf4), ((c4_zg9-8)*-1) + c4_zg4, ((c4_zf9-8)*-1) + c4_zf4) ),
    openness = two_sd( ifelse(is.na(c4_zf5), ((c4_zg10-8)*-1) + c4_zg5, ((c4_zf10-8)*-1) + c4_zf5) )
  )
