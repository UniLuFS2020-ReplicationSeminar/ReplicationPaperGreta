# loading packages #
library(tidyverse)
library(rio)
library(foreign)
<<<<<<< HEAD


#################
## DATA IMPORT ##
#################

# American National Election Study 2010 - 2012 #
anes1012 <- import(here::here("1_Data","1_Panel Datasets","anes_specialstudies_2012egss4.dta"))

# American National Election Study 2012 #
anes12 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2012.dta"))

# American National Election Study 2016 #
anes16 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2016.dta"))

# British Election Study #
bes <- read.spss("1_Data/1_Panel Datasets/BES2017_W13_v1.6.sav", to.data.frame = TRUE, use.value.labels = FALSE)

# Swiss Household Panel #
shp <- import(here::here("1_Data","1_Panel Datasets","shp09_p_user.dta"))

# Longitudinal Internet Studies in the Social Sciences #
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

# Latin American Public Opinion Project #
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
