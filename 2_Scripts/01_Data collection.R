# loading packages #
library(tidyverse)
library(rio)
library(foreign)

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
