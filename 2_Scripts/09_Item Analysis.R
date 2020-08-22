### ITEM ANALYSIS ###

library(rio)
library(tidyverse)
library(dplyr)
library(psych)
library(Hmisc)
library(corrplot)
library(sjmisc)
library(mirt)
library(ltm)
library(semTools)
library(lavaan)

## 1. Import ####
tipi_items <- import(here::here("1_Data","tipi_items.csv"))
ipip_items <- import(here::here("1_Data","ipip_items.csv"))
bfiS_items <- import(here::here("1_Data","bfiS_items.csv"))
bfi10_items <- import(here::here("1_Data","bfi10_items.csv"))

# Testing CFA to check if factor is recognised #
# TIPI

# IPIP
# BFI S
# BFI 10

## Decision IPIP observation for further indices. -> multiple items

# Adding group variable under 30 #
ipip_items <- ipip_items %>% mutate(
  under30 = ifelse(age < 30, 1, 0))

### 2. Preparing subset for personality traits #----
ipip_openness <- ipip_items %>% dplyr::select(under30,
  ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
  ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag)

ipip_conscientiousness <- ipip_items %>% dplyr::select(under30,
  ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
  ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo)

ipip_extraversion <- ipip_items %>% dplyr::select(under30,
  ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
  ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay)

ipip_agreeableness <- ipip_items %>% dplyr::select(under30,
  ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
  ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim)

ipip_neuroticism <- ipip_items %>% dplyr::select(under30,
  ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
  ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

#Creating factors
ipip_openness$under30 =  factor(ipip_openness$under30, labels = c("over30","under30"))
ipip_conscientiousness$under30 =  factor(ipip_conscientiousness$under30, labels = c("over30","under30"))
ipip_extraversion$under30 =  factor(ipip_extraversion$under30, labels = c("over30","under30"))
ipip_agreeableness$under30 =  factor(ipip_agreeableness$under30, labels = c("over30","under30"))
ipip_neuroticism$under30 =  factor(ipip_neuroticism$under30, labels = c("over30","under30"))

### 3. CFA Model for each personality traits ----

###### 3.1. OPENNESS ######

open_model = '
OPENNESS =~ ipip_O_vivimag + ipip_O_ricvoca + ipip_O_excidea + ipip_O_quiunde + ipip_O_difword + ipip_O_reflthi + ipip_O_fulidea + ipip_O_difunde + ipip_O_intidea + ipip_O_notimag
'

open.fit = cfa(open_model,
                 data = ipip_openness,
                 meanstructure = T)

summary(open.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
open_under30 <- subset(ipip_openness, under30 == "under30")
open_over30 <- subset(ipip_openness, under30 == "over30")

open.un30.fit = cfa(open_model,
               data = open_under30,
               meanstructure = T)

open.ov30.fit = cfa(open_model,
               data = open_over30,
               meanstructure = T)

summary(open.un30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(open.ov30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.1.1. MG Invariances -----
options(scipen = 999) #turning off scientific notations
multistep_open <- measurementInvariance(model = open_model,
                      data = ipip_openness,
                      group = "under30",
                      strict = T)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_open <- partialInvariance(multistep_open, type = "scalar")
partial_O <- partial_open$results

#free.cfi highest item, that give the most to CFI. problematic constrait = ipip_O_vivimag~1
partial_scalar_O <- measurementInvariance(model = open_model,
                                          data = ipip_openness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_O_vivimag~1"))

# fit.intercepts CFI detal STILL NOT significant. partial invariance on SCALAR step
partial_open2 <- partialInvariance(partial_scalar_O, type = "scalar")
partial_O2 <- partial_open2$results

#free.cfi problematic constraint = ipip_O_difword~1
partial_scalar_O2 <- measurementInvariance(model = open_model,
                                          data = ipip_openness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_O_vivimag~1", "ipip_O_difword~1"))
# 3.1.2. Model details # ----
configural_O <- partial_scalar_O2$fit.configural
summary(configural_O)

metric_O <- partial_scalar_O2$fit.loadings
summary(metric_O)

scalar_O <- partial_scalar_O2$fit.intercepts
summary(scalar_O)

strict_O <- partial_scalar_O2$fit.residuals
summary(scalar_O)

###### 3.2. CONSCIENTIOUSNESS ######

consc_model = '
CONSCIENT =~ ipip_C_alwprep + ipip_C_leabelo + ipip_C_attdeta + ipip_C_makmess + ipip_C_chodone + ipip_C_forgbac + ipip_C_likorde + ipip_C_shirdut + ipip_C_folsche + ipip_C_exactwo
'

consc.fit = cfa(consc_model,
               data = ipip_conscientiousness,
               meanstructure = T)

summary(consc.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
consc_under30 <- subset(ipip_conscientiousness, under30 == "under30")
consc_over30 <- subset(ipip_conscientiousness, under30 == "over30")

consc.un30.fit = cfa(consc_model,
                    data = consc_under30,
                    meanstructure = T)

consc.ov30.fit = cfa(consc_model,
                    data = consc_over30,
                    meanstructure = T)

summary(consc.un30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(consc.ov30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)


# 3.2.1 MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_consc <- measurementInvariance(model = consc_model,
                                        data = ipip_conscientiousness,
                                        group = "under30",
                                        strict = T)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_consc <- partialInvariance(multistep_consc, type = "scalar")
partial_C <- partial_consc$results

# ipip_C_shirdut~1 problematic constraint
# free.cfi highest item, that give the most to CFI. 
partial_scalar_C <- measurementInvariance(model = consc_model,
                                          data = ipip_conscientiousness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_C_shirdut~1"))

# fit.intercepts NOT significant
partial_consc2 <- partialInvariance(partial_scalar_C, type = "scalar")
partial_C2 <- partial_consc2$results

#free.cfi problematic constraint = ipip_C_makmess~1
partial_scalar_C2 <- measurementInvariance(model = consc_model,
                                           data = ipip_conscientiousness,
                                           group = "under30",
                                           strict = T,
                                           group.partial = c("ipip_C_shirdut~1", "ipip_C_makmess~1"))


###### 3.3. EXTRAVERSION ######

extra_model = '
EXTRAVER =~ ipip_E_lifpart + ipip_E_nottalk + ipip_E_comfaro + ipip_E_inbackg + ipip_E_talkdif + ipip_E_noatten + ipip_E_cenatte + ipip_E_quistra + ipip_E_startco + ipip_E_littsay
'
extra.fit = cfa(extra_model,
                data = ipip_extraversion,
                meanstructure = T)

summary(extra.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
extra_under30 <- subset(ipip_extraversion, under30 == "under30")
extra_over30 <- subset(ipip_extraversion, under30 == "over30")

extra.un30.fit = cfa(extra_model,
                     data = extra_under30,
                     meanstructure = T)

extra.ov30.fit = cfa(extra_model,
                     data = extra_over30,
                     meanstructure = T)

summary(extra.un30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(extra.ov30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.3.1. MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_extra <- measurementInvariance(model = extra_model,
                                         data = ipip_extraversion,
                                         group = "under30",
                                         strict = T)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_extra <- partialInvariance(multistep_extra, type = "scalar")
partial_E <- partial_extra$results

# ipip_E_quistra~1 problematic constraint
partial_scalar_E <- measurementInvariance(model = extra_model,
                                         data = ipip_extraversion,
                                         group = "under30",
                                         strict = T,
                                         group.partial = c("ipip_E_quistra~1"))

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_extra2 <- partialInvariance(partial_scalar_E, type = "scalar")
partial_E2 <- partial_extra2$results

# problematic constraint: ipip_E_startco~1
partial_scalar_E2 <- measurementInvariance(model = extra_model,
                                          data = ipip_extraversion,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_E_quistra~1","ipip_E_startco~1"))


###### 3.4. AGREEABLENESS ######

agree_model = '
AGREEABL =~ ipip_A_littcon + ipip_A_intpeop + ipip_A_peopeas + ipip_A_insupeo + ipip_A_symfeel + ipip_A_feelemo + ipip_A_softhea + ipip_A_nointot + ipip_A_nointpr + ipip_A_taketim
'
agree.fit = cfa(agree_model,
                data = ipip_agreeableness,
                meanstructure = T)

summary(agree.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
agree_under30 <- subset(ipip_agreeableness, under30 == "under30")
agree_over30 <- subset(ipip_agreeableness, under30 == "over30")

agree.un30.fit = cfa(agree_model,
                     data = agree_under30,
                     meanstructure = T)

agree.ov30.fit = cfa(agree_model,
                     data = agree_over30,
                     meanstructure = T)

summary(agree.un30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(agree.ov30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.4.1. MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_agree <- measurementInvariance(model = agree_model,
                                         data = ipip_agreeableness,
                                         group = "under30",
                                         strict = T)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_agree <- partialInvariance(multistep_agree, type = "scalar")
partial_A <- partial_agree$results

# ipip_A_insupeo~1 problematic constraint
# free.cfi highest item, that give the most to CFI. 
partial_scalar_A <- measurementInvariance(model = agree_model,
                                          data = ipip_agreeableness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_A_insupeo~1"))

###### 3.5. NEUROTICISM #####

neuro_model = '
NEUROTIC = ~ ipip_N_irrieas + ipip_N_seldblu + ipip_N_easdisr + ipip_N_upseasy + ipip_N_streasy + ipip_N_oftblue + ipip_N_relamot + ipip_N_worryth + ipip_N_chamood + ipip_N_moodswi
'
neuro.fit = cfa(neuro_model,
                data = ipip_neuroticism,
                meanstructure = T)

summary(neuro.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
neuro_under30 <- subset(ipip_neuroticism, under30 == "under30")
neuro_over30 <- subset(ipip_neuroticism, under30 == "over30")

neuro.un30.fit = cfa(neuro_model,
                     data = neuro_under30,
                     meanstructure = T)

neuro.ov30.fit = cfa(neuro_model,
                     data = neuro_over30,
                     meanstructure = T)

summary(neuro.un30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(neuro.ov30.fit,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.5.1. MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_neuro <- measurementInvariance(model = neuro_model,
                                         data = ipip_neuroticism,
                                         group = "under30",
                                         strict = T)

# >> Measurement Invariance is fully granted

#### 4. DIF with MIRT ####
#multipleGroup function
open_model = '
OPENNESS =~ ipip_O_vivimag + ipip_O_ricvoca + ipip_O_excidea + ipip_O_quiunde + ipip_O_difword + ipip_O_reflthi + ipip_O_fulidea + ipip_O_difunde + ipip_O_intidea + ipip_O_notimag
'
MG_open <- multipleGroup(ipip_openness, open_model, group = "under30", SE = T)

## I tried to change the class from variable "under30" from factor to numeric

ipip_openness_MG <- ipip_openness %>% 
  mutate(under30 = as.numeric(under30))

MG_open <- multipleGroup(ipip_openness_MG, open_model, group = under30, SE = T)

## polytomous IRT testing # unidimensional mandatory# ----

#ipip_O_reflthi, ipip_O_notimag slightly problematic
ipip_O_irtmod <- mirt(data = ipip_openness,
                     model = 1,
                     itemtype = "gpcm")
summary(ipip_O_irtmod) #standardized coefficients
coef(ipip_O_irtmod, IRTbars = T) #coefficients (IRT format and not slope interact format) a first row over one
itemplot(ipip_O_irtmod, 6, type = "trace") #curves for each item, super
itemplot(ipip_O_irtmod, 3, type = "trace") #higher 3 4
itemplot(ipip_O_irtmod, 6, type = "trace") #very low 1
itemplot(ipip_O_irtmod, 9, type = "trace") #strong 3
plot(ipip_O_irtmod, type = "trace")

itemplot(ipip_O_irtmod, 10, type = "info") #ICC
# Shows what area of the latent variable is being measured