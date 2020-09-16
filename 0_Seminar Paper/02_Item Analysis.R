### ITEM ANALYSIS ###

library(rio)
library(tidyverse)
library(dplyr)
library(psych)
library(Hmisc)
library(corrplot)
library(sjmisc)
library(mirt)
library(sirt)
library(ltm)
library(semTools)
library(lavaan)
library(ggplot2)

## Import ####
tipi_items <- import(here::here("1_Data","tipi_items.csv"))
ipip_items <- import(here::here("1_Data","ipip_items.csv"))
bfiS_items <- import(here::here("1_Data","bfiS_items.csv"))
bfi10_items <- import(here::here("1_Data","bfi10_items.csv"))

#### 0. Assumptions of correlation ####
#### BFI-10 ####
# correlation
bfi10 <- bfi10_items %>% dplyr::select(bfi_O_imag, bfi_O_arti, bfi_C_lazy, bfi_C_thor, bfi_E_rese, bfi_E_soci, bfi_A_faul, bfi_A_trus, bfi_N_rela, bfi_N_nerv)
bfi10_R <- rcorr(as.matrix(bfi10), 2)
bfi10_R
as.table(bfi10_R$r)

#### BFI-S ####
bfiS <- bfiS_items %>% dplyr::select(bfis_O_newidea, bfis_O_apprart, bfis_O_highima, bfis_C_worktho, bfis_C_taskeff, bfis_C_ratlazy, bfis_E_talkati, bfis_E_sociabl,
                                           bfis_E_reserve, bfis_A_harshwo, bfis_A_ablefor, bfis_A_conside, bfis_N_worries, bfis_N_nervous, bfis_N_relaxed)
bfiS_R <- rcorr(as.matrix(bfiS), 2)
bfiS_R
as.table(bfiS_R$r)

#### IPIP ####
ipip_O <- ipip_items %>% dplyr::select(ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
                                              ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag)

ipip_C <- ipip_items %>% dplyr::select(ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
                                                       ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo)

ipip_E <- ipip_items %>% dplyr::select(ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
                                                  ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay)

ipip_A <- ipip_items %>% dplyr::select(ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
                                                   ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim)

ipip_N <- ipip_items %>% dplyr::select(ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
                                                 ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

(ipip_O_R <- rcorr(as.matrix(ipip_O), 2))
(ipip_C_R <- rcorr(as.matrix(ipip_C), 2))
(ipip_E_R <- rcorr(as.matrix(ipip_E), 2))
(ipip_A_R <- rcorr(as.matrix(ipip_A), 2))
(ipip_N_R <- rcorr(as.matrix(ipip_N), 2))

round(as.table(ipip_O_R$r), digits = 3)
round(as.table(ipip_C_R$r), digits = 3)
round(as.table(ipip_E_R$r), digits = 3)
round(as.table(ipip_A_R$r), digits = 3)
round(as.table(ipip_N_R$r), digits = 3)



#### TIPI ####
# overall
tipi <- tipi_items %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_R <- rcorr(as.matrix(tipi), 2)
tipi_R
round(as.table(tipi_R$r), digits = 3)

# country specific
tipi_items_lapop <- tipi_items %>% filter(dataset == "lapop")
tipi_items_ces <- tipi_items %>% filter(dataset == "ces")
tipi_items_anes1012 <- tipi_items %>% filter(dataset == "anes1012")
tipi_items_anes12 <- tipi_items %>% filter(dataset == "anes12")
tipi_items_anes16 <- tipi_items %>% filter(dataset == "anes16")
tipi_items_nzes <- tipi_items %>% filter(dataset == "nzes")

tipi_lapop <- tipi_items_lapop %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_ces <- tipi_items_ces %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_anes1012 <- tipi_items_anes1012 %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_anes12 <- tipi_items_anes12 %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_anes16 <- tipi_items_anes16 %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)
tipi_nzes <- tipi_items_nzes %>% dplyr::select(tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)

(tipi_lapop_R <- rcorr(as.matrix(tipi_lapop), 2))
(tipi_ces_R <- rcorr(as.matrix(tipi_ces), 2))
(tipi_anes1012_R <- rcorr(as.matrix(tipi_anes1012), 2))
(tipi_anes12_R <- rcorr(as.matrix(tipi_anes12), 2))
(tipi_anes16_R <- rcorr(as.matrix(tipi_anes16), 2))
(tipi_nzes_R <- rcorr(as.matrix(tipi_nzes), 2))


#### 1. Testing factanalysis to check if factors show ####
#### BFI 10 ####
bfi10_fact <- fa(bfi10, nfactors = 5, rotate = "varimax", fm = "ml")
bfi10_fact

# CFI index
1 - ((bfi10_fact$STATISTIC - bfi10_fact$dof) /
       (bfi10_fact$null.chisq - bfi10_fact$null.dof))

#### BFI S ####
bfis_fact <- fa(bfiS, nfactors = 5, rotate = "varimax", fm = "ml")
bfis_fact

# CFI index
1 - ((bfis_fact$STATISTIC - bfis_fact$dof) /
       (bfis_fact$null.chisq - bfis_fact$null.dof))

#### IPIP ####
ipip <- ipip_items %>% dplyr::select(ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
                                     ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag,
                                     ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
                                     ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo,
                                     ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
                                     ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay,
                                     ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
                                     ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim,
                                     ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
                                     ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)


ipip_fact <- fa(ipip, nfactors = 5, rotate = "varimax", fm = "ml")
ipip_fact

# CFI index
1 - ((ipip_fact$STATISTIC - ipip_fact$dof) /
       (ipip_fact$null.chisq - ipip_fact$null.dof))


#### TIPI ####
(tipi_fact <- fa(tipi, nfactors = 5, rotate = "varimax", fm = "ml"))

# CFI index
1 - ((tipi_fact$STATISTIC - tipi_fact$dof) /
       (tipi_fact$null.chisq - tipi_fact$null.dof))

(tipi_lapop_fact <- fa(tipi_lapop, nfactors = 5, rotate = "varimax", fm = "ml"))
(tipi_anes1012_fact <- fa(tipi_anes1012, nfactors = 5, rotate = "varimax", fm = "ml"))
(tipi_anes12_fact <- fa(tipi_anes12, nfactors = 5, rotate = "varimax", fm = "ml"))
(tipi_anes16_fact <- fa(tipi_anes16, nfactors = 5, rotate = "varimax", fm = "ml"))
(tipi_nzes_fact <- fa(tipi_nzes, nfactors = 5, rotate = "varimax", fm = "ml"))
(tipi_ces_fact <- fa(tipi_ces, nfactors = 5, rotate = "varimax", fm = "ml"))

### 2. IPIP: Preparing subset for personality traits #----
#creating subset under and over 30# ----
ipip_items <- ipip_items %>% mutate(
  under30 = ifelse(age < 30, 1, 0))

ipip_item_under30 <- ipip_items %>% filter(
  under30 == 1)

ipip_item_over30 <- ipip_items %>% filter(
  under30 == 0)

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

fitmeasures(multistep_open$fit.configural)
fitmeasures(multistep_open$fit.loadings)
fitmeasures(multistep_open$fit.intercepts)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_open <- partialInvariance(multistep_open, type = "scalar")
partial_O <- partial_open$results

#free.cfi highest item, that give the most to CFI. problematic constrait = ipip_O_vivimag~1
partial_scalar_O <- measurementInvariance(model = open_model,
                                          data = ipip_openness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_O_vivimag~1"))

fitmeasures(partial_scalar_O$fit.intercepts)

# fit.intercepts CFI detal STILL NOT significant. partial invariance on SCALAR step
partial_open2 <- partialInvariance(partial_scalar_O, type = "scalar")
partial_O2 <- partial_open2$results

#free.cfi problematic constraint = ipip_O_difword~1
partial_scalar_O2 <- measurementInvariance(model = open_model,
                                          data = ipip_openness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("ipip_O_vivimag~1", "ipip_O_difword~1"))
fitmeasures(partial_scalar_O2$fit.intercepts)

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

fitmeasures(multistep_consc$fit.configural)
fitmeasures(multistep_consc$fit.loadings)
fitmeasures(multistep_consc$fit.intercepts)


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

fitmeasures(partial_scalar_C$fit.intercepts)

# fit.intercepts NOT significant
partial_consc2 <- partialInvariance(partial_scalar_C, type = "scalar")
partial_C2 <- partial_consc2$results

#free.cfi problematic constraint = ipip_C_makmess~1
partial_scalar_C2 <- measurementInvariance(model = consc_model,
                                           data = ipip_conscientiousness,
                                           group = "under30",
                                           strict = T,
                                           group.partial = c("ipip_C_shirdut~1", "ipip_C_makmess~1"))

fitmeasures(partial_scalar_C2$fit.intercepts)

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

fitmeasures(multistep_extra$fit.configural)
fitmeasures(multistep_extra$fit.loadings)
fitmeasures(multistep_extra$fit.intercepts)

# fit.intercepts CFI delta not significant. partial invariance on SCALAR step
partial_extra <- partialInvariance(multistep_extra, type = "scalar")
partial_E <- partial_extra$results

# ipip_E_quistra~1 problematic constraint
partial_scalar_E <- measurementInvariance(model = extra_model,
                                         data = ipip_extraversion,
                                         group = "under30",
                                         strict = T,
                                         group.partial = c("ipip_E_quistra~1"))

fitmeasures(partial_scalar_E$fit.intercepts)


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
fitmeasures(multistep_agree$fit.configural)
fitmeasures(multistep_agree$fit.loadings)
fitmeasures(multistep_agree$fit.intercepts)

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

fitmeasures(partial_scalar_A$fit.intercepts)

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

fitmeasures(multistep_neuro$fit.configural)
fitmeasures(multistep_neuro$fit.loadings)
fitmeasures(multistep_neuro$fit.intercepts)
fitmeasures(multistep_neuro$fit.residuals)

# >> Measurement Invariance is fully granted


#### 4. BFI-S: Preparing subsets for personality traits ####
#creating subset under and over 30# ----
bfiS_items <- bfiS_items %>% mutate(
  under30 = ifelse(age < 30, 1, 0))

bfiS_items_under30 <- bfiS_items %>% filter(
  under30 == 1)

bfiS_items_over30 <- bfiS_items %>% filter(
  under30 == 0)

bfiS_openness <- bfiS_items %>% dplyr::select(under30, bfis_O_newidea, bfis_O_apprart, bfis_O_highima)
bfiS_conscientiousness <- bfiS_items %>% dplyr::select(under30, bfis_C_worktho, bfis_C_taskeff, bfis_C_ratlazy)
bfiS_extraversion <- bfiS_items %>% dplyr::select(under30, bfis_E_talkati, bfis_E_sociabl, bfis_E_reserve)
bfiS_agreeableness <- bfiS_items %>% dplyr::select(under30, bfis_A_harshwo, bfis_A_ablefor, bfis_A_conside)
bfiS_neuroticism <- bfiS_items %>% dplyr::select(under30, bfis_N_worries, bfis_N_nervous, bfis_N_relaxed)

#Creating factors
bfiS_openness$under30 =  factor(bfiS_openness$under30, labels = c("over30","under30"))
bfiS_conscientiousness$under30 =  factor(bfiS_conscientiousness$under30, labels = c("over30","under30"))
bfiS_extraversion$under30 =  factor(bfiS_extraversion$under30, labels = c("over30","under30"))
bfiS_agreeableness$under30 =  factor(bfiS_agreeableness$under30, labels = c("over30","under30"))
bfiS_neuroticism$under30 =  factor(bfiS_neuroticism$under30, labels = c("over30","under30"))

### 5. BFI-S: CFA Model for each personality traits ----

#Descriptives about sample sizes
table(bfiS_openness$under30)

###### 5.1. OPENNESS ######

open_model_bfis = '
OPENNESS =~ bfis_O_newidea + bfis_O_apprart + bfis_O_highima
'

open.fit.bfis = cfa(open_model_bfis, data = bfiS_openness, meanstructure = T)
summary(open.fit.bfis, standardized = T, rsquare = T, fit.measure = T)

# Models for group datasets #
open_under30_bfis <- subset(bfiS_openness, under30 == "under30")
open_over30_bfis <- subset(bfiS_openness, under30 == "over30")

open.un30.fit.bfis = cfa(open_model_bfis, data = open_under30_bfis, meanstructure = T)
open.ov30.fit.bfis = cfa(open_model_bfis, data = open_over30_bfis, meanstructure = T)

summary(open.un30.fit.bfis, standardized = T,  rsquare = T,  fit.measure = T)
summary(open.ov30.fit.bfis, standardized = T,  rsquare = T, fit.measure = T)

# 5.1.1. MG Invariances -----
options(scipen = 999) #turning off scientific notations

multistep_open_bfis <- measurementInvariance(model = open_model_bfis,
                                        data = bfiS_openness,
                                        group = "under30",
                                        strict = T)

## Configural ##
open.fit.bfis.config = cfa(open_model_bfis, data = bfiS_openness, meanstructure = T, group = "under30")
summary(open.fit.bfis.config, standardized = T, rsquare = T, fit.measure = T)

## Metric ##
open.fit.bfis.metric = cfa(open_model_bfis, data = bfiS_openness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings"))
summary(open.fit.bfis.metric, standardized = T, rsquare = T, fit.measure = T)

## Scalar ##
open.fit.bfis.scalar = cfa(open_model_bfis, data = bfiS_openness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings","intercepts"))
summary(open.fit.bfis.scalar, standardized = T, rsquare = T, fit.measure = T)

## Strict ##
open.fit.bfis.strict = cfa(open_model_bfis, data = bfiS_openness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings","intercepts","residuals"))
summary(open.fit.bfis.strict, standardized = T, rsquare = T, fit.measure = T)


###### 5.2. CONSCIENTIOUSNESS ######

consc_model_bfis = '
CONSCIENT =~ bfis_C_worktho + bfis_C_taskeff + bfis_C_ratlazy
'

consc.fit.bfis = cfa(consc_model_bfis,  data = bfiS_conscientiousness,  meanstructure = T)
summary(consc.fit.bfis,  standardized = T,  rsquare = T,  fit.measure = T)

# Models for group datasets #
consc_under30_bfis <- subset(bfiS_conscientiousness, under30 == "under30")
consc_over30_bfis <- subset(bfiS_conscientiousness, under30 == "over30")

consc.un30.fit.bfis = cfa(consc_model_bfis,  data = consc_under30_bfis,   meanstructure = T)
consc.ov30.fit.bfis = cfa(consc_model_bfis,  data = consc_over30_bfis,    meanstructure = T)

summary(consc.un30.fit.bfis, standardized = T,  rsquare = T,  fit.measure = T)
summary(consc.ov30.fit.bfis, standardized = T,  rsquare = T,  fit.measure = T)


# 5.2.1 MG Invariances  ----
options(scipen = 999) #turning off scientific notations
multistep_consc_bfis <- measurementInvariance(model = consc_model_bfis,
                                         data = bfiS_conscientiousness,
                                         group = "under30",
                                         strict = T)

## Configural ##
cons.fit.bfis.config = cfa(consc_model_bfis, data = bfiS_conscientiousness, meanstructure = T, group = "under30")
summary(cons.fit.bfis.config, standardized = T, rsquare = T, fit.measure = T)

## Metric ##
cons.fit.bfis.metric = cfa(consc_model_bfis, data = bfiS_conscientiousness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings"))
summary(cons.fit.bfis.metric, standardized = T, rsquare = T, fit.measure = T)

## Scalar ##
cons.fit.bfis.scalar = cfa(consc_model_bfis, data = bfiS_conscientiousness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings","intercepts"))
summary(cons.fit.bfis.scalar, standardized = T, rsquare = T, fit.measure = T)


# fit.intercepts partial invariance on SCALAR step (delta CFI too high)
partial_consc_bfis <- partialInvariance(multistep_consc_bfis, type = "scalar")
partial_C_bfis <- partial_consc_bfis$results

# ipip_C_shirdut~1 problematic constraint
# free.cfi highest item, that give the most to CFI. 
partial_scalar_C_bfis <- measurementInvariance(model = consc_model_bfis,
                                          data = bfiS_conscientiousness,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("bfis_C_ratlazy~1"))

summary(partial_scalar_C_bfis$fit.intercepts)
fitmeasures(partial_scalar_C_bfis$fit.intercepts)

###### 5.3. EXTRAVERSION ######

extra_model_bfis = '
EXTRAVER =~ bfis_E_talkati + bfis_E_sociabl + bfis_E_reserve
'
extra.fit.bfis = cfa(extra_model_bfis,  data = bfiS_extraversion,  meanstructure = T)
summary(extra.fit.bfis,  standardized = T,  rsquare = T,  fit.measure = T)

# Models for group datasets #
extra_under30_bfis <- subset(bfiS_extraversion, under30 == "under30")
extra_over30_bfis <- subset(bfiS_extraversion, under30 == "over30")

extra.un30.fit.bfis = cfa(extra_model_bfis,  data = extra_under30_bfis,   meanstructure = T)
extra.ov30.fit.bfis = cfa(extra_model_bfis,  data = extra_over30_bfis,    meanstructure = T)

summary(extra.un30.fit.bfis, standardized = T,  rsquare = T,  fit.measure = T)
summary(extra.ov30.fit.bfis, standardized = T,  rsquare = T,  fit.measure = T)


# 3.2.1 MG Invariances  ----
options(scipen = 999) #turning off scientific notations
multistep_extra_bfis <- measurementInvariance(model = extra_model_bfis,
                                         data = bfiS_extraversion,
                                         group = "under30",
                                         strict = T)

## Configural ##
extra.fit.bfis.config = cfa(extra_model_bfis, data = bfiS_extraversion, meanstructure = T, group = "under30")
summary(extra.fit.bfis.config, standardized = T, rsquare = T, fit.measure = T)

## Metric ##
extra.fit.bfis.metric = cfa(extra_model_bfis, data = bfiS_extraversion, meanstructure = T, 
                           group = "under30", group.equal = c("loadings"))
summary(extra.fit.bfis.metric, standardized = T, rsquare = T, fit.measure = T)


# fit.intercepts CFI delta not significant. partial invariance on METRIC step
partial_extra_bfis <- partialInvariance(multistep_extra_bfis, type = "metric")
partial_E_bfis <- partial_extra_bfis$results

# ipip_E_quistra~1 problematic constraint
partial_scalar_E_bfis <- measurementInvariance(model = extra_model_bfis,
                                          data = bfiS_extraversion,
                                          group = "under30",
                                          strict = T,
                                          group.partial = c("EXTRAVER=~bfis_E_reserve"))

summary(partial_scalar_E_bfis$fit.intercepts)
fitmeasures(partial_scalar_E_bfis$fit.intercepts)

###### 3.4. AGREEABLENESS ######

agree_model_bfis = '
AGREEABL =~ bfis_A_harshwo + bfis_A_ablefor + bfis_A_conside
'
agree.fit.bfis = cfa(agree_model_bfis,
                data = bfiS_agreeableness,
                meanstructure = T)

summary(agree.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
agree_under30_bfis <- subset(bfiS_agreeableness, under30 == "under30")
agree_over30_bfis <- subset(bfiS_agreeableness, under30 == "over30")

agree.un30.fit.bfis = cfa(agree_model_bfis,
                     data = agree_under30_bfis,
                     meanstructure = T)

agree.ov30.fit.bfis = cfa(agree_model_bfis,
                     data = agree_over30_bfis,
                     meanstructure = T)

summary(agree.un30.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(agree.ov30.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.4.1. MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_agree_bfis <- measurementInvariance(model = agree_model_bfis,
                                         data = bfiS_agreeableness,
                                         group = "under30",
                                         strict = T)
## Configural ##
agree.fit.bfis.config = cfa(agree_model_bfis, data = bfiS_agreeableness, meanstructure = T, group = "under30")
summary(agree.fit.bfis.config, standardized = T, rsquare = T, fit.measure = T)

## Metric ##
agree.fit.bfis.metric = cfa(agree_model_bfis, data = bfiS_agreeableness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings"))
summary(agree.fit.bfis.metric, standardized = T, rsquare = T, fit.measure = T)

## Scalar ##
agree.fit.bfis.scalar = cfa(agree_model_bfis, data = bfiS_agreeableness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings","intercepts"))
summary(agree.fit.bfis.scalar, standardized = T, rsquare = T, fit.measure = T)

## Strict ##
agree.fit.bfis.strict = cfa(agree_model_bfis, data = bfiS_agreeableness, meanstructure = T, 
                           group = "under30", group.equal = c("loadings","intercepts","residuals"))
summary(agree.fit.bfis.strict, standardized = T, rsquare = T, fit.measure = T)


###### 3.5. NEUROTICISM #####

neuro_model_bfis = '
NEUROTIC = ~ bfis_N_worries + bfis_N_nervous + bfis_N_relaxed
'
neuro.fit.bfis = cfa(neuro_model_bfis,
                data = bfiS_neuroticism,
                meanstructure = T)

summary(neuro.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# Models for group datasets #
neuro_under30_bfis <- subset(bfiS_neuroticism, under30 == "under30")
neuro_over30_bfis <- subset(bfiS_neuroticism, under30 == "over30")

neuro.un30.fit.bfis = cfa(neuro_model_bfis,
                     data = neuro_under30_bfis,
                     meanstructure = T)

neuro.ov30.fit.bfis = cfa(neuro_model_bfis,
                     data = neuro_over30_bfis,
                     meanstructure = T)

summary(neuro.un30.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

summary(neuro.ov30.fit.bfis,
        standardized = T,
        rsquare = T,
        fit.measure = T)

# 3.5.1. MG Invariances # ----
options(scipen = 999) #turning off scientific notations
multistep_neuro_bfis <- measurementInvariance(model = neuro_model_bfis,
                                         data = bfiS_neuroticism,
                                         group = "under30",
                                         strict = T)
# getting fit coefficients for table
fitmeasures(multistep_neuro_bfis$fit.configural)
fitmeasures(multistep_neuro_bfis$fit.loadings)
fitmeasures(multistep_neuro_bfis$fit.intercepts)
fitmeasures(multistep_neuro_bfis$fit.residuals)

# >> Measurement Invariance is fully granted


#### 5. Mean SD, Tables per group ####
#### IPIP ####

# Subset under 30
under30_openness <- subset(ipip_openness, under30 == "under30")
under30_conscientiousness <- subset(ipip_conscientiousness, under30 == "under30")
under30_extraversion <- subset(ipip_extraversion, under30 == "under30")
under30_agreeableness <- subset(ipip_agreeableness, under30 == "under30")
under30_neuroticism <- subset(ipip_neuroticism, under30 == "under30")

# Subset over 30
over30_openness <- subset(ipip_openness, under30 == "over30")
over30_conscientiousness <- subset(ipip_conscientiousness, under30 == "over30")
over30_extraversion <- subset(ipip_extraversion, under30 == "over30")
over30_agreeableness <- subset(ipip_agreeableness, under30 == "over30")
over30_neuroticism <- subset(ipip_neuroticism, under30 == "over30")

# Mean, SD etc. #
# openness #
psych::describeBy(ipip_openness, group = c("under30"))
psych::describeBy(ipip_conscientiousness, group = c("under30"))
psych::describeBy(ipip_extraversion, group = c("under30"))
psych::describeBy(ipip_agreeableness, group = c("under30"))
psych::describeBy(ipip_neuroticism, group = c("under30"))

#### BFI-S ####
# Subset under 30
under30_openness_bfis <- subset(bfiS_openness, under30 == "under30")
under30_conscientiousness_bfis <- subset(bfiS_conscientiousness, under30 == "under30")
under30_extraversion_bfis <- subset(bfiS_extraversion, under30 == "under30")
under30_agreeableness_bfis <- subset(bfiS_agreeableness, under30 == "under30")
under30_neuroticism_bfis <- subset(bfiS_neuroticism, under30 == "under30")

# Subset over 30
over30_openness_bfis <- subset(bfiS_openness, under30 == "over30")
over30_conscientiousness_bfis <- subset(bfiS_conscientiousness, under30 == "over30")
over30_extraversion_bfis <- subset(bfiS_extraversion, under30 == "over30")
over30_agreeableness_bfis <- subset(bfiS_agreeableness, under30 == "over30")
over30_neuroticism_bfis <- subset(bfiS_neuroticism, under30 == "over30")

# Mean, SD etc. #
# openness #
psych::describeBy(bfiS_openness, group = c("under30"))
psych::describeBy(bfiS_conscientiousness, group = c("under30"))
psych::describeBy(bfiS_extraversion, group = c("under30"))
psych::describeBy(bfiS_agreeableness, group = c("under30"))
psych::describeBy(bfiS_neuroticism, group = c("under30"))



#### Cronbach Alpha ####
# BFI 10 #
bfi10_O <- bfi10_items %>% dplyr::select(bfi_O_imag, bfi_O_arti)
bfi10_C <- bfi10_items %>% dplyr::select(bfi_C_lazy, bfi_C_thor)
bfi10_E <- bfi10_items %>% dplyr::select(bfi_E_rese, bfi_E_soci)
bfi10_A <- bfi10_items %>% dplyr::select(bfi_A_faul, bfi_A_trus)
bfi10_N <- bfi10_items %>% dplyr::select(bfi_N_rela, bfi_N_nerv)

alpha(bfi10_O)
alpha(bfi10_C)
alpha(bfi10_E)
alpha(bfi10_A)
alpha(bfi10_N)


# BFI S #
bfis_O <- bfiS_items %>% dplyr::select(bfis_O_newidea, bfis_O_apprart, bfis_O_highima)
bfis_C <- bfiS_items %>% dplyr::select(bfis_C_worktho, bfis_C_taskeff, bfis_C_ratlazy)
bfis_E <- bfiS_items %>% dplyr::select(bfis_E_talkati, bfis_E_sociabl, bfis_E_reserve)
bfis_A <- bfiS_items %>% dplyr::select(bfis_A_harshwo, bfis_A_ablefor, bfis_A_conside)
bfis_N <- bfiS_items %>% dplyr::select(bfis_N_worries, bfis_N_nervous, bfis_N_relaxed)

alpha(bfis_O)
alpha(bfis_C)
alpha(bfis_E)
alpha(bfis_A)
alpha(bfis_N)

# IPIP #
ipip_O <- ipip_items %>% dplyr::select(ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
                                      ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag)

ipip_C <- ipip_items %>% dplyr::select(ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
                                      ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo)

ipip_E <- ipip_items %>% dplyr::select(ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
                                      ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay)

ipip_A <- ipip_items %>% dplyr::select(ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
                                       ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim)

ipip_N <- ipip_items %>% dplyr::select(ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
                                      ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

alpha(ipip_O)
alpha(ipip_C)
alpha(ipip_E)
alpha(ipip_A)
alpha(ipip_N)

bfi10_O <- bfi10_items %>% dplyr::select(bfi_O_imag, bfi_O_arti)
bfi10_C <- bfi10_items %>% dplyr::select(bfi_C_lazy, bfi_C_thor)
bfi10_E <- bfi10_items %>% dplyr::select(bfi_E_rese, bfi_E_soci)
bfi10_A <- bfi10_items %>% dplyr::select(bfi_A_faul, bfi_A_trus)
bfi10_N <- bfi10_items %>% dplyr::select(bfi_N_rela, bfi_N_nerv)

alpha(bfi10_O)
alpha(bfi10_C)
alpha(bfi10_E)
alpha(bfi10_A)
alpha(bfi10_N)


# TIPI #
tipi_O <- tipi_items %>% dplyr::select(tipi_O_open, tipi_O_conv)
tipi_C <- tipi_items %>% dplyr::select(tipi_C_depe, tipi_C_diso)
tipi_E <- tipi_items %>% dplyr::select(tipi_E_extr, tipi_E_rese)
tipi_A <- tipi_items %>% dplyr::select(tipi_A_warm, tipi_A_crit)
tipi_N <- tipi_items %>% dplyr::select(tipi_N_anxi, tipi_N_calm)

alpha(tipi_O)
alpha(tipi_C)
alpha(tipi_E)
alpha(tipi_A)
alpha(tipi_N)
