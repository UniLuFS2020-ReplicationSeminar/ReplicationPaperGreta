## CREATION OF DATASET WITH PERSONALITY TEST ITEMS ##
## ! This script will not run without access to original Panel Datasets ! ##
## Open script 09_Item Analysis for further with personality items ##

library(rio)
library(tidyverse)

#function#
## CREATING FUNCTION TO STANDARDISE VARIABLES divided by 2 SD##
two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}

## TIPI: American National Election Study 2010 - 2012 #----
#Import#
anes1012 <- import(here::here("1_Data","1_Panel Datasets","anes_specialstudies_2012egss4.dta"))

anes1012 <- anes1012 %>% 
  mutate_at(vars(c4_zf1, c4_zf2, c4_zf3, c4_zf4, c4_zf5, c4_zf6, c4_zf7, c4_zf8, c4_zf9, c4_zf10, 
                 c4_zg1, c4_zg2, c4_zg3, c4_zg4, c4_zg5, c4_zg6, c4_zg7, c4_zg8, c4_zg9, c4_zg10), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>% 
  mutate(dataset = "anes1012", 
         age = ifelse(c4_ppage > 1 & c4_ppage < 100, c4_ppage, NA),
  
  # tipi personality items 2 per dim, scale 1 disagree strongly - 7 agree strongly
  # OPENNESS #
  tipi_O_open = two_sd(ifelse(is.na(c4_zf5), c4_zg5, c4_zf5)), #open to new experiences, complex
  tipi_O_conv = two_sd(ifelse(is.na(c4_zf10), ((c4_zg10-8)*-1), ((c4_zf10-8)*-1))), #conventional, uncreativ
  
  # CONSCIENTIOUSNESS #
  tipi_C_depe = two_sd(ifelse(is.na(c4_zf3), c4_zg3, c4_zf3)),#dependable, self-diciplined
  tipi_C_diso = two_sd(ifelse(is.na(c4_zf8), ((c4_zg8-8)*-1), ((c4_zf8-8)*-1))),#disorganized, careless
  
  # EXTRAVERSION #
  tipi_E_extr = two_sd(ifelse(is.na(c4_zf1), c4_zg1, c4_zf1)), #extraverted, enthusiastic
  tipi_E_rese = two_sd(ifelse(is.na(c4_zf6), ((c4_zg6-8)*-1), ((c4_zf6-8)*-1))), #reserved, quiet
  
  # AGREEABLENESS #
  tipi_A_warm = two_sd(ifelse(is.na(c4_zf7), c4_zg7, c4_zf7)), #sympathetic, warm
  tipi_A_crit = two_sd(ifelse(is.na(c4_zf2), ((c4_zg2-8)*-1), ((c4_zf2-8)*-1))), #critical, quarrelsome
  
  # NEUROTICISM #
  tipi_N_anxi = two_sd(ifelse(is.na(c4_zf4), c4_zg4, c4_zf4)), #anxious, easily upset
  tipi_N_calm = two_sd(ifelse(is.na(c4_zf9), ((c4_zg9-8)*-1), ((c4_zf9-8)*-1)))) #calm, emotionally stable

anes1012_var <- anes1012 %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)


## TIPI: American National Election Study 2012 #----
#Import#
anes12 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2012.dta"))

anes12 <- anes12 %>% 
  mutate_at(vars(tipi_open,tipi_conv,tipi_dep,tipi_disorg,tipi_extra,tipi_resv,tipi_warm,tipi_crit, tipi_anx, tipi_calm), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>% 
  
  mutate(
    dataset = "anes12",
    age = ifelse(dem_age_r_x > 1 & dem_age_r_x < 100, dem_age_r_x, NA),
  
  # tipi personality items 2 per dim, scale 1 disagree strongly - 7 agree strongly
  # OPENNESS #
  tipi_O_open = two_sd(tipi_open),
  tipi_O_conv = two_sd((8-tipi_conv)),
  
  # CONSCIENTIOUSNESS #
  tipi_C_depe = two_sd(tipi_dep),
  tipi_C_diso = two_sd(8-tipi_disorg),
      
  # EXTRAVERSION #
  tipi_E_extr = two_sd(tipi_extra),
  tipi_E_rese = two_sd(8-tipi_resv),
  
  # AGREEABLENESS #
  tipi_A_warm = two_sd(tipi_warm),
  tipi_A_crit = two_sd(8-tipi_crit),
              
  # NEUROTICISM #
  tipi_N_anxi = two_sd(tipi_anx),
  tipi_N_calm = two_sd(8-tipi_calm)
)

anes12_var <- anes12 %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)

## TIPI: American National Election Study 2016 #----
#Import#
anes16 <- import(here::here("1_Data","1_Panel Datasets","anes_timeseries_2016.dta"))

anes16 <- anes16 %>% 
  mutate_at(vars(V162337,V162342,V162335,V162340,V162333,V162338,V162339,V162334,V162336,V162341), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
  dataset = "anes16",
  age = ifelse(V161267 > 1 & V161267 < 100, V161267, NA),
  
  # tipi personality items 2 per dim, scale 1 disagree strongly - 7 agree strongly
  # OPENNESS #
  tipi_O_open = two_sd(V162337),
  tipi_O_conv = two_sd((V162342-8)*-1),
  
  # CONSCIENTIOUSNESS 
  tipi_C_depe = two_sd(V162335),
  tipi_C_diso = two_sd((V162340-8)*-1),
  
  # EXTRAVERSION #
  tipi_E_extr = two_sd(V162333),
  tipi_E_rese = two_sd((V162338-8)*-1),
  
  # AGREEABLENESS #
  tipi_A_warm = two_sd(V162339),
  tipi_A_crit = two_sd((V162334-8)*-1),
  
  # NEUROTICISM #
  tipi_N_anxi = two_sd(V162336),
  tipi_N_calm = two_sd((V162341-8)*-1),
)

anes16_var <- anes16 %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)

## TIPI: Latin American Public Opinion Project #----
#Import#
lapop <- import(here::here("1_Data","1_Panel Datasets","lapop.csv"))

lapop <- lapop %>% mutate(
  dataset = "lapop",
  age = ifelse(q2 > 1 & q2 < 100, q2, NA),
  
  # tipi personality items 2 per dim, scale 1 disagree strongly - 7 agree strongly
  # OPENNESS #
  tipi_O_open = two_sd(per5),
  tipi_O_conv = two_sd((per10 - 8) * -1),
  
  # CONSCIENTIOUSNESS #
  tipi_C_depe = two_sd(per3),
  tipi_C_diso = two_sd((per8 - 8) * -1),
  
  # EXTRAVERSION #
  tipi_E_extr = two_sd(per1),
  tipi_E_rese = two_sd((per6 - 8) * -1),
  
  # AGREEABLENESS #
  tipi_A_warm = two_sd(per7),
  tipi_A_crit = two_sd((per2 - 8) * -1),
  
  # NEUROTICISM #
  tipi_N_anxi = two_sd(per4),
  tipi_N_calm = two_sd((per9 - 8) * -1)
)

lapop_var <- lapop %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)


## TIPI: New Zealand Election Study ##----
#Import#
nzes <- import(here::here("1_Data","1_Panel Datasets","NZES2014GeneralReleaseApril16.sav"))

nzes <- nzes %>%   
  mutate(
  dataset = "nzes",
  age = ifelse(dage > 1 & dage < 100, dage, NA),
  
  # tipi personality items 2 per dim, scale 1 AGREE strongly - 7 DISagree strongly
  # !!scale opposite then with the other TIPI surveys!!
  
  # OPENNESS #
  tipi_O_open = two_sd(8-dperscomplex),
  tipi_O_conv = two_sd(dpersconvent),
  
  # CONSCIENTIOUSNESS #
  tipi_C_depe = two_sd(8-dpersdepend),
  tipi_C_diso = two_sd(dperscareless),
  
  # EXTRAVERSION #
  tipi_E_extr = two_sd(8-dpersextra), 
  tipi_E_rese = two_sd(dpersreserved),
  
  # AGREEABLENESS #
  tipi_A_warm = two_sd(8-dperswarm),
  tipi_A_crit = two_sd(dperscritical),
  
  # NEUROTICISM #
  tipi_N_anxi = two_sd(8-dpersanxious),
  tipi_N_calm = two_sd(dperscalm))

nzes_var <- nzes %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)

## TIPI: Canadian Election Study ##----
#Import#
ces <- import(here::here("1_Data","1_Panel Datasets","CES2015_Combined_Stata14.dta"))

ces <- ces %>% 
  mutate_at(vars(p_psych1:p_psych10), 
            function(x) case_when(x == 1000 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  mutate(
    dataset = "ces",
    age = ifelse((2015-age) > 1 & (2015-age) < 100,(2015-age),NA),
    
    # tipi personality items 2 per dim, scale 1 disagree strongly - 7 agree strongly
    # OPENNESS #
    tipi_O_open = two_sd(p_psych5),
    tipi_O_conv = two_sd(8-p_psych10),
    
    # CONSCIENTIOUSNESS #
    tipi_C_depe = two_sd(p_psych3),
    tipi_C_diso = two_sd(8-p_psych8),
    
    # EXTRAVERSION #
    tipi_E_extr = two_sd(p_psych1),
    tipi_E_rese = two_sd(8-p_psych6),
    
    # AGREEABLENESS #
    tipi_A_warm = two_sd(p_psych7),
    tipi_A_crit = two_sd(8-p_psych2),
    
    # NEUROTICISM #
    tipi_N_anxi = two_sd(p_psych4),
    tipi_N_calm = two_sd(8-p_psych9)
  )

ces_var <- ces %>% 
  select(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm) %>%
  drop_na(dataset, age, tipi_O_open, tipi_O_conv, tipi_C_depe, tipi_C_diso, tipi_E_extr, tipi_E_rese, tipi_A_warm, tipi_A_crit, tipi_N_anxi, tipi_N_calm)

## TIPI MERGE ## ----

tipi_items <- rbind(ces_var, nzes_var, lapop_var, anes1012_var, anes12_var, anes16_var)

write_csv(tipi_items, here::here("1_Data","tipi_items.csv"))


## BFI-10: Swiss Household Panel #----
#Import#
shp <- import(here::here("1_Data","1_Panel Datasets","shp09_p_user.dta"))

shp <- shp %>% mutate(
  dataset = "shp",
  age = ifelse(age09 > 1 & age09 < 100, age09, NA),
  
  # OPENNESS #
  bfi_O_imag = ifelse(p09c64 >= 0 & p09c64 <= 10, p09c64, NA),
  bfi_O_arti = ifelse(p09c69 >= 0 & p09c69 <= 10, p09c69, NA),
  
  # CONSCIENTIOUSNESS #
  bfi_C_lazy = ifelse(p09c67 >= 0 & p09c67 <= 10, (p09c67 - 10)*-1, NA),
  bfi_C_thor = ifelse(p09c62 >= 0 & p09c62 <= 10, p09c62, NA),
  
  # EXTRAVERSION #
  bfi_E_rese = ifelse(p09c60 >= 0 & p09c60 <= 10, (p09c60 - 10)*-1, NA),
  bfi_E_soci = ifelse(p09c65 >= 0 & p09c65 <= 10, p09c65, NA),
  
  # AGREEABLENESS #
  bfi_A_faul = ifelse(p09c66 >= 0 & p09c66 <= 10, (p09c66 - 10)*-1, NA),
  bfi_A_trus = ifelse(p09c61 >= 0 & p09c61 <= 10, p09c61, NA),
  
  # NEUROTICISM #
  bfi_N_rela = ifelse(p09c63 >= 0 & p09c63 <= 10, (p09c63 - 10)*-1, NA),
  bfi_N_nerv = ifelse(p09c68 >= 0 & p09c68 <= 10, p09c68, NA)
)

bfi10_items <- shp %>% 
  select(dataset, age, bfi_O_imag, bfi_O_arti, bfi_C_lazy, bfi_C_thor, bfi_E_rese, bfi_E_soci, bfi_A_faul, bfi_A_trus, bfi_N_rela, bfi_N_nerv) %>%
  drop_na(dataset, age, bfi_O_imag, bfi_O_arti, bfi_C_lazy, bfi_C_thor, bfi_E_rese, bfi_E_soci, bfi_A_faul, bfi_A_trus, bfi_N_rela, bfi_N_nerv)

write_csv(bfi10_items, here::here("1_Data","bfi10_items.csv"))


## IPIP: Longitudinal Internet Studies in the Social Sciences #----
#Import#
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

liss <- liss %>% mutate(
  dataset = "liss",
  age = ifelse(leeftijd > 1 & leeftijd < 100, leeftijd, NA),
  
  ## OPENNESS ##
  # Scale: 1 very inaccurate,2 moderately inaccurate,
  #3 neither inaccurate nor accurate, 4 moderately accurate,
  # 5 very accurate 
  ipip_O_vivimag = cp08a034,
  ipip_O_ricvoca = cp08a024,
  ipip_O_excidea = cp08a044,
  ipip_O_quiunde = cp08a054,
  ipip_O_difword = cp08a059,
  ipip_O_reflthi = cp08a064,
  ipip_O_fulidea = cp08a069,
  ipip_O_difunde = (cp08a029 - 6) * -1,
  ipip_O_intidea = (cp08a039 - 6) * -1,
  ipip_O_notimag = (cp08a049 - 6) * -1,
  
  # CONSCIENTIOUSNESS #
  ipip_C_alwprep = cp08a022,
  ipip_C_leabelo = (cp08a027 -6) * -1,
  ipip_C_attdeta = cp08a032,
  ipip_C_makmess = (cp08a037 -6) * -1,
  ipip_C_chodone = cp08a042,
  ipip_C_forgbac = (cp08a047 -6) * -1,
  ipip_C_likorde = cp08a052,
  ipip_C_shirdut = (cp08a057 -6) * -1,
  ipip_C_folsche = cp08a062,
  ipip_C_exactwo = cp08a067,
  
  # EXTRAVERSION #
  ipip_E_lifpart = cp08a020,
  ipip_E_nottalk = (cp08a025 - 6) * -1,
  ipip_E_comfaro = cp08a030,
  ipip_E_inbackg = (cp08a035 - 6) * -1,
  ipip_E_talkdif = cp08a050,
  ipip_E_noatten = (cp08a055 - 6) * -1,
  ipip_E_cenatte = cp08a060,
  ipip_E_quistra = (cp08a065 - 6) * -1,
  ipip_E_startco = cp08a040,
  ipip_E_littsay = (cp08a045 - 6) * -1,
  
  # AGREEABLENESS #
  ipip_A_littcon = (cp08a021 - 6) *-1,
  ipip_A_intpeop = cp08a026,
  ipip_A_peopeas = cp08a066,
  ipip_A_insupeo = (cp08a031 - 6) *-1,
  ipip_A_symfeel = cp08a036,
  ipip_A_feelemo = cp08a061,
  ipip_A_softhea = cp08a046,
  ipip_A_nointot = (cp08a051 - 6) *-1,
  ipip_A_nointpr = (cp08a041 - 6) *-1,
  ipip_A_taketim = cp08a056,
  
  # NEUROTICISM #
  ipip_N_irrieas = cp08a063,
  ipip_N_seldblu = (cp08a038 - 6)*-1,
  ipip_N_easdisr = cp08a043,
  ipip_N_upseasy = cp08a048,
  ipip_N_streasy = cp08a023,
  ipip_N_oftblue = cp08a068,
  ipip_N_relamot = (cp08a028-6)*-1,
  ipip_N_worryth = cp08a033,
  ipip_N_chamood = cp08a053,
  ipip_N_moodswi = cp08a058
)

ipip_items <- liss %>% 
  select(dataset, age, ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag, 
         ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone, ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo, 
         ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif, ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay, 
         ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel, ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim,
         ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy, ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi) %>% 
  drop_na(dataset, age, ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag, 
          ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone, ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo, 
          ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif, ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay, 
          ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel, ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim,
          ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy, ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

write_csv(ipip_items, here::here("1_Data","ipip_items.csv"))


## BFI-S: Swiss Election Study ##----
sels <- import(here::here("1_Data","1_Panel Datasets", "828_Selects2015_PanelRCS_Data_v1.1.dta"))

sels <- sels %>% mutate(
  dataset = "sels",
  age2 = ifelse(age > 1 & age < 100, age, NA),
  
  # OPENNESS #
  bfis_O_newidea = ifelse(W3_f15770d >= 0 & W3_f15770d <= 10, W3_f15770d, NA),
  bfis_O_apprart = ifelse(W3_f15771a >= 0 & W3_f15771a <= 10, W3_f15771a, NA),
  bfis_O_highima = ifelse(W3_f15771f >= 0 & W3_f15771f <= 10, W3_f15771f, NA),
  
  # CONSCIENTIOUSNESS #
  bfis_C_worktho = ifelse(W3_f15770a >= 0 & W3_f15770a <= 10, W3_f15770a, NA),
  bfis_C_taskeff = ifelse(W3_f15771c >= 0 & W3_f15771c <= 10, W3_f15771c, NA),
  bfis_C_ratlazy = ifelse(W3_f15770g >= 0 & W3_f15770g <= 10, 10-W3_f15770g, NA),
  
  # EXTRAVERSION #
  bfis_E_talkati = ifelse(W3_f15770b >= 0 & W3_f15770b <= 10, W3_f15770b, NA),
  bfis_E_sociabl = ifelse(W3_f15770h >= 0 & W3_f15770h <= 10, W3_f15770h, NA),
  bfis_E_reserve = ifelse(W3_f15771d >= 0 & W3_f15771d <= 10, 10-W3_f15771d, NA),
  
  # AGREEABLENESS #
  bfis_A_harshwo = ifelse(W3_f15770c >= 0 & W3_f15770c <= 10, 10-W3_f15770c, NA),
  bfis_A_ablefor = ifelse(W3_f15770f >= 0 & W3_f15770f <= 10, W3_f15770f, NA),
  bfis_A_conside = ifelse(W3_f15771e >= 0 & W3_f15771e <= 10, W3_f15771e, NA),
  
  # NEUROTICISM #
  bfis_N_worries = ifelse(W3_f15770e >= 0 & W3_f15770e <= 10, W3_f15770e, NA),
  bfis_N_nervous = ifelse(W3_f15771b >= 0 & W3_f15771b <= 10, W3_f15771b, NA),
  bfis_N_relaxed = ifelse(W3_f15771g >= 0 & W3_f15771g <= 10, 10 - W3_f15771g, NA)
)

bfiS_items <- sels %>% 
  select(
    age, dataset, bfis_O_newidea, bfis_O_apprart, bfis_O_highima, bfis_C_worktho, bfis_C_taskeff, bfis_C_ratlazy, bfis_E_talkati, bfis_E_sociabl, bfis_E_reserve, 
    bfis_A_harshwo, bfis_A_ablefor, bfis_A_conside, bfis_N_worries, bfis_N_nervous, bfis_N_relaxed
  ) %>% 
  drop_na(age, dataset, bfis_O_newidea, bfis_O_apprart, bfis_O_highima, bfis_C_worktho, bfis_C_taskeff, bfis_C_ratlazy, bfis_E_talkati, bfis_E_sociabl, bfis_E_reserve, 
          bfis_A_harshwo, bfis_A_ablefor, bfis_A_conside, bfis_N_worries, bfis_N_nervous, bfis_N_relaxed)

write_csv(bfiS_items, here::here("1_Data", "bfiS_items.csv"))

