#creating subset under and over 30# ----
ipip_items <- ipip_items %>% mutate(
  under30 = ifelse(age < 30, 1, 0))

ipip_item_under30 <- ipip_items %>% filter(
  under30 == 1)

ipip_item_over30 <- ipip_items %>% filter(
  under30 == 0)

#creating unidimensional datasetset# 
#over 30#
ipip_openness_over30 <- ipip_item_over30 %>% dplyr::select(
  ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
  ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag)

ipip_conscientiousness_over30 <- ipip_item_over30 %>% dplyr::select(
  ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
  ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo)

ipip_extraversion_over30 <- ipip_item_over30 %>% dplyr::select(
  ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
  ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay)

ipip_agreeableness_over30 <- ipip_item_over30 %>% dplyr::select(
  ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
  ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim)

ipip_neuroticism_over30 <- ipip_item_over30 %>% dplyr::select(
  ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
  ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

#under 30#
ipip_openness_under30 <- ipip_item_under30 %>% dplyr::select(
  ipip_O_vivimag, ipip_O_ricvoca, ipip_O_excidea, ipip_O_quiunde, ipip_O_difword, 
  ipip_O_reflthi, ipip_O_fulidea, ipip_O_difunde, ipip_O_intidea, ipip_O_notimag)

ipip_conscientiousness_under30 <- ipip_item_under30 %>% dplyr::select(
  ipip_C_alwprep, ipip_C_leabelo, ipip_C_attdeta, ipip_C_makmess, ipip_C_chodone,
  ipip_C_forgbac, ipip_C_likorde, ipip_C_shirdut, ipip_C_folsche, ipip_C_exactwo)

ipip_extraversion_under30 <- ipip_item_under30 %>% dplyr::select(
  ipip_E_lifpart, ipip_E_nottalk, ipip_E_comfaro, ipip_E_inbackg, ipip_E_talkdif,
  ipip_E_noatten, ipip_E_cenatte, ipip_E_quistra, ipip_E_startco, ipip_E_littsay)

ipip_agreeableness_under30 <- ipip_item_under30 %>% dplyr::select(
  ipip_A_littcon, ipip_A_intpeop, ipip_A_peopeas, ipip_A_insupeo, ipip_A_symfeel,
  ipip_A_feelemo, ipip_A_softhea, ipip_A_nointot, ipip_A_nointpr, ipip_A_taketim)

ipip_neuroticism_under30 <- ipip_item_under30 %>% dplyr::select(
  ipip_N_irrieas, ipip_N_seldblu, ipip_N_easdisr, ipip_N_upseasy, ipip_N_streasy,
  ipip_N_oftblue, ipip_N_relamot, ipip_N_worryth, ipip_N_chamood, ipip_N_moodswi)

# Checking means of different groups for every item #
frq(ipip_openness_over30$ipip_O_reflthi)
mean(ipip_openness_over30$ipip_O_reflthi)
frq(ipip_openness_under30$ipip_O_reflthi)
mean(ipip_openness_under30$ipip_O_reflthi)



# TESTING
a <- matrix(abs(rnorm(15,1,.3)), ncol=1)
d <- matrix(rnorm(15,0,.7),ncol=1)
itemtype <- rep('2PL', nrow(a))
N <- 1000
dataset1 <- simdata(a, d, N, itemtype)
dataset2 <- simdata(a, d, N, itemtype, mu = .1, sigma = matrix(1.5))
dat <- rbind(dataset1, dataset2)
group <- c(rep('D1', N), rep('D2', N))
models <- 'F1 = 1-15'


itemtype <- rep('2PL', nrow(a1))
N <- 1000
dataset1 <- simdata(a1, d1, N, itemtype)
dataset2 <- simdata(a2, d2, N, itemtype, mu = .1, sigma = matrix(1.5))
dat <- rbind(dataset1, dataset2)
group <- c(rep('D1', N), rep('D2', N))