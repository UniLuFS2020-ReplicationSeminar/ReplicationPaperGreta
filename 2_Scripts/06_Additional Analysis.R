library(psy)
library(stargazer)
library(grid)
library(gridExtra)
library(scales)
library(psych)
library(xtable)
library(magrittr)
library(tidyverse)
library(rio)
library(foreign)
library(sjmisc)
library(sjlabelled)
library(stringr)

maindata <- import(here::here("1_Data","personalitypolitics.csv"))

## Creating New Datasets for Analysis ## ----

# Set variable names for Big Five traits
trait.names <- c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism")
trait.Names <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")

anes1012 <- maindata %>% filter(dataset == "anes1012")
anes12 <- maindata %>% filter(dataset == "anes12")
anes16 <- maindata %>% filter(dataset == "anes16")
bes <- maindata %>% filter(dataset == "bes")
shp <- maindata %>% filter(dataset == "shp")
liss <- maindata %>% filter(dataset == "liss")
lapop <- maindata %>% filter(dataset == "lapop")
sels <- maindata %>% filter(dataset == "sels")
nzes <- maindata %>% filter(dataset == "nzes")
ces <- maindata %>% filter(dataset == "ces")

# Creating Results # Conditional Coefficients Under 30 & Low Education----
# Prepping dataset for coeffs #
outcome <- 9
datasets <- 10

add_df <- data.frame(
  outcome = rep(c("ideology", "stfdem", "polintr", "poleff", "involvement", "polpar", "media", "poltr", "knowledge"),5*datasets),
  trait = rep(trait.Names, outcome * datasets),
  dataset = c(rep(c("anes1012"), outcome * 5),
              rep(c("anes12"), outcome * 5), 
              rep(c("anes16"), outcome * 5), 
              rep(c("bes"), outcome * 5), 
              rep(c("liss"), outcome * 5), 
              rep(c("shp"), outcome * 5),
              rep(c("lapop"), outcome * 5),
              rep(c("sels"), outcome * 5),
              rep(c("nzes"), outcome * 5),
              rep(c("ces"), outcome * 5)),
  est = NA,
  se = NA
)

add_df <- rbind(add_df, add_df)
add_df$mod <- c(rep(c("under30"), outcome * datasets * 5), rep(c("lowEdu"), outcome * datasets * 5))

## Calculating Moderation Coeffs - Under 30##
# ANES 1012 - Under 30 Moderator #
for (i in c("ideology", "polintr", "poleff", "involvement", "polpar", "media", "knowledge")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes1012)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes1012)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes1012)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes1012)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes1012)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes1012)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes1012)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes1012)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes1012)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes1012)))["under30:neuroticism","Std. Error"]
}

# ANES 12 - Under 30 Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes12)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes12)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes12)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes12)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes12)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes12)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes12)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes12)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes12)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes12)))["under30:neuroticism","Std. Error"]
}

# ANES 16 - Under 30 Moderator #
for (i in c("ideology", "poltr", "polpar", "involvement", "polintr", "media", "poleff", "stfdem", "knowledge")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes16)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = anes16)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes16)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = anes16)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes16)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = anes16)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes16)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = anes16)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes16)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = anes16)))["under30:neuroticism","Std. Error"]
}

# LISS - Under 30 Moderator #
for (i in c("stfdem", "poltr", "ideology", "involvement", "polintr", "media", "poleff", "polpar")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = liss)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = liss)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = liss)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = liss)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = liss)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = liss)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = liss)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = liss)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = liss)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = liss)))["under30:neuroticism","Std. Error"]
}

# BES - Under 30 Moderator #
for (i in unique(add_df$outcome[add_df$dataset == "bes"])) {
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = bes)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = bes)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = bes)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = bes)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = bes)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = bes)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = bes)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = bes)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = bes)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = bes)))["under30:neuroticism","Std. Error"]
}

# SHP - Under 30 Moderator #
for (i in c("stfdem", "polintr", "ideology", "poltr", "polpar")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = shp)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = shp)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = shp)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = shp)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = shp)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = shp)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = shp)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = shp)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = shp)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = shp)))["under30:neuroticism","Std. Error"]
}

# LAPOP - Under 30 Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*openness")), data = lapop)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*openness")), data = lapop)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*conscientiousness")), data = lapop)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*conscientiousness")), data = lapop)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*extraversion")), data = lapop)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*extraversion")), data = lapop)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*agreeableness")), data = lapop)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*agreeableness")), data = lapop)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*neuroticism")), data = lapop)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + under30*neuroticism")), data = lapop)))["under30:neuroticism","Std. Error"]
}

# SELECTs - Under 30 Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = sels)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = sels)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = sels)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = sels)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = sels)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = sels)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = sels)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = sels)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = sels)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = sels)))["under30:neuroticism","Std. Error"]
}

# NZES - Under 30 Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = nzes)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = nzes)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = nzes)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = nzes)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = nzes)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = nzes)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = nzes)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = nzes)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = nzes)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = nzes)))["under30:neuroticism","Std. Error"]
}

# CES - Under 30 Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = ces)))["under30:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Openness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*openness")), data = ces)))["under30:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = ces)))["under30:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Conscientiousness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*conscientiousness")), data = ces)))["under30:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = ces)))["under30:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Extraversion" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*extraversion")), data = ces)))["under30:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = ces)))["under30:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Agreeableness" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*agreeableness")), data = ces)))["under30:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = ces)))["under30:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Neuroticism" & add_df$mod == "under30"] <- coef(summary(lm(as.formula(paste(i,"~ under30*neuroticism")), data = ces)))["under30:neuroticism","Std. Error"]
}

## Low Education Moderator ----
# ANES 1012 - Low Education Moderator #
for (i in c("ideology", "polintr", "poleff", "involvement", "polpar", "media", "knowledge")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes1012)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes1012)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes1012)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes1012)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes1012)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes1012)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes1012)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes1012)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes1012)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes1012" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes1012)))["lowEdu:neuroticism","Std. Error"]
}

# ANES 12 - Low Education Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes12)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes12)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes12)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes12)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes12)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes12)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes12)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes12)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes12)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes12" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes12)))["lowEdu:neuroticism","Std. Error"]
}

# ANES 16 - Low Education Moderator #
for (i in c("ideology", "poltr", "polpar", "involvement", "polintr", "media", "poleff", "stfdem", "knowledge")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes16)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = anes16)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes16)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = anes16)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes16)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = anes16)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes16)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = anes16)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes16)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "anes16" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = anes16)))["lowEdu:neuroticism","Std. Error"]
}

# LISS - Low Education Moderator #
for (i in c("stfdem", "poltr", "ideology", "involvement", "polintr", "media", "poleff", "polpar")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = liss)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = liss)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = liss)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = liss)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = liss)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = liss)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = liss)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = liss)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = liss)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "liss" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = liss)))["lowEdu:neuroticism","Std. Error"]
}

# BES - Low Education Moderator #
for (i in unique(add_df$outcome[add_df$dataset == "bes"])) {
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = bes)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = bes)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = bes)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = bes)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = bes)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = bes)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = bes)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = bes)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = bes)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "bes" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = bes)))["lowEdu:neuroticism","Std. Error"]
}

# SHP - Low Education Moderator #
for (i in c("stfdem", "polintr", "ideology", "poltr", "polpar")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = shp)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = shp)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = shp)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = shp)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = shp)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = shp)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = shp)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = shp)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = shp)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "shp" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = shp)))["lowEdu:neuroticism","Std. Error"]
}

# LAPOP - Low Education Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*openness")), data = lapop)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*openness")), data = lapop)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*conscientiousness")), data = lapop)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*conscientiousness")), data = lapop)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*extraversion")), data = lapop)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*extraversion")), data = lapop)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*agreeableness")), data = lapop)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*agreeableness")), data = lapop)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*neuroticism")), data = lapop)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "lapop" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + lowEdu*neuroticism")), data = lapop)))["lowEdu:neuroticism","Std. Error"]
}

# SELECTs - Low Education Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = sels)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = sels)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = sels)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = sels)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = sels)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = sels)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = sels)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = sels)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = sels)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "sels" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = sels)))["lowEdu:neuroticism","Std. Error"]
}

# NZES - Low Education Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = nzes)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = nzes)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = nzes)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = nzes)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = nzes)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = nzes)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = nzes)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = nzes)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = nzes)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "nzes" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = nzes)))["lowEdu:neuroticism","Std. Error"]
}

# CES - Low Education Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = ces)))["lowEdu:openness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Openness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*openness")), data = ces)))["lowEdu:openness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = ces)))["lowEdu:conscientiousness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Conscientiousness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*conscientiousness")), data = ces)))["lowEdu:conscientiousness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = ces)))["lowEdu:extraversion","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Extraversion" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*extraversion")), data = ces)))["lowEdu:extraversion","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = ces)))["lowEdu:agreeableness","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Agreeableness" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*agreeableness")), data = ces)))["lowEdu:agreeableness","Std. Error"]
  add_df$est[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = ces)))["lowEdu:neuroticism","Estimate"]
  add_df$se[add_df$outcome == i & add_df$dataset == "ces" & add_df$trait == "Neuroticism" & add_df$mod == "lowEdu"] <- coef(summary(lm(as.formula(paste(i,"~ lowEdu*neuroticism")), data = ces)))["lowEdu:neuroticism","Std. Error"]
}

# Renaming for Clarity #
add_df <- add_df %>%
  mutate(
    Data = case_when(
      dataset == "anes1012" ~ "ANES 2010-12",
      dataset == "anes12" ~ "ANES 2012",
      dataset == "anes16" ~ "ANES 2016",
      dataset == "liss" ~ "LISS",
      dataset == "bes" ~ "BES",
      dataset == "shp" ~ "SHP",
      dataset == "lapop" ~ "LAPOP",
      dataset == "sels" ~ "SELECTS",
      dataset == "nzes" ~ "NZES",
      dataset == "ces" ~ "CES",
      TRUE  ~  NA_character_),
    name = case_when(
      outcome == "ideology" ~ "Left-right ideology",
      outcome == "stfdem" ~ "Sat. democracy",
      outcome == "polintr" ~ "Interest",
      outcome == "poleff" ~ "Efficacy",
      outcome == "involvement" ~ "Involvement",
      outcome == "polpar" ~ "Participation",
      outcome == "media" ~ "Media use",
      outcome == "poltr" ~ "Political trust",
      outcome == "knowledge" ~ "Knowledge",
      TRUE  ~  NA_character_)
  )

## Creating Median and Mean Values for Estimates
add_df$est_abs <- abs(add_df$est)
add_df %>% 
  group_by(Data, mod) %>%
  summarise(Median = median(est_abs, na.rm=TRUE), Mean = mean(est_abs, na.rm=TRUE)) %>%
  filter(!is.na(Median))

## Adding p value
add_df$pval <- 2 * pt(-abs(add_df$est / add_df$se), df = Inf)

add_df <- add_df %>%
  mutate(
    pval_text = case_when(
      pval < 0.01 ~ "p < 0.01",
      TRUE  ~  paste("p = ", as.character(round(pval, 2))))
  )

NROW(add_df[!is.na(add_df$est),])

NROW(add_df[!is.na(add_df$est) & add_df$pval < 0.05,])


## WRITE CSV add_df ----
write_csv(add_df, here::here("1_Data","extra_lm_coeffs_moderator.csv"))


## Visualisation of Interaction Effect ## ----
cor(ces$involvement, ces$openness)
lin1 <- lm(formula = involvement ~ openness, data = ces)
lin2 <- lm(formula = involvement ~ openness*student, data = ces)
summary(lin1)
summary(lin2)
stargazer(lin1, lin2, out=here::here("3_Figures", "results_openInvolvStudent.htm"))

## Low Education Moderator for pers Traits on 

## Creating figs Interaction Effekts LOW EDUCATION ----
fig_lowEdu <- add_df %>%
  filter(mod == "lowEdu" & !is.na(est)) %>%
  ggplot(aes(x = trait, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_jitter(height=0, width=0.15, size=3, aes(colour = Data, shape = ifelse(pval < 0.05, "Significant", "Insignificant"))) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "",
       y = "") +
  coord_flip()

fig_lowEdu %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig2-results-student.pdf"), plot = ., height=7, width=9) %T>%
  ggsave(here::here("3_Figures", "fig2-results-student.png"), plot = ., height=7, width=9)


fig_under30 <- add_df %>%
  filter(mod == "under30" & !is.na(est)) %>%
  ggplot(aes(x = trait, y = est, ymin=est-1.96*se, ymax=est+1.96*se)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_jitter(height=0, width=0.15, size=3, aes(colour = Data, shape = ifelse(pval < 0.05, "Significant", "Insignificant"))) +
  facet_wrap(~ name) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(palette="Paired") +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "",
       y = "") +
  coord_flip()

fig_under30 %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig-results-under30.pdf"), plot = ., height=7, width=9) %T>%
  ggsave(here::here("3_Figures", "fig-results-under30.png"), plot = ., height=7, width=9)


# P Value under 30
add_df_u30 <- add_df %>% filter(mod == "under30")

df_sign <- ifelse(add_df_u30$pval<0.05,1,0)
fig_all_pvals <- ggplot(add_df_u30, aes(x=pval, fill=ifelse(df_sign==1,"80 Interactions","325 Interactions"))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour="black", bins=20, binwidth=0.05,boundary=-0.5) +
  scale_y_continuous("", labels=percent) +
  scale_x_continuous(expression(paste(italic("p"), " value")), 
                     breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "0.05", "0.1", "0.25", "0.5", "0.75", "1"))

fig_all_pvals
fig_all_pvals %T>%
  print() %T>% 
  ggsave(here::here("3_Figures", "fig_u30-results-all_pvals.pdf"), plot = ., height=4, width=8) %T>%
  ggsave(here::here("3_Figures", "fig_u30-results-all_pvals.png"), plot = ., height=4, width=8)

NROW(add_df_u30[!is.na(add_df_u30$est),])
NROW(add_df_u30[!is.na(add_df_u30$est) & add_df_u30$pval < 0.05,])





## Agreeableness on Interest Moderates by Under30
# Renaming for Clarity #
nzes <- nzes %>%
  mutate(
    Data = case_when(
      dataset == "anes1012" ~ "ANES 2010-12",
      dataset == "anes12" ~ "ANES 2012",
      dataset == "anes16" ~ "ANES 2016",
      dataset == "liss" ~ "LISS",
      dataset == "bes" ~ "BES",
      dataset == "shp" ~ "SHP",
      dataset == "lapop" ~ "LAPOP",
      dataset == "sels" ~ "SELECTS",
      dataset == "nzes" ~ "NZES",
      dataset == "ces" ~ "CES",
      TRUE  ~  NA_character_),
    name = case_when(
      outcome == "ideology" ~ "Left-right ideology",
      outcome == "stfdem" ~ "Sat. democracy",
      outcome == "polintr" ~ "Interest",
      outcome == "poleff" ~ "Efficacy",
      outcome == "involvement" ~ "Involvement",
      outcome == "polpar" ~ "Participation",
      outcome == "media" ~ "Media use",
      outcome == "poltr" ~ "Political trust",
      outcome == "knowledge" ~ "Knowledge",
      TRUE  ~  NA_character_)
  )

cor(nzes$polintr, nzes$agreeableness, use="pairwise.complete.obs")
lin_nzes <- lm(formula = polintr ~ agreeableness, data = nzes)
lin_nzes_mod <- lm(formula = polintr ~ agreeableness*under30, data = nzes)
stargazer(lin_nzes,lin_nzes_mod, dep.var.labels = "Interest", out=here::here("3_Figures","results_agreInterUnder30.htm"))

cor(nzes$polintr, nzes$under30, use="pairwise.complete.obs")