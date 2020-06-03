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

outcome <- 9
datasets <- 10

df_direct <- data.frame(
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

## Calculation Estimated Regressioncoefficient "est" and Standarddeviation "se" ## ----
## Model: Personality traits impact on Political variables ## 
#Anes 2010 2012
for (i in c("ideology", "polintr", "poleff", "involvement", "polpar", "media", "knowledge")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes1012)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes1012)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes1012)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes1012)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes1012)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes1012)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes1012)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes1012)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes1012)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes1012" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes1012)))["neuroticism","Std. Error"]
}

## Checking Calculated Coefficients ##
model_OpenIdeology <- lm(formula = ideology ~ openness, data = anes1012)
summary(model_OpenIdeology)
df_direct

# Anes 2012
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes12)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes12)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes12)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes12)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes12)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes12)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes12)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes12)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes12)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes12" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes12)))["neuroticism","Std. Error"]
}

# Anes 2016
for (i in c("ideology", "poltr", "polpar", "involvement", "polintr", "media", "poleff", "stfdem", "knowledge")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes16)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = anes16)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes16)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = anes16)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes16)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = anes16)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes16)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = anes16)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes16)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "anes16" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = anes16)))["neuroticism","Std. Error"]
}

# LISS
for (i in c("stfdem", "poltr", "ideology", "involvement", "polintr", "media", "poleff", "polpar")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = liss)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = liss)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = liss)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = liss)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = liss)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = liss)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = liss)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = liss)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = liss)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "liss" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = liss)))["neuroticism","Std. Error"]
}

# BES
for (i in unique(df_direct$outcome[df_direct$dataset == "bes"])) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = bes)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = bes)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = bes)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = bes)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = bes)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = bes)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = bes)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = bes)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = bes)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "bes" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = bes)))["neuroticism","Std. Error"]
}

# SHP
for (i in c("stfdem", "polintr", "ideology", "poltr", "polpar")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = shp)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = shp)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = shp)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = shp)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = shp)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = shp)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = shp)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = shp)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = shp)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "shp" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = shp)))["neuroticism","Std. Error"]
}

# LAPOP
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + openness")), data = lapop)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + openness")), data = lapop)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + conscientiousness")), data = lapop)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + conscientiousness")), data = lapop)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + extraversion")), data = lapop)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + extraversion")), data = lapop)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + agreeableness")), data = lapop)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + agreeableness")), data = lapop)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + neuroticism")), data = lapop)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "lapop" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + neuroticism")), data = lapop)))["neuroticism","Std. Error"]
}

# SELS
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = sels)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = sels)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = sels)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = sels)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = sels)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = sels)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = sels)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = sels)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = sels)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "sels" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = sels)))["neuroticism","Std. Error"]
}

# NZES
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = nzes)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = nzes)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = nzes)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = nzes)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = nzes)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = nzes)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = nzes)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = nzes)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = nzes)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "nzes" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = nzes)))["neuroticism","Std. Error"]
}

# CES
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = ces)))["openness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Openness"] <- coef(summary(lm(as.formula(paste(i,"~ openness")), data = ces)))["openness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = ces)))["conscientiousness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Conscientiousness"] <- coef(summary(lm(as.formula(paste(i,"~ conscientiousness")), data = ces)))["conscientiousness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = ces)))["extraversion","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Extraversion"] <- coef(summary(lm(as.formula(paste(i,"~ extraversion")), data = ces)))["extraversion","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = ces)))["agreeableness","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Agreeableness"] <- coef(summary(lm(as.formula(paste(i,"~ agreeableness")), data = ces)))["agreeableness","Std. Error"]
  df_direct$est[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = ces)))["neuroticism","Estimate"]
  df_direct$se[df_direct$outcome == i & df_direct$dataset == "ces" & df_direct$trait == "Neuroticism"] <- coef(summary(lm(as.formula(paste(i,"~ neuroticism")), data = ces)))["neuroticism","Std. Error"]
}

## Renaming Variables for clarity ## ----
df_direct <- df_direct %>%
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

# Adding p value ----
df_direct$pval <- 2 * pt(-abs(df_direct$est / df_direct$se), df = Inf)

# Rounding P Value and Testing for 1% Sign Level
df_direct <- df_direct %>%
  mutate(
    pval_text = case_when(
      pval < 0.01 ~ "p < 0.01",
      TRUE  ~  paste("p = ", as.character(round(pval, 2)))))

# Checking correctness of calculated p value
summary(model_OpenIdeology)
head(df_direct)

model_InteresExtra <- lm(formula = polintr ~ extraversion, data = anes1012)
summary(model_InteresExtra)
head(df_direct)

##Write CSV df_direct ----
write_csv(df_direct, here::here("1_Data","lm_coeffs.csv"))


# Creating Results # Conditional Coefficients INTERNET + STUDENTS----
# Prepping dataset for coeffs #
outcome <- 9
datasets <- 10

df <- data.frame(
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
df <- rbind(df, df)
df$mod <- c(rep(c("student"), outcome * datasets * 5), rep(c("internet"), outcome * datasets * 5))

# ANES 1012 - Internet Moderator #
for (i in c("ideology", "polintr", "poleff", "involvement", "polpar", "media", "knowledge")) {
  df$est[df$outcome == i & df$dataset == "anes1012" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes1012)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes1012" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes1012)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes1012" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes1012)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes1012" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes1012)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes1012" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes1012)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes1012" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes1012)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes1012" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes1012)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes1012" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes1012)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes1012" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes1012)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes1012" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes1012)))["internet:neuroticism","Std. Error"]
}

# ANES 12 - Internet Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes12)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes12)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes12)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes12)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes12)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes12)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes12)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes12)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes12)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes12)))["internet:neuroticism","Std. Error"]
}

# ANES 16 - Internet Moderator #
for (i in c("ideology", "poltr", "polpar", "involvement", "polintr", "media", "poleff", "stfdem", "knowledge")) {
  df$est[df$outcome == i & df$dataset == "anes16" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes16)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes16" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = anes16)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes16" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes16)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes16" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = anes16)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes16" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes16)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes16" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = anes16)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes16" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes16)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes16" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = anes16)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes16" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes16)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes16" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = anes16)))["internet:neuroticism","Std. Error"]
}

# LISS - Internet Moderator #
for (i in c("stfdem", "poltr", "ideology", "involvement", "polintr", "media", "poleff", "polpar")) {
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = liss)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = liss)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = liss)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = liss)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = liss)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = liss)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = liss)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = liss)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = liss)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = liss)))["internet:neuroticism","Std. Error"]
}

# LAPOP - Internet Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*openness")), data = lapop)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*openness")), data = lapop)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*conscientiousness")), data = lapop)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*conscientiousness")), data = lapop)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*extraversion")), data = lapop)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*extraversion")), data = lapop)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*agreeableness")), data = lapop)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*agreeableness")), data = lapop)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*neuroticism")), data = lapop)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + internet*neuroticism")), data = lapop)))["internet:neuroticism","Std. Error"]
}

# NZES - Internet Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "nzes" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = nzes)))["internet:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "nzes" & df$trait == "Openness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*openness")), data = nzes)))["internet:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "nzes" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = nzes)))["internet:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "nzes" & df$trait == "Conscientiousness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*conscientiousness")), data = nzes)))["internet:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "nzes" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = nzes)))["internet:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "nzes" & df$trait == "Extraversion" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*extraversion")), data = nzes)))["internet:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "nzes" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = nzes)))["internet:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "nzes" & df$trait == "Agreeableness" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*agreeableness")), data = nzes)))["internet:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "nzes" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = nzes)))["internet:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "nzes" & df$trait == "Neuroticism" & df$mod == "internet"] <- coef(summary(lm(as.formula(paste(i,"~ internet*neuroticism")), data = nzes)))["internet:neuroticism","Std. Error"]
}

# ANES 12 - Student Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = anes12)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = anes12)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = anes12)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = anes12)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = anes12)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = anes12)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = anes12)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = anes12)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "anes12" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = anes12)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "anes12" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = anes12)))["student:neuroticism","Std. Error"]
}

# LISS - Student Moderator #
for (i in c("stfdem", "poltr", "ideology", "involvement", "polintr", "media", "poleff", "polpar")) {
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = liss)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = liss)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = liss)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = liss)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = liss)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = liss)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = liss)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = liss)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "liss" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = liss)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "liss" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = liss)))["student:neuroticism","Std. Error"]
}

# BES - Student Moderator #
for (i in unique(df$outcome[df$dataset == "bes"])) {
  df$est[df$outcome == i & df$dataset == "bes" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = bes)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "bes" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = bes)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "bes" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = bes)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "bes" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = bes)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "bes" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = bes)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "bes" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = bes)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "bes" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = bes)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "bes" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = bes)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "bes" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = bes)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "bes" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = bes)))["student:neuroticism","Std. Error"]
}

# SHP - Student Moderator #
for (i in c("stfdem", "polintr", "ideology", "poltr", "polpar")) {
  df$est[df$outcome == i & df$dataset == "shp" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = shp)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "shp" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = shp)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "shp" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = shp)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "shp" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = shp)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "shp" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = shp)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "shp" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = shp)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "shp" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = shp)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "shp" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = shp)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "shp" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = shp)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "shp" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = shp)))["student:neuroticism","Std. Error"]
}

# LAPOP - Student Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*openness")), data = lapop)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*openness")), data = lapop)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*conscientiousness")), data = lapop)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*conscientiousness")), data = lapop)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*extraversion")), data = lapop)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*extraversion")), data = lapop)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*agreeableness")), data = lapop)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*agreeableness")), data = lapop)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "lapop" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*neuroticism")), data = lapop)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "lapop" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ factor(cntry) + student*neuroticism")), data = lapop)))["student:neuroticism","Std. Error"]
}

# SELECTs - Student Moderator #
for (i in c("ideology", "media", "poltr", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "sels" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = sels)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "sels" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = sels)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "sels" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = sels)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "sels" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = sels)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "sels" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = sels)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "sels" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = sels)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "sels" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = sels)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "sels" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = sels)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "sels" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = sels)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "sels" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = sels)))["student:neuroticism","Std. Error"]
}

# CES - Student Moderator #
for (i in c("ideology", "media", "poleff", "polpar", "involvement", "knowledge", "stfdem", "polintr")) {
  df$est[df$outcome == i & df$dataset == "ces" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = ces)))["student:openness","Estimate"]
  df$se[df$outcome == i & df$dataset == "ces" & df$trait == "Openness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*openness")), data = ces)))["student:openness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "ces" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = ces)))["student:conscientiousness","Estimate"]
  df$se[df$outcome == i & df$dataset == "ces" & df$trait == "Conscientiousness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*conscientiousness")), data = ces)))["student:conscientiousness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "ces" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = ces)))["student:extraversion","Estimate"]
  df$se[df$outcome == i & df$dataset == "ces" & df$trait == "Extraversion" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*extraversion")), data = ces)))["student:extraversion","Std. Error"]
  df$est[df$outcome == i & df$dataset == "ces" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = ces)))["student:agreeableness","Estimate"]
  df$se[df$outcome == i & df$dataset == "ces" & df$trait == "Agreeableness" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*agreeableness")), data = ces)))["student:agreeableness","Std. Error"]
  df$est[df$outcome == i & df$dataset == "ces" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = ces)))["student:neuroticism","Estimate"]
  df$se[df$outcome == i & df$dataset == "ces" & df$trait == "Neuroticism" & df$mod == "student"] <- coef(summary(lm(as.formula(paste(i,"~ student*neuroticism")), data = ces)))["student:neuroticism","Std. Error"]
}

# Renaming for Clarity #
df <- df %>%
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
## P-Value for differences in Interaction test
df$pval <- 2 * pt(-abs(df$est / df$se), df = Inf)

df <- df %>%
  mutate(
    pval_text = case_when(
      pval < 0.01 ~ "p < 0.01",
      TRUE  ~  paste("p = ", as.character(round(pval, 2))))
  )

## Creating Median and Mean Values for Estimates
df$est_abs <- abs(df$est)
df %>% 
  group_by(Data, mod) %>%
  summarise(Median = median(est_abs, na.rm=TRUE), Mean = mean(est_abs, na.rm=TRUE)) %>%
  filter(!is.na(Median))


## WRITE CSV df ----
write_csv(df, here::here("1_Data","lm_coeffs_moderator.csv"))


## CORRELATIONS - EACH TRAIT WITH EACH POLITICAL VARIABLE ----
# Meta-analytical estimates

# COR: Ideology and Openness ----
cors <- c(
  cor(shp$ideology, shp$openness, use = "pairwise.complete.obs"),
  cor(sels$ideology, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$ideology, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$ideology, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$ideology, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$ideology, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$ideology, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$ideology, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$ideology, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$ideology, anes1012$openness, use = "pairwise.complete.obs")
)
# Fisher's Z Transformation to make correlation closer to evenly distributed for further analysis
zs <- fisherz(cors)
# Getting number of valid entries IDEOLOGY & OPENNESS - all datasets
# - 7 (n - 3)
n3 <- c(
  NROW(shp[!is.na(shp$ideology) & !is.na(shp$openness),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$openness),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$openness),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$openness),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$openness),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$openness),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$openness),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$openness),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$openness),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$openness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
ideology_O <- fisherz2r(sum(n3z)/sum(n3))

ideology_n <- sum(c(
  NROW(shp[!is.na(shp$ideology),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$openness),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$openness),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$openness),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$openness),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$openness),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$openness),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$openness),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$openness),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$openness),])
))
# Calculating Confidence intervall 
round(r.con(ideology_O, n = ideology_n), 3)

# COR: Ideology and Conscientiousness ----
cors <- c(
  cor(shp$ideology, shp$conscientiousness, use = "pairwise.complete.obs"),
  cor(sels$ideology, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$ideology, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$ideology, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$ideology, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$ideology, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$ideology, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$ideology, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$ideology, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$ideology, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$ideology) & !is.na(shp$conscientiousness),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(ideology_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(ideology_C, n = ideology_n), 3)

# COR: Ideology and Extraversion ----
cors <- c(
  cor(shp$ideology, shp$extraversion, use = "pairwise.complete.obs"),
  cor(sels$ideology, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$ideology, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$ideology, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$ideology, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$ideology, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$ideology, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$ideology, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$ideology, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$ideology, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$ideology) & !is.na(shp$extraversion),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(ideology_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(ideology_E, n = ideology_n), 3)

# COR: Ideology and Agreeableness ----
cors <- c(
  cor(shp$ideology, shp$agreeableness, use = "pairwise.complete.obs"),
  cor(sels$ideology, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$ideology, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$ideology, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$ideology, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$ideology, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$ideology, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$ideology, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$ideology, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$ideology, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$ideology) & !is.na(shp$agreeableness),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(ideology_A <- fisherz2r(sum(n3z)/sum(n3)))
round(r.con(ideology_A, n = ideology_n), 3)

# COR: Ideology and Neuroticism ----
cors <- c(
  cor(shp$ideology, shp$neuroticism, use = "pairwise.complete.obs"),
  cor(sels$ideology, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$ideology, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$ideology, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$ideology, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$ideology, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$ideology, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$ideology, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$ideology, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$ideology, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$ideology) & !is.na(shp$neuroticism),]),
  NROW(sels[!is.na(sels$ideology) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$ideology) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$ideology) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$ideology) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$ideology) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$ideology) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$ideology) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$ideology) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$ideology) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)

(ideology_N <- fisherz2r(sum(n3z)/sum(n3)))
round(r.con(ideology_N, n = ideology_n), 3)


# COR: Efficacy and Openness ----
cors <- c(
  cor(sels$poleff, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$poleff, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$poleff, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$poleff, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$poleff, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$poleff, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$poleff, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$poleff, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$poleff, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$poleff),]),
  NROW(nzes[!is.na(nzes$poleff),]),
  NROW(liss[!is.na(liss$poleff),]),
  NROW(lapop[!is.na(lapop$poleff),]),
  NROW(ces[!is.na(ces$poleff),]),
  NROW(bes[!is.na(bes$poleff),]),
  NROW(anes16[!is.na(anes16$poleff),]),
  NROW(anes12[!is.na(anes12$poleff),]),
  NROW(anes1012[!is.na(anes1012$poleff),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poleff_O <- fisherz2r(sum(n3z)/sum(n3)))

poleff_n <- sum(c(
  NROW(sels[!is.na(sels$poleff),]),
  NROW(nzes[!is.na(nzes$poleff),]),
  NROW(liss[!is.na(liss$poleff),]),
  NROW(lapop[!is.na(lapop$poleff),]),
  NROW(ces[!is.na(ces$poleff),]),
  NROW(bes[!is.na(bes$poleff),]),
  NROW(anes16[!is.na(anes16$poleff),]),
  NROW(anes12[!is.na(anes12$poleff),]),
  NROW(anes1012[!is.na(anes1012$poleff),])
))
round(r.con(poleff_O, n = poleff_n), 3)

# COR: Efficacy and Conscientiousness ----
cors <- c(
  cor(sels$poleff, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$poleff, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$poleff, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$poleff, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$poleff, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$poleff, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$poleff, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$poleff, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$poleff, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$poleff) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$poleff) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$poleff) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$poleff) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$poleff) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$poleff) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$poleff) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$poleff) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$poleff) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poleff_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poleff_C, n = poleff_n), 3)

# COR: Efficacy and Extraversion ----
cors <- c(
  cor(sels$poleff, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$poleff, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$poleff, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$poleff, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$poleff, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$poleff, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$poleff, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$poleff, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$poleff, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$poleff) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$poleff) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$poleff) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$poleff) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$poleff) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$poleff) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$poleff) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$poleff) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$poleff) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poleff_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poleff_E, n = poleff_n), 3)

# COR: Efficacy and Agreeableness ----
cors <- c(
  cor(sels$poleff, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$poleff, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$poleff, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$poleff, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$poleff, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$poleff, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$poleff, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$poleff, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$poleff, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$poleff) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$poleff) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$poleff) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$poleff) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$poleff) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$poleff) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$poleff) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$poleff) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$poleff) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poleff_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poleff_A, n = poleff_n), 3)

# COR: Efficacy and Neuroticism ----
cors <- c(
  cor(sels$poleff, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$poleff, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$poleff, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$poleff, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$poleff, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$poleff, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$poleff, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$poleff, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$poleff, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$poleff) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$poleff) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$poleff) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$poleff) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$poleff) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$poleff) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$poleff) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$poleff) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$poleff) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poleff_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poleff_N, n = poleff_n), 3)

# Interest
# COR: Interest and Openness ----
cors <- c(
  cor(shp$polintr, shp$openness, use = "pairwise.complete.obs"),
  cor(sels$polintr, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$polintr, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$polintr, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$polintr, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$polintr, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$polintr, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$polintr, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$polintr, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$polintr, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polintr),]),
  NROW(sels[!is.na(sels$polintr),]),
  NROW(nzes[!is.na(nzes$polintr),]),
  NROW(liss[!is.na(liss$polintr),]),
  NROW(lapop[!is.na(lapop$polintr),]),
  NROW(ces[!is.na(ces$polintr),]),
  NROW(bes[!is.na(bes$polintr),]),
  NROW(anes16[!is.na(anes16$polintr),]),
  NROW(anes12[!is.na(anes12$polintr),]),
  NROW(anes1012[!is.na(anes1012$polintr),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polintr_O <- fisherz2r(sum(n3z)/sum(n3)))

polintr_n <- sum(c(
  NROW(shp[!is.na(shp$polintr),]),
  NROW(sels[!is.na(sels$polintr),]),
  NROW(nzes[!is.na(nzes$polintr),]),
  NROW(liss[!is.na(liss$polintr),]),
  NROW(lapop[!is.na(lapop$polintr),]),
  NROW(ces[!is.na(ces$polintr),]),
  NROW(bes[!is.na(bes$polintr),]),
  NROW(anes16[!is.na(anes16$polintr),]),
  NROW(anes12[!is.na(anes12$polintr),]),
  NROW(anes1012[!is.na(anes1012$polintr),])
))
round(r.con(polintr_O, n = polintr_n), 3)

# COR: Interest and Conscientiousness ----
cors <- c(
  cor(shp$polintr, shp$conscientiousness, use = "pairwise.complete.obs"),
  cor(sels$polintr, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$polintr, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$polintr, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$polintr, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$polintr, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$polintr, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$polintr, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$polintr, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$polintr, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polintr) & !is.na(shp$conscientiousness),]),
  NROW(sels[!is.na(sels$polintr) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$polintr) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$polintr) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$polintr) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$polintr) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$polintr) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$polintr) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$polintr) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$polintr) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polintr_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polintr_C, n = polintr_n), 3)

# COR: Interest and Extraversion ----
cors <- c(
  cor(shp$polintr, shp$extraversion, use = "pairwise.complete.obs"),
  cor(sels$polintr, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$polintr, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$polintr, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$polintr, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$polintr, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$polintr, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$polintr, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$polintr, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$polintr, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polintr) & !is.na(shp$extraversion),]),
  NROW(sels[!is.na(sels$polintr) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$polintr) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$polintr) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$polintr) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$polintr) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$polintr) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$polintr) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$polintr) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$polintr) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polintr_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polintr_E, n = polintr_n), 3)

# COR: Interest and Agreeableness ----
cors <- c(
  cor(shp$polintr, shp$agreeableness, use = "pairwise.complete.obs"),
  cor(sels$polintr, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$polintr, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$polintr, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$polintr, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$polintr, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$polintr, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$polintr, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$polintr, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$polintr, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polintr) & !is.na(shp$agreeableness),]),
  NROW(sels[!is.na(sels$polintr) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$polintr) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$polintr) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$polintr) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$polintr) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$polintr) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$polintr) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$polintr) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$polintr) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polintr_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polintr_A, n = polintr_n), 3)

# COR: Interest and Neuroticism ----
cors <- c(
  cor(shp$polintr, shp$neuroticism, use = "pairwise.complete.obs"),
  cor(sels$polintr, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$polintr, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$polintr, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$polintr, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$polintr, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$polintr, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$polintr, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$polintr, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$polintr, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polintr) & !is.na(shp$neuroticism),]),
  NROW(sels[!is.na(sels$polintr) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$polintr) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$polintr) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$polintr) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$polintr) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$polintr) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$polintr) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$polintr) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$polintr) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polintr_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polintr_N, n = polintr_n), 3)

# Involvement
# COR: Involvement and Openness ----
cors <- c(
  cor(sels$involvement, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$involvement, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$involvement, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$involvement, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$involvement, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$involvement, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$involvement, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$involvement, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$involvement, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$involvement),]),
  NROW(nzes[!is.na(nzes$involvement),]),
  NROW(liss[!is.na(liss$involvement),]),
  NROW(lapop[!is.na(lapop$involvement),]),
  NROW(ces[!is.na(ces$involvement),]),
  NROW(bes[!is.na(bes$involvement),]),
  NROW(anes16[!is.na(anes16$involvement),]),
  NROW(anes12[!is.na(anes12$involvement),]),
  NROW(anes1012[!is.na(anes1012$involvement),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(involvement_O <- fisherz2r(sum(n3z)/sum(n3)))

involvement_n <- sum(c(
  NROW(sels[!is.na(sels$involvement),]),
  NROW(nzes[!is.na(nzes$involvement),]),
  NROW(liss[!is.na(liss$involvement),]),
  NROW(lapop[!is.na(lapop$involvement),]),
  NROW(ces[!is.na(ces$involvement),]),
  NROW(bes[!is.na(bes$involvement),]),
  NROW(anes16[!is.na(anes16$involvement),]),
  NROW(anes12[!is.na(anes12$involvement),]),
  NROW(anes1012[!is.na(anes1012$involvement),])
))
round(r.con(involvement_O, n = involvement_n), 3)

# COR: Involvement and Conscientiousness ----
cors <- c(
  cor(sels$involvement, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$involvement, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$involvement, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$involvement, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$involvement, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$involvement, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$involvement, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$involvement, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$involvement, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$involvement) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$involvement) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$involvement) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$involvement) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$involvement) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$involvement) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$involvement) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$involvement) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$involvement) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(involvement_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(involvement_C, n = involvement_n), 3)

# COR: Involvement and Extraversion ----
cors <- c(
  cor(sels$involvement, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$involvement, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$involvement, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$involvement, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$involvement, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$involvement, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$involvement, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$involvement, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$involvement, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$involvement) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$involvement) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$involvement) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$involvement) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$involvement) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$involvement) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$involvement) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$involvement) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$involvement) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(involvement_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(involvement_E, n = involvement_n), 3)

# COR: Involvement and Agreesableness ----
cors <- c(
  cor(sels$involvement, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$involvement, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$involvement, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$involvement, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$involvement, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$involvement, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$involvement, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$involvement, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$involvement, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$involvement) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$involvement) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$involvement) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$involvement) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$involvement) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$involvement) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$involvement) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$involvement) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$involvement) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(involvement_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(involvement_A, n = involvement_n), 3)

cors <- c(
  cor(sels$involvement, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$involvement, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$involvement, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$involvement, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$involvement, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$involvement, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$involvement, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$involvement, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$involvement, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$involvement) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$involvement) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$involvement) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$involvement) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$involvement) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$involvement) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$involvement) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$involvement) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$involvement) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(involvement_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(involvement_N, n = involvement_n), 3)

# Knowledge
# COR: Knowledge and Openness ----
cors <- c(
  cor(sels$knowledge, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$knowledge, nzes$openness, use = "pairwise.complete.obs"),
  cor(lapop$knowledge, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$knowledge, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$knowledge, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$knowledge, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$knowledge, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$knowledge, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$knowledge),]),
  NROW(nzes[!is.na(nzes$knowledge),]),
  NROW(lapop[!is.na(lapop$knowledge),]),
  NROW(ces[!is.na(ces$knowledge),]),
  NROW(bes[!is.na(bes$knowledge),]),
  NROW(anes16[!is.na(anes16$knowledge),]),
  NROW(anes12[!is.na(anes12$knowledge),]),
  NROW(anes1012[!is.na(anes1012$knowledge),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(knowledge_O <- fisherz2r(sum(n3z)/sum(n3)))

knowledge_n <- sum(c(
  NROW(sels[!is.na(sels$knowledge),]),
  NROW(nzes[!is.na(nzes$knowledge),]),
  NROW(lapop[!is.na(lapop$knowledge),]),
  NROW(ces[!is.na(ces$knowledge),]),
  NROW(bes[!is.na(bes$knowledge),]),
  NROW(anes16[!is.na(anes16$knowledge),]),
  NROW(anes12[!is.na(anes12$knowledge),]),
  NROW(anes1012[!is.na(anes1012$knowledge),])
))
round(r.con(knowledge_O, n = knowledge_n), 3)

# COR: Knowledge and Conscientiousness ----
cors <- c(
  cor(sels$knowledge, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$knowledge, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$knowledge, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$knowledge, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$knowledge, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$knowledge, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$knowledge, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$knowledge, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$knowledge) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$knowledge) & !is.na(nzes$conscientiousness),]),
  NROW(lapop[!is.na(lapop$knowledge) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$knowledge) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$knowledge) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$knowledge) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$knowledge) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$knowledge) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(knowledge_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(knowledge_C, n = knowledge_n), 3)

# COR: Knowledge and Extraversion ----
cors <- c(
  cor(sels$knowledge, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$knowledge, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$knowledge, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$knowledge, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$knowledge, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$knowledge, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$knowledge, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$knowledge, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$knowledge) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$knowledge) & !is.na(nzes$extraversion),]),
  NROW(lapop[!is.na(lapop$knowledge) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$knowledge) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$knowledge) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$knowledge) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$knowledge) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$knowledge) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(knowledge_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(knowledge_E, n = knowledge_n), 3)

# COR: Knowledge and Agreeableness ----
cors <- c(
  cor(sels$knowledge, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$knowledge, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$knowledge, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$knowledge, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$knowledge, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$knowledge, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$knowledge, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$knowledge, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$knowledge) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$knowledge) & !is.na(nzes$agreeableness),]),
  NROW(lapop[!is.na(lapop$knowledge) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$knowledge) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$knowledge) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$knowledge) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$knowledge) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$knowledge) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(knowledge_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(knowledge_A, n = knowledge_n), 3)

# COR: Knowledge and Neuroticism ----
cors <- c(
  cor(sels$knowledge, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$knowledge, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$knowledge, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$knowledge, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$knowledge, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$knowledge, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$knowledge, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$knowledge, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$knowledge) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$knowledge) & !is.na(nzes$neuroticism),]),
  NROW(lapop[!is.na(lapop$knowledge) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$knowledge) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$knowledge) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$knowledge) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$knowledge) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$knowledge) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(knowledge_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(knowledge_N, n = knowledge_n), 3)

# Media
# COR: Media and Openness ----
cors <- c(
  cor(sels$media, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$media, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$media, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$media, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$media, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$media, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$media, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$media, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$media, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$media),]),
  NROW(nzes[!is.na(nzes$media),]),
  NROW(liss[!is.na(liss$media),]),
  NROW(lapop[!is.na(lapop$media),]),
  NROW(ces[!is.na(ces$media),]),
  NROW(bes[!is.na(bes$media),]),
  NROW(anes16[!is.na(anes16$media),]),
  NROW(anes12[!is.na(anes12$media),]),
  NROW(anes1012[!is.na(anes1012$media),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(media_O <- fisherz2r(sum(n3z)/sum(n3)))

media_n <- sum(c(
  NROW(sels[!is.na(sels$media),]),
  NROW(nzes[!is.na(nzes$media),]),
  NROW(liss[!is.na(liss$media),]),
  NROW(lapop[!is.na(lapop$media),]),
  NROW(ces[!is.na(ces$media),]),
  NROW(bes[!is.na(bes$media),]),
  NROW(anes16[!is.na(anes16$media),]),
  NROW(anes12[!is.na(anes12$media),]),
  NROW(anes1012[!is.na(anes1012$media),])
))
round(r.con(media_O, n = media_n), 3)

# COR: Media and Conscientiousness ----
cors <- c(
  cor(sels$media, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$media, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$media, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$media, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$media, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$media, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$media, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$media, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$media, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$media) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$media) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$media) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$media) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$media) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$media) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$media) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$media) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$media) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(media_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(media_C, n = media_n), 3)

# COR: Media and Extraversion ----
cors <- c(
  cor(sels$media, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$media, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$media, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$media, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$media, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$media, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$media, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$media, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$media, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$media) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$media) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$media) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$media) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$media) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$media) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$media) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$media) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$media) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(media_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(media_E, n = media_n), 3)

# COR: Media and Agreeableness ----
cors <- c(
  cor(sels$media, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$media, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$media, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$media, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$media, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$media, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$media, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$media, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$media, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$media) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$media) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$media) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$media) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$media) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$media) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$media) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$media) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$media) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(media_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(media_A, n = media_n), 3)

# COR: Media and Neuroticism ----
cors <- c(
  cor(sels$media, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$media, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$media, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$media, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$media, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$media, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$media, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$media, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$media, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(sels[!is.na(sels$media) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$media) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$media) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$media) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$media) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$media) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$media) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$media) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$media) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(media_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(media_N, n = media_n), 3)

# Political participation
# COR: Political participation and Openness ----
cors <- c(
  cor(shp$polpar, shp$openness, use = "pairwise.complete.obs"),
  cor(sels$polpar, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$polpar, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$polpar, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$polpar, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$polpar, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$polpar, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$polpar, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$polpar, anes12$openness, use = "pairwise.complete.obs"),
  cor(anes1012$polpar, anes1012$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polpar),]),
  NROW(sels[!is.na(sels$polpar),]),
  NROW(nzes[!is.na(nzes$polpar),]),
  NROW(liss[!is.na(liss$polpar),]),
  NROW(lapop[!is.na(lapop$polpar),]),
  NROW(ces[!is.na(ces$polpar),]),
  NROW(bes[!is.na(bes$polpar),]),
  NROW(anes16[!is.na(anes16$polpar),]),
  NROW(anes12[!is.na(anes12$polpar),]),
  NROW(anes1012[!is.na(anes1012$polpar),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polpar_O <- fisherz2r(sum(n3z)/sum(n3)))

polpar_n <- sum(c(
  NROW(shp[!is.na(shp$polpar),]),
  NROW(sels[!is.na(sels$polpar),]),
  NROW(nzes[!is.na(nzes$polpar),]),
  NROW(liss[!is.na(liss$polpar),]),
  NROW(lapop[!is.na(lapop$polpar),]),
  NROW(ces[!is.na(ces$polpar),]),
  NROW(bes[!is.na(bes$polpar),]),
  NROW(anes16[!is.na(anes16$polpar),]),
  NROW(anes12[!is.na(anes12$polpar),]),
  NROW(anes1012[!is.na(anes1012$polpar),])
))
round(r.con(polpar_O, n = polpar_n), 3)

# COR: Political participation and Conscientiousness ----
cors <- c(
  cor(shp$polpar, shp$conscientiousness, use = "pairwise.complete.obs"),
  cor(sels$polpar, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$polpar, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$polpar, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$polpar, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$polpar, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$polpar, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$polpar, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$polpar, anes12$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes1012$polpar, anes1012$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polpar) & !is.na(shp$conscientiousness),]),
  NROW(sels[!is.na(sels$polpar) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$polpar) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$polpar) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$polpar) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$polpar) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$polpar) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$polpar) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$polpar) & !is.na(anes12$conscientiousness),]),
  NROW(anes1012[!is.na(anes1012$polpar) & !is.na(anes1012$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polpar_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polpar_C, n = polpar_n), 3)

# COR: Political participation and Extraversion ----
cors <- c(
  cor(shp$polpar, shp$extraversion, use = "pairwise.complete.obs"),
  cor(sels$polpar, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$polpar, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$polpar, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$polpar, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$polpar, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$polpar, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$polpar, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$polpar, anes12$extraversion, use = "pairwise.complete.obs"),
  cor(anes1012$polpar, anes1012$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polpar) & !is.na(shp$extraversion),]),
  NROW(sels[!is.na(sels$polpar) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$polpar) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$polpar) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$polpar) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$polpar) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$polpar) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$polpar) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$polpar) & !is.na(anes12$extraversion),]),
  NROW(anes1012[!is.na(anes1012$polpar) & !is.na(anes1012$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polpar_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polpar_E, n = polpar_n), 3)

# COR: Political participation and Agreeableness ----
cors <- c(
  cor(shp$polpar, shp$agreeableness, use = "pairwise.complete.obs"),
  cor(sels$polpar, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$polpar, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$polpar, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$polpar, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$polpar, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$polpar, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$polpar, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$polpar, anes12$agreeableness, use = "pairwise.complete.obs"),
  cor(anes1012$polpar, anes1012$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polpar) & !is.na(shp$agreeableness),]),
  NROW(sels[!is.na(sels$polpar) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$polpar) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$polpar) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$polpar) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$polpar) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$polpar) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$polpar) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$polpar) & !is.na(anes12$agreeableness),]),
  NROW(anes1012[!is.na(anes1012$polpar) & !is.na(anes1012$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polpar_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polpar_A, n = polpar_n), 3)

# COR: Political participation and Neuroticism ----
cors <- c(
  cor(shp$polpar, shp$neuroticism, use = "pairwise.complete.obs"),
  cor(sels$polpar, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$polpar, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$polpar, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$polpar, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$polpar, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$polpar, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$polpar, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$polpar, anes12$neuroticism, use = "pairwise.complete.obs"),
  cor(anes1012$polpar, anes1012$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$polpar) & !is.na(shp$neuroticism),]),
  NROW(sels[!is.na(sels$polpar) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$polpar) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$polpar) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$polpar) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$polpar) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$polpar) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$polpar) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$polpar) & !is.na(anes12$neuroticism),]),
  NROW(anes1012[!is.na(anes1012$polpar) & !is.na(anes1012$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(polpar_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(polpar_N, n = polpar_n), 3)

# Political trust
# COR: Political trust and Openness ----
cors <- c(
  cor(shp$poltr, shp$openness, use = "pairwise.complete.obs"),
  cor(sels$poltr, sels$openness, use = "pairwise.complete.obs"),
  cor(liss$poltr, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$poltr, lapop$openness, use = "pairwise.complete.obs"),
  cor(bes$poltr, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$poltr, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$poltr, anes12$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$poltr),]),
  NROW(sels[!is.na(sels$poltr),]),
  NROW(liss[!is.na(liss$poltr),]),
  NROW(lapop[!is.na(lapop$poltr),]),
  NROW(bes[!is.na(bes$poltr),]),
  NROW(anes16[!is.na(anes16$poltr),]),
  NROW(anes12[!is.na(anes12$poltr),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poltr_O <- fisherz2r(sum(n3z)/sum(n3)))

poltr_n <- sum(c(
  NROW(shp[!is.na(shp$poltr),]),
  NROW(sels[!is.na(sels$poltr),]),
  NROW(liss[!is.na(liss$poltr),]),
  NROW(lapop[!is.na(lapop$poltr),]),
  NROW(bes[!is.na(bes$poltr),]),
  NROW(anes16[!is.na(anes16$poltr),]),
  NROW(anes12[!is.na(anes12$poltr),])
))
round(r.con(poltr_O, n = poltr_n), 3)

# COR: Political trust and Conscientiousness ----
cors <- c(
  cor(shp$poltr, shp$conscientiousness, use = "pairwise.complete.obs"),
  cor(sels$poltr, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$poltr, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$poltr, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$poltr, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$poltr, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$poltr, anes12$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$poltr) & !is.na(shp$conscientiousness),]),
  NROW(sels[!is.na(sels$poltr) & !is.na(sels$conscientiousness),]),
  NROW(liss[!is.na(liss$poltr) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$poltr) & !is.na(lapop$conscientiousness),]),
  NROW(bes[!is.na(bes$poltr) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$poltr) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$poltr) & !is.na(anes12$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poltr_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poltr_C, n = poltr_n), 3)

# COR: Political trust and Extraversion ----
cors <- c(
  cor(shp$poltr, shp$extraversion, use = "pairwise.complete.obs"),
  cor(sels$poltr, sels$extraversion, use = "pairwise.complete.obs"),
  cor(liss$poltr, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$poltr, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(bes$poltr, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$poltr, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$poltr, anes12$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$poltr) & !is.na(shp$extraversion),]),
  NROW(sels[!is.na(sels$poltr) & !is.na(sels$extraversion),]),
  NROW(liss[!is.na(liss$poltr) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$poltr) & !is.na(lapop$extraversion),]),
  NROW(bes[!is.na(bes$poltr) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$poltr) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$poltr) & !is.na(anes12$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poltr_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poltr_E, n = poltr_n), 3)

# COR: Political trust and Agreeableness ----
cors <- c(
  cor(shp$poltr, shp$agreeableness, use = "pairwise.complete.obs"),
  cor(sels$poltr, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$poltr, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$poltr, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$poltr, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$poltr, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$poltr, anes12$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$poltr) & !is.na(shp$agreeableness),]),
  NROW(sels[!is.na(sels$poltr) & !is.na(sels$agreeableness),]),
  NROW(liss[!is.na(liss$poltr) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$poltr) & !is.na(lapop$agreeableness),]),
  NROW(bes[!is.na(bes$poltr) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$poltr) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$poltr) & !is.na(anes12$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poltr_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poltr_A, n = poltr_n), 3)

# COR: Political trust and Neuroticism ----
cors <- c(
  cor(shp$poltr, shp$neuroticism, use = "pairwise.complete.obs"),
  cor(sels$poltr, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$poltr, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$poltr, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$poltr, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$poltr, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$poltr, anes12$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$poltr) & !is.na(shp$neuroticism),]),
  NROW(sels[!is.na(sels$poltr) & !is.na(sels$neuroticism),]),
  NROW(liss[!is.na(liss$poltr) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$poltr) & !is.na(lapop$neuroticism),]),
  NROW(bes[!is.na(bes$poltr) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$poltr) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$poltr) & !is.na(anes12$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(poltr_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(poltr_N, n = poltr_n), 3)

# Sat. democracy
# COR: Sat. democracy and Openness ----
cors <- c(
  cor(shp$stfdem, shp$openness, use = "pairwise.complete.obs"),
  cor(sels$stfdem, sels$openness, use = "pairwise.complete.obs"),
  cor(nzes$stfdem, nzes$openness, use = "pairwise.complete.obs"),
  cor(liss$stfdem, liss$openness, use = "pairwise.complete.obs"),
  cor(lapop$stfdem, lapop$openness, use = "pairwise.complete.obs"),
  cor(ces$stfdem, ces$openness, use = "pairwise.complete.obs"),
  cor(bes$stfdem, bes$openness, use = "pairwise.complete.obs"),
  cor(anes16$stfdem, anes16$openness, use = "pairwise.complete.obs"),
  cor(anes12$stfdem, anes12$openness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$stfdem),]),
  NROW(sels[!is.na(sels$stfdem),]),
  NROW(nzes[!is.na(nzes$stfdem),]),
  NROW(liss[!is.na(liss$stfdem),]),
  NROW(lapop[!is.na(lapop$stfdem),]),
  NROW(ces[!is.na(ces$stfdem),]),
  NROW(bes[!is.na(bes$stfdem),]),
  NROW(anes16[!is.na(anes16$stfdem),]),
  NROW(anes12[!is.na(anes12$stfdem),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(stfdem_O <- fisherz2r(sum(n3z)/sum(n3)))

stfdem_n <- sum(c(
  NROW(shp[!is.na(shp$stfdem),]),
  NROW(sels[!is.na(sels$stfdem),]),
  NROW(nzes[!is.na(nzes$stfdem),]),
  NROW(liss[!is.na(liss$stfdem),]),
  NROW(lapop[!is.na(lapop$stfdem),]),
  NROW(ces[!is.na(ces$stfdem),]),
  NROW(bes[!is.na(bes$stfdem),]),
  NROW(anes16[!is.na(anes16$stfdem),]),
  NROW(anes12[!is.na(anes12$stfdem),])
))
round(r.con(stfdem_O, n = stfdem_n), 3)

# COR: Sat. democracy and Conscientiousness ----
cors <- c(
  cor(shp$stfdem, shp$conscientiousness, use = "pairwise.complete.obs"),
  cor(sels$stfdem, sels$conscientiousness, use = "pairwise.complete.obs"),
  cor(nzes$stfdem, nzes$conscientiousness, use = "pairwise.complete.obs"),
  cor(liss$stfdem, liss$conscientiousness, use = "pairwise.complete.obs"),
  cor(lapop$stfdem, lapop$conscientiousness, use = "pairwise.complete.obs"),
  cor(ces$stfdem, ces$conscientiousness, use = "pairwise.complete.obs"),
  cor(bes$stfdem, bes$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes16$stfdem, anes16$conscientiousness, use = "pairwise.complete.obs"),
  cor(anes12$stfdem, anes12$conscientiousness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$stfdem) & !is.na(shp$conscientiousness),]),
  NROW(sels[!is.na(sels$stfdem) & !is.na(sels$conscientiousness),]),
  NROW(nzes[!is.na(nzes$stfdem) & !is.na(nzes$conscientiousness),]),
  NROW(liss[!is.na(liss$stfdem) & !is.na(liss$conscientiousness),]),
  NROW(lapop[!is.na(lapop$stfdem) & !is.na(lapop$conscientiousness),]),
  NROW(ces[!is.na(ces$stfdem) & !is.na(ces$conscientiousness),]),
  NROW(bes[!is.na(bes$stfdem) & !is.na(bes$conscientiousness),]),
  NROW(anes16[!is.na(anes16$stfdem) & !is.na(anes16$conscientiousness),]),
  NROW(anes12[!is.na(anes12$stfdem) & !is.na(anes12$conscientiousness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(stfdem_C <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(stfdem_C, n = stfdem_n), 3)

# COR: Sat. democracy and Extraversion ----
cors <- c(
  cor(shp$stfdem, shp$extraversion, use = "pairwise.complete.obs"),
  cor(sels$stfdem, sels$extraversion, use = "pairwise.complete.obs"),
  cor(nzes$stfdem, nzes$extraversion, use = "pairwise.complete.obs"),
  cor(liss$stfdem, liss$extraversion, use = "pairwise.complete.obs"),
  cor(lapop$stfdem, lapop$extraversion, use = "pairwise.complete.obs"),
  cor(ces$stfdem, ces$extraversion, use = "pairwise.complete.obs"),
  cor(bes$stfdem, bes$extraversion, use = "pairwise.complete.obs"),
  cor(anes16$stfdem, anes16$extraversion, use = "pairwise.complete.obs"),
  cor(anes12$stfdem, anes12$extraversion, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$stfdem) & !is.na(shp$extraversion),]),
  NROW(sels[!is.na(sels$stfdem) & !is.na(sels$extraversion),]),
  NROW(nzes[!is.na(nzes$stfdem) & !is.na(nzes$extraversion),]),
  NROW(liss[!is.na(liss$stfdem) & !is.na(liss$extraversion),]),
  NROW(lapop[!is.na(lapop$stfdem) & !is.na(lapop$extraversion),]),
  NROW(ces[!is.na(ces$stfdem) & !is.na(ces$extraversion),]),
  NROW(bes[!is.na(bes$stfdem) & !is.na(bes$extraversion),]),
  NROW(anes16[!is.na(anes16$stfdem) & !is.na(anes16$extraversion),]),
  NROW(anes12[!is.na(anes12$stfdem) & !is.na(anes12$extraversion),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(stfdem_E <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(stfdem_E, n = stfdem_n), 3)

# COR: Sat. democracy and Agreeableness ----
cors <- c(
  cor(shp$stfdem, shp$agreeableness, use = "pairwise.complete.obs"),
  cor(sels$stfdem, sels$agreeableness, use = "pairwise.complete.obs"),
  cor(nzes$stfdem, nzes$agreeableness, use = "pairwise.complete.obs"),
  cor(liss$stfdem, liss$agreeableness, use = "pairwise.complete.obs"),
  cor(lapop$stfdem, lapop$agreeableness, use = "pairwise.complete.obs"),
  cor(ces$stfdem, ces$agreeableness, use = "pairwise.complete.obs"),
  cor(bes$stfdem, bes$agreeableness, use = "pairwise.complete.obs"),
  cor(anes16$stfdem, anes16$agreeableness, use = "pairwise.complete.obs"),
  cor(anes12$stfdem, anes12$agreeableness, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$stfdem) & !is.na(shp$agreeableness),]),
  NROW(sels[!is.na(sels$stfdem) & !is.na(sels$agreeableness),]),
  NROW(nzes[!is.na(nzes$stfdem) & !is.na(nzes$agreeableness),]),
  NROW(liss[!is.na(liss$stfdem) & !is.na(liss$agreeableness),]),
  NROW(lapop[!is.na(lapop$stfdem) & !is.na(lapop$agreeableness),]),
  NROW(ces[!is.na(ces$stfdem) & !is.na(ces$agreeableness),]),
  NROW(bes[!is.na(bes$stfdem) & !is.na(bes$agreeableness),]),
  NROW(anes16[!is.na(anes16$stfdem) & !is.na(anes16$agreeableness),]),
  NROW(anes12[!is.na(anes12$stfdem) & !is.na(anes12$agreeableness),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(stfdem_A <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(stfdem_A, n = stfdem_n), 3)

# COR: Sat. democracy and Neuroticism ----
cors <- c(
  cor(shp$stfdem, shp$neuroticism, use = "pairwise.complete.obs"),
  cor(sels$stfdem, sels$neuroticism, use = "pairwise.complete.obs"),
  cor(nzes$stfdem, nzes$neuroticism, use = "pairwise.complete.obs"),
  cor(liss$stfdem, liss$neuroticism, use = "pairwise.complete.obs"),
  cor(lapop$stfdem, lapop$neuroticism, use = "pairwise.complete.obs"),
  cor(ces$stfdem, ces$neuroticism, use = "pairwise.complete.obs"),
  cor(bes$stfdem, bes$neuroticism, use = "pairwise.complete.obs"),
  cor(anes16$stfdem, anes16$neuroticism, use = "pairwise.complete.obs"),
  cor(anes12$stfdem, anes12$neuroticism, use = "pairwise.complete.obs")
)
zs <- fisherz(cors)
n3 <- c(
  NROW(shp[!is.na(shp$stfdem) & !is.na(shp$neuroticism),]),
  NROW(sels[!is.na(sels$stfdem) & !is.na(sels$neuroticism),]),
  NROW(nzes[!is.na(nzes$stfdem) & !is.na(nzes$neuroticism),]),
  NROW(liss[!is.na(liss$stfdem) & !is.na(liss$neuroticism),]),
  NROW(lapop[!is.na(lapop$stfdem) & !is.na(lapop$neuroticism),]),
  NROW(ces[!is.na(ces$stfdem) & !is.na(ces$neuroticism),]),
  NROW(bes[!is.na(bes$stfdem) & !is.na(bes$neuroticism),]),
  NROW(anes16[!is.na(anes16$stfdem) & !is.na(anes16$neuroticism),]),
  NROW(anes12[!is.na(anes12$stfdem) & !is.na(anes12$neuroticism),])
)-7 
n3z <- n3*zs
r.rc <- data.frame(cors = cors,zs = zs, n3 = n3, n3z = n3z)
(stfdem_N <- fisherz2r(sum(n3z)/sum(n3)))

round(r.con(stfdem_N, n = stfdem_n), 3)


## Creating Dataframe with all correlations ----

df_meta <- data.frame(
  Trait = c("O", "C", "E", "A", "N"),
  Efficacy = c(poleff_O, poleff_C, poleff_E, poleff_A, poleff_N),
  Ideology = c(ideology_O, ideology_C, ideology_E, ideology_A, ideology_N),
  Interest = c(polintr_O, polintr_C, polintr_E, polintr_A, polintr_N),
  Involvement = c(involvement_O, involvement_C, involvement_E, involvement_A, involvement_N),
  Knowledge = c(knowledge_O, knowledge_C, knowledge_E, knowledge_A, knowledge_N),
  Media_use = c(media_O, media_C, media_E, media_A, media_N),
  Participation = c(polpar_O, polpar_C, polpar_E, polpar_A, polpar_N),
  Political_trust = c(poltr_O, poltr_C, poltr_E, poltr_A, poltr_N),
  Sat_democracy = c(stfdem_O, stfdem_C, stfdem_E, stfdem_A, stfdem_N)
)

corr_table <- stargazer(df_meta, 
          out = here::here("3_Figures","correlations.htm"), 
          summary = F,
          digits = 2,
          title = "Correlations Personality Traits & Political Variables")

## WRITE CSV df ----
write_csv(df_meta, here::here("1_Data","df_correlations.csv"))




