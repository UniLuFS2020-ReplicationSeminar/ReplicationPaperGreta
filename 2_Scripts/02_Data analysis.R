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

## Creating Theme ##
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

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

### DESCRIPTIVES ### ----
# Checking N of each panel dataset
frq(maindata$dataset) # N accurate or slightly different then stated in original paper

# Student ratio
frq(anes1012$student) # NA as expected
frq(anes12$student) # Ratio exact like in orig Study
frq(anes16$student) # NA as expected
frq(bes$student) # same ratio
frq(ces$student) # same ratio
frq(lapop$student) # same ratio
frq(liss$student) # same ratio
frq(nzes$student) # 5%, in paper NA
frq(sels$student) # same ratio
frq(shp$student) # same ratio

# Internet ratio
frq(anes1012$internet) # same ratio
frq(anes12$internet) # same ratio
frq(anes16$internet) # same ratio
frq(bes$internet) # NA as expected
frq(ces$internet) # NA as expected
frq(lapop$internet) # same ratio
frq(liss$internet) # 48% users, was wrongly coded therefore a difference here (orig paper = 52%)
frq(nzes$internet) # same ratio


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

df_direct
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

# Adding p value
df_direct$pval <- 2 * pt(-abs(df_direct$est / df_direct$se), df = Inf)

