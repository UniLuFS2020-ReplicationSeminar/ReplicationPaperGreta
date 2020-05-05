here::here()
library(rio)
library(ggplot2)
library(tidyverse)
library(sjmisc)

# Longitudinal Internet Studies in the Social Sciences - Background Variables 2008 #
avars2008 <- import(here::here("1_Data","1_Panel Datasets", "avars_200805_EN_2.0p.dta"))

# Creating new file without personalised Data
avars_background_var <- avars2008 %>% 
  select(geslacht,gebjaar,aantalhh,burgstat,nettoink,oplcat)
write_csv(avars_background_var, here::here("1_Data","Income_Standardizing.csv"))

## Import File for Testing ##
avars <- import(here::here("1_Data","Income_Standardizing.csv"))
                 
avars <- avars %>% 
  mutate(male = ifelse(geslacht==1,1,0)) %>%
  mutate(age = 2008 - gebjaar, rm.na=T) %>%
  mutate(nettink = ifelse(nettoink==-13 |nettoink==-14 | nettoink==-15,0,nettoink)) %>%
  mutate(nettink = nettink/1000, rm.na=T) %>% ## Monthly income in thousands
  mutate(high_edu = oplcat, rm.na = T) %>% 
  filter(!is.na(high_edu)) %>% 
  filter(age >= 18) #Filter only working people
                    
#Dependend Variable
summary(avars$nettink)

#Independend Variables
frq(avars$male)
frq(avars$high_edu) #Level of highest education: 1=Primary School, 2=Jr.High School, 3=Senor High School, 4=Junior College, 5=College, 6=Uni
summary(avars$age)
                    
# Plotting all independend variables on nett income unstanderdized
ggplot(avars)+
  geom_point(mapping=aes(x=male,y=nettink), color = "darkblue",alpha = 0.5)+
  geom_point(mapping=aes(x=high_edu,y=nettink), color = "yellow2",alpha = 0.5)+
  geom_point(mapping=aes(x=age,y=nettink), color = "darkseagreen",alpha = 0.5)+
  theme_minimal()

                   
##Standardize Functions##
# Divided by one standarddeviation
divby_1sd <- function(x) {return((x - mean(x, na.rm = T)) / sd(x, na.rm = T))}

# Divided by two standarddeviation
divby_2sd <- function(x) {return((x - mean(x, na.rm = T))/(2*sd(x, na.rm = T)))}


## Creating new standerdized Variables (1 SD) 
avars <- avars %>% 
  mutate(male0 = male - mean(male)) %>% 
  mutate(high_edu1 = divby_1sd(high_edu)) %>% 
  mutate(age1 = divby_1sd(age)) %>% 
  mutate(male1 = divby_1sd(male))

# Creating new standerdized Variables (2 SD)
avars <- avars %>% 
  mutate(male2 = divby_2sd(male)) %>% 
  mutate(high_edu2 = divby_2sd(high_edu)) %>% 
  mutate(age2 = divby_2sd(age))


# PLOT DIV BY 1 SD - NORMAL STANDARDIZE#
ggplot(avars)+
  geom_point(mapping=aes(x=male,y=nettink), color = "darkblue",alpha = 0.5)+
  geom_point(mapping=aes(x=high_edu1,y=nettink), color = "yellow2",alpha = 0.5)+
  geom_point(mapping=aes(x=age1,y=nettink), color = "darkseagreen",alpha = 0.5)+
  theme_minimal()

# PLOT DIV BY 2 SD #
ggplot(avars)+
  geom_point(mapping=aes(x=male,y=nettink), color = "darkblue",alpha = 0.5)+
  geom_point(mapping=aes(x=high_edu2,y=nettink), color = "yellow2",alpha = 0.5)+
  geom_point(mapping=aes(x=age2,y=nettink), color = "darkseagreen",alpha = 0.5)+
  theme_minimal()

#mod0: Linear Model Without Standerdizing #
mod0 <- lm(formula = avars$nettink ~ 
             avars$male + 
             avars$high_edu +  
             avars$age)
summary(mod0)

#mod1: Linear Model Standerdizing by 1 SD #
mod1 <- lm(formula = avars$nettink ~ 
             avars$male + 
             avars$high_edu1 + 
             avars$age1)
summary(mod1)

#mod2: Linear Model Standerdizing by 2 SD #
mod2 <- lm(formula = avars$nettink ~ 
             avars$male + 
             avars$high_edu2 + 
             avars$age2)
summary(mod2)
                  