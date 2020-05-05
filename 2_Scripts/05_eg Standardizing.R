here::here()
library(rio)
library(ggplot2)
library(tidyverse)
library(sjmisc)

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

require(ggpubr)
p1 <- ggplot(data = avars, aes(x = nettink)) + 
  geom_histogram(binwidth = 5) + theme_bw()
p2 <- ggplot(data = avars, aes(x = nettink)) + 
  geom_histogram(binwidth = 1) + 
  coord_cartesian(xlim = c(0, 20)) + theme_bw()
ggpubr::ggarrange(p1, p2, labels = list("Net income", "Net income: zoom x < 20"))


#Independend Variables
frq(avars$male)
frq(avars$high_edu) #Level of highest education: 1=Primary School, 2=Jr.High School, 3=Senor High School, 4=Junior College, 5=College, 6=Uni
summary(avars$age)

# Plotting all independend variables on nett income unstanderdized
p1 <- ggplot(avars) +
  geom_boxplot(aes(x = factor(male), y = nettink)) + 
  labs(title = "Gender", x = "", y = "Net income") + 
  scale_x_discrete(labels = c("Female", "Male")) + 
  theme_minimal()
p2 <- ggplot(avars) +
  geom_point(aes(x = high_edu, y = nettink), color = "yellow2", alpha = 0.5) +
  labs(title = "Education", x = "", y = "Net income") + 
  theme_minimal()
p3 <- ggplot(avars) + 
  geom_point(aes(x = age, y = nettink), color = "darkseagreen",alpha = 0.5) +
  labs(title = "Age", x = "", y = "Net income") + 
  theme_minimal()
p <- ggpubr::ggarrange(p1, p2, p3, ncol = 3)
# Link: https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html
ggpubr::annotate_figure(p, top = text_grob("Comparing three predictors with different measurement scales", face = "bold", size = 18))

                   
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
                  