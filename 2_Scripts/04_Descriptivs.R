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
head(maindata)
## DESCRIPTIVES ##

# Table 1 sample sizes----

## BES
# Student
maindata %>% 
  filter(dataset == "bes") %>% 
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

1540/(24803+1540)

## SHP
# Student
maindata %>% 
  filter(dataset == "shp") %>% 
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

753/(6007+753)

## LISS
# Student
maindata %>% 
  filter(dataset == "liss") %>% 
  drop_na(internet, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

497/(5040+497)

# Internet
maindata %>% 
  filter(dataset == "liss") %>% 
  drop_na(student, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

5213/(5213+324)

## LAPOP
# Internet
maindata %>% 
  filter(dataset == "lapop") %>% 
  drop_na(student, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

17114/(18326+17114)

# Student
maindata %>% 
  filter(dataset == "lapop") %>% 
  drop_na(internet, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

2986/(32454+2986)

## SELECTS
# Student
maindata %>% 
  filter(dataset == "sels") %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

515/(515+6708)

## NZES
# Internet
maindata %>% 
  filter(dataset == "nzes") %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

2193/(213+2193)

## CES

maindata %>% 
  filter(dataset == "ces") %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

175 / (3508+175)

## ANES 2012
# Student
maindata %>% 
  filter(dataset == "anes12") %>%
  drop_na(internet, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(student) %>%
  summarize(N = n())

404 / (404+5064)

# Internet
maindata %>% 
  filter(dataset == "anes12") %>%
  drop_na(student, openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

4779 / (4779+689)

## ANES 2010-12
# Internet
maindata %>% 
  filter(dataset == "anes1012") %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

1046 / (199+1046)

## ANES 2016
maindata %>% 
  filter(dataset == "anes16") %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  group_by(internet) %>%
  summarize(N = n())

3212 / (3212+360)



