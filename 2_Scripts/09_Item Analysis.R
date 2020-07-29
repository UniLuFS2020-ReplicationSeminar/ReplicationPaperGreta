### ITEM ANALYSIS ###

library(rio)
library(tidyverse)

## Import ## ----
tipi_items <- import(here::here("1_Data","tipi_items.csv"))
ipip_items <- import(here::here("1_Data","ipip_items.csv"))
bfiS_items <- import(here::here("1_Data","bfiS_items.csv"))
bfi10_items <- import(here::here("1_Data","bfi10_items.csv"))

#### I N T E R C O R R E L A T I O N #### ----
library(Hmisc)
library(corrplot)

# Removing age and dataset
tipi <- tipi_items %>% select(!dataset & !age)
# Correlation
tipi_items_corr <- cor(tipi)
tipi_items_corr

# Create Matrix
tipi_matrix_corr <- rcorr(as.matrix(tipi))
tipi_matrix_corr

# Creating plot
corrplot(tipi_items_corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



### F A C T O R   A N A L Y S I S ## ----
# TIPI - Exploratory Factor Analysis#

#PCA for amount of factors
tipi_pca <- princomp(tipi)
summary(tipi_pca)
plot(tipi_pca)

#Factor analysis with 5 factors
tipi_fact5 <- factanal(tipi, factors = 5, rotation = "varimax")
tipi_fact5


# IPIP - Exploratory Factor Analysis #

# Removing age and dataset
ipip <- ipip_items %>% select(!dataset & !age)

#PCA for amount of factors
ipip_pca <- princomp(ipip)
summary(ipip_pca)
plot(ipip_pca)

#Factor analysis with 5 factors
ipip_fac <- factanal(ipip, factors = 5, rotation = "varimax")
ipip_fac


## polytomous IRT testing ##
library(mirt)
ipip_openness <- ipip[,c(1:10)]

ipip_polymod <- mirt(data = ipip_openness,
                     model = 1,
                     itemtype = "gpcm")

summary(ipip_polymod) #standardized coefficients
coef(ipip_polymod, IRTbars = T) #coefficients (IRT format and not slope interact format)??
itemplot(ipip_polymod, 1, type = "trace") #curves for each item, super
itemplot(ipip_polymod, 3, type = "trace") #higher 3 4
itemplot(ipip_polymod, 6, type = "trace") #very low 1
itemplot(ipip_polymod, 9, type = "trace") #strong 3
plot(ipip_polymod, type = "trace")


# BFI 10 #

# Removing age and dataset
bfi10 <- bfi10_items %>% select(!dataset & !age)

#PCA for amount of factors
bfi10_pca <- princomp(bfi10)
summary(bfi10_pca)
plot(bfi10_pca)

#Factor analysis with 5 factors
bfi10_fac <- factanal(bfi10, factors = 5, rotation = "varimax")
bfi10_fac


# BFI S #

# Removing age and dataset
bfiS <- bfiS_items %>% select(!dataset & !age)

#PCA for amount of factors
bfiS_pca <- princomp(bfiS)
summary(bfiS_pca)
plot(bfiS_pca)

#Factor analysis with 5 factors
bfiS_fac <- factanal(bfiS, factors = 5, rotation = "varimax")
bfiS_fac
