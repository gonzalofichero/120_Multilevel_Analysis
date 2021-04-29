#   |=========================================================================|
#   | Title:              EDSD 2021 - Multilevel Data Analysis                |
#   | Author:             Konrad Turek                                        |
#   | Date:               2021                                                |  
#   |-------------------------------------------------------------------------|
#   | File:               Excersie 4: Panel data analysis                     |  
#   | Data:               Part of SHARE data  (Germany)                       |  
#   |=========================================================================|

# -------------------------------------------------------------------------
### Introduction
# -------------------------------------------------------------------------
# Level-1: occasions
# Level-2: respondent 
# DV: Self-rate Health [higehr=better]
# level-1 pred: # of chronic diseases
# level-2 pred: gender
# -------------------------------------------------------------------------


###########################################################  
###                                                       #
###   Setup                                               #
###                                                       #
###########################################################  

#----------------------------------------------------------
#      Load Packages          
#----------------------------------------------------------
# install.packages("plm")

library(plm) # for panel analysis 

library(lme4) # ML analysis package
library(lmerTest) # to get p-value in lme4

library(lmtest) # Robust Standard Errors 
library(performance) # for ICC measures 
library(psych) # For desrptive statistics 
library(dplyr)
library(MuMIn) # R2


#----------------------------------------------------------
# Loading data
#----------------------------------------------------------

dat <- read.table("Day 4/panel_GE_srh.csv", header = TRUE,  sep = ",")

head(dat)
nrow(dat)
length(unique(dat$id)) # Number of groups 
table(dat$wave)


#
options(scipen=3)
########################################################### 
###                                                       #
###   Exploration                                         #
###                                                       #
###########################################################

mean(dat$srh, na.rm = TRUE)
hist(dat$srh)


### Descriptive statistics by married 
describeBy(dat$srh, group=dat$wave) # psych package

dat %>% 
  group_by(wave) %>% 
  summarise(mean = mean(srh, na.rm = TRUE),
            sd = sd(srh, na.rm = TRUE),
            min = min(srh, na.rm = TRUE),
            max = max(srh, na.rm = TRUE),
            n = n())

boxplot(srh~ wave, dat) 

###
summary(dat$chronic) # psych package

########################################################### 
###                                                       #
###   Baseline one-level model (not multilevel)           #
###                                                       #
###########################################################
  # NOTE: 
  # - this is a simple regression (OLS) model on pooled data
  # - it ignores the hierarchical (correlated) data structure

#----------------------------------------------------------
# Only predictor                                                                                                  #
#----------------------------------------------------------
m.ols <- lm(srh ~ chronic + female , data = dat)
summary(m.ols)
nobs(m.ols)

coeftest(m.ols) # Robust Standard Errors 
coeftest(m.ols, vcov. = vcovHC, type = "HC1")


########################################################### 
###                                                       #
###   Multilevel model (rand.intercept)  with lme4        #
###                                                       #
###########################################################
m.ri <- lmer(srh ~ chronic + female + (1|id), data = dat)
summary(m.ri)

icc(m.ri)

r2_nakagawa(m.ri, by_group=TRUE) 
r2_nakagawa(m.ri) 

########################################################### 
###                                                       #
###   Fixed effects panel model   [plm]                   #
###                                                       #
###########################################################


# estimate the fixed effects regression with plm()
m.fe <- plm(srh ~ chronic + female , data = dat,
                    index = c("id", "wave"), 
                    model = "within")
 
summary(m.fe)
pFtest(m.fe, m.ols) # If p-value < 0.05 then the fixed effects model is a better choice

within_intercept(m.fe) #  overall intercept for within models and its accompanying standard error



########################################################### 
###                                                       #
###   Random effects panel model  (rand.intercept) [plm]  #
###                                                       #
###########################################################


# estimate the fixed effects regression with plm()
m.re <- plm(srh ~ chronic + female , data = dat,
                    index = c("id", "wave"), 
                    model = "random")
 
summary(m.re)


# Robust SE
coeftest(m.re, vcov. = vcovHC, type = "HC3") # HC1 the same results as in Stata

# Hausman test 
phtest(m.fe, m.re)




########################################################### 
###                                                       #
###   Within-between (Hybrid) model                       #
###                                                       #
###########################################################

### Group-centering
dat.m <- aggregate(dat = dat, chronic ~ id, FUN = mean)
dat <- merge(x = dat, y = dat.m, by = "id", suffixes = c("",".m")) #between component (group-mean)
dat$chronic.d <- (dat$chronic-dat$chronic.m) # within component (group-centering / deviation)

head(dat)


### Hybrid Model
m.wb <- plm(srh ~ chronic.d + chronic.m + female , data = dat,
                    index = c("id", "wave"), 
                    model = "random")
summary(m.wb)

### Mundlak Model
m.mdl <- plm(srh ~ chronic + chronic.m + female , data = dat,
                    index = c("id", "wave"), 
                    model = "random")
summary(m.mdl)
 
### Between Model - to compare 
m.be <- plm(srh ~ chronic + female , data = dat,
                    index = c("id", "wave"), 
                    model = "between")
summary(m.be)
 


