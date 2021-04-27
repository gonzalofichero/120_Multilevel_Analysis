#   |=========================================================================|
#   | Title:              EDSE 2021 - Multilevel Data Analysis                |
#   | Author:             Konrad Turek                                        |
#   | Date:               04.2021                                             |  
#   |-------------------------------------------------------------------------|
#   | File:               Exercise 1: Basic ML models in R (see slides day 1) | 
#   | Data:               Part of SHARE data wave 6                           |  
#   |=========================================================================|

# -------------------------------------------------------------------------
### Introduction
# -------------------------------------------------------------------------
# Level-1: respondents
# Level-2: countries 
# DV: Quality of life and well-being index (casp) [higehr=better]
# level-1 pred: married
# level-2 pred: - none - 
# -------------------------------------------------------------------------


###########################################################  
###                                                       #
###   Setup                                               #
###                                                       #
###########################################################  

#----------------------------------------------------------
#      Load Packages          
#----------------------------------------------------------
# if a package is not installed, use install.packages, e.g. install.packages("lme4")

library(miceadds) # Clustered Roboust SE
library(sandwich) # Clustered Roboust SE

library(lme4) # ML analysis package
library(lmerTest) # to get p-value in lme4

library(readstata13) # to read data

library(dplyr) # technical package 
library(psych) # For desrptive statistics 
library(lattice) # for densityplot
library(performance) # for icc
library(arm) # for se.ranef


#----------------------------------------------------------
# Loading data
#----------------------------------------------------------

dat <- read.dta13("Day 1/ex_1.dta")

head(dat)
nrow(dat)
length(unique(dat$cntr)) # Number of groups 


########################################################### 
###                                                       #
###   Exploration                                         #
###                                                       #
###########################################################

###
table(dat$cntr,dat$married)

### Let's look at the dependent variable 
mean(dat$casp, na.rm = TRUE)
hist(dat$casp)

### Descriptive statistics by married 
describeBy(dat$casp, group=dat$married) # psych package
boxplot(casp~ married, dat) 
densityplot(~ casp, groups = married, dat,
            plot.points = FALSE) # Distribution of single variables by married (lattice)
 

### Descriptive statistics by country 

dat %>% 
  group_by(cntr) %>% 
  summarize(mean = mean(casp, na.rm = TRUE),
            sd = sd(casp, na.rm = TRUE),
            min = min(casp, na.rm = TRUE),
            max = max(casp, na.rm = TRUE),
            n = n())
#
densityplot(~ casp, groups = cntr, dat,
            plot.points = FALSE) 

 


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
m.ols <- lm(casp ~ married , data = dat)
summary(m.ols)

#----------------------------------------------------------
# Clustered standard errors                                                                                                #
#----------------------------------------------------------
m.olsre <-  lm.cluster(casp ~ married , data = dat, cluster= "cntr")
summary(m.olsre)

#----------------------------------------------------------
# Include country dummies                                                                                                   #
#----------------------------------------------------------
m.ols2 <- lm(casp~married + cntr , data = dat)
summary(m.ols2)   # Cntr #1 used as a reference category (intercept)

m.ols3 <- lm(casp~married + cntr-1, data = dat)
  # "-1" added to remove constant term, so all countries are included 
summary(m.ols3)

#----------------------------------------------------------
# Include interaction with country dummies                                                                                                   #
#----------------------------------------------------------
# Produces slopes-as-outcomes for married X country
m.ols4 <- lm (casp~married + cntr - 1 + married:cntr, data = dat)
summary(m.ols4)




########################################################### 
###                                                       #
###   Multilevel (random effects) models                  #
###                                                       #
###########################################################
#----------------------------------------------------------
# Random Intercept Only - null model                                                                                                       #
#----------------------------------------------------------
  # NOTE:
  # - empty model but usefull to get an overview of variance at the within and between level (and ICC)

m.null <- lmer(casp ~ 1 + (1|cntr), data = dat) 
    # Can also be written without "1"(intercept) in the fixed part

summary(m.null)

fixef(m.null) # Print fixed effects 
ranef(m.null)$cntr # Print random effects (deviations from the fixed)
coef(m.null)$cntr # Print coefficients by country (BLUPs) 
dotplot(ranef(m.null, condVar = TRUE))

# To see the variance part of the lme4 model 
  VarCorr(m.null) # Shows SD from lme4 model 
  print(VarCorr(m.null),comp="Variance") # Shows Variance 
  var.null <- as.data.frame(VarCorr(m.null)) # Save all: SD & Variance 

# Interpretation of the random intercept
  fixef(m.null)+c(-1.96,1.96)*var.null[1,5]
          # 95% of the population is predicted to have individual intercepts 
          # (cluster-means) between 31.86 and 42.40

  
# Compute ICC by hand
  var.null[1,4]/(var.null[1,4]+var.null[2,4])
  
# Or get it from library(sjstats) command
  icc(m.null)

# R2
library("MuMIn")
r.squaredGLMM(m.null)

#----------------------------------------------------------
# Random Intercept (with predictor)                                                                                                         #
#----------------------------------------------------------
  # NOTE:
  # - the basic multilevel model
  # - we allow groups to have different intercepts
  # - but slopes are fixed across groups 

m.ri <- lmer(casp~married + (1|cntr), data = dat)
    
summary(m.ri)
fixef(m.ri)
coef(m.ri)$cntr
ranef(m.ri)
 
### Significance test for this Variance part of the model (lmerTest)
  ranova(m.ri)


#----------------------------------------------------------
# Random Intercept & Slope (Random Coefficient)          
#----------------------------------------------------------
  # NOTE:
  # - Random Intercept & Slope are correlated 
  # - 

m.rc <- lmer(casp~married + (1+married|cntr), data = dat)
  # NOTE: the same as "(married|cntr)"
summary(m.rc)
fixef(m.rc)
coef(m.rc)$cntr
ranef(m.rc) 

### Significance test for this Variance part of the model (lmerTest)
  ranova(m.rc)

  
### Interpretation of the random intercept & slope
dotplot(ranef(m.rc, condVar = TRUE), strip = F) # RE (relative) scale
dotplot(coef(m.rc), scales = "free") # nominal (variable) scale
# Save SD & Variance 
var.rc<-as.data.frame(VarCorr(m.rc)) 
# Intercept
fixef(m.rc)[1]+c(-1.96,1.96)*var.rc[1,5]
          # 95% range of the individual intercepts 
# Slope
fixef(m.rc)[2]+c(-1.96,1.96)*var.rc[2,5]
          # 95% range of the individual intercepts 



### Correlation of random effects 
# (compare it with the result in variance part of the model - it often won't be precise)
cor(ranef(m.rc)$cntr[1], ranef(m.rc)$cntr[2])



### Compute CI for cluster-specific effects 
# Getting SE for coefficients (from "arm" package)
se.fixef(m.rc)
se.ranef(m.rc)

# compute 95% CI for coefficients 
fixef(m.rc)["married"] + c(-1.96,1.96)*se.fixef(m.rc)["married"]
confint(m.rc, oldNames = FALSE) # Estimate CI 

# compute 95% CI for country effects 
coef(m.rc)$cntr["GRE",1] # intercept (compare with coef(m.rc)$cntr)
coef(m.rc)$cntr["GRE",1]+c(-2,2)*se.ranef(m.rc)$cntr["GRE",1] # intercept CI

coef(m.rc)$cntr["GRE",2] # slope (compare with coef(m.rc)$cntr)
coef(m.rc)$cntr["GRE",2]+c(-2,2)*se.ranef(m.rc)$cntr["GRE",2] # slope CI

ranef(m.rc)$cntr["GRE",1] # random effect (deviation) for intercept (compare with ranef(m.rc))
ranef(m.rc)$cntr["GRE",1]+c(-2,2)*se.ranef(m.rc)$cntr["GRE",1]




########################################################### 
###                                                       #
### Model Comparison                                      #
###                                                       #
########################################################### 

# Likelihood ratio tests allow us to compare model fit between nested maximum likelihood models 
library(lmtest)
lrtest(m.ols, m.ri)
lrtest(m.ri, m.rc)

# Comparing models with different fixed effects requires MLE (REML is default in lme4)
# "anova" in lme4 automatically refits models using maximum likelihood estimation (MLE) if necessary 
# It also shows AIC and BIC

anova(m.ri, m.rc)
