# Loading libraries
library(lme4)
library(lmerTest) # to get p-value in lme4
library(tidyverse)
library(ggplot2)
library(lattice) # Useful graphs for multilevel models 
library(performance) # for ICC measures 
library(psych) # For descriptive statistics 
library(readstata13) # to read data in dta
library(sjPlot) # plot interaction within-between


# Loading data
math <- read.dta13("Exam/Exam21.dta")

# Checking data
glimpse(math)


# Let's check for the amount of schools and kids
length(unique(math$schid))
length(unique(paste(math$stuid,"-",math$schid))) # 23 schools, 519 students
# The Student ID is repeated through schools... Maybe creating a new ID


# Let's do some quick plots and see correlations between covariates
# We are gonna focus on: ses, homework (as control), sex, race, public (contextual), ratio (as control contextual)
# We need to find literature about this!


#----------------------------------------------------------
# Distributions and basic descriptive 
#----------------------------------------------------------
hist(math$math)

densityplot(~ math, groups = schid, math,
            plot.points = FALSE) 



#----------------------------------------------------------
# Plots: math ~ ses
#----------------------------------------------------------
ggplot(data  = math,
       aes(ses, math,
           col = schid,
           group = schid)) + # colours for schools 
  geom_point(position = "jitter")+
  theme_light()+
  scale_color_gradientn(colours = rainbow(23))+
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size=.3)



#----------------------------------------------------------
# Plots: math ~ homework
#----------------------------------------------------------
ggplot(data  = math,
       aes(homework, math,
           col = schid,
           group = schid)) + # colours for schools 
  geom_point(position = "jitter")+
  theme_light()+
  scale_color_gradientn(colours = rainbow(23))+
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size=.3)


#----------------------------------------------------------
# Plots: math ~ ratio
#----------------------------------------------------------
ggplot(data  = math,
       aes(ratio, math,
           col = schid,
           group = schid)) + # colours for schools 
  geom_point(position = "jitter")+
  theme_light() +
  scale_color_gradientn(colours = rainbow(23))+
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size=.3)



#----------------------------------------------------------
# Plots: math ~ sex
#----------------------------------------------------------
ggplot(data  = math,
       aes(as.factor(sex), math)) + # colours for schools 
  geom_boxplot() +
  theme_light() +
  facet_wrap(~ schid)


#----------------------------------------------------------
# Plots: math ~ race
#----------------------------------------------------------
ggplot(data  = math,
       aes(as.factor(white), math)) + # colours for schools 
  geom_boxplot() +
  theme_light() +
  facet_wrap(~ schid)


#----------------------------------------------------------
# Plots: math ~ public
#----------------------------------------------------------
ggplot(data  = math,
       aes(x = math, col=as.factor(public))) + # colours for schools 
  geom_density()+
  theme_light()


########################################################### 
###                                                       #
###   Confounding: SES ~?                                 #
###                                                       #
###########################################################
summary(lm(ses ~ sex + white + parented, data = math))

# Can't use race since it's confounding with SES. Same with Parent Education.


#----------------------------------------------------------
# Centering SES: for individual and for school
#----------------------------------------------------------
### Grand-centering - Center individual SES by subtracting the mean SES
math$ses.c <- (math$ses - mean(math$ses, na.rm = TRUE)) 

### Group-centering
dat.m <- aggregate(dat = math, ses ~ schid, FUN = mean)
math <- merge(x = math, y = dat.m, by = "schid", suffixes = c("",".m"))

math$ses.cc <- (math$ses - math$ses.m) # Group-centered variable 
head(math) # Check


#----------------------------------------------------------
# Centering HOMEWORK: for individual and for school
#----------------------------------------------------------
### Grand-centering - Center individual SES by subtracting the mean SES
math$hwk.c <- (math$homework - mean(math$homework, na.rm = TRUE)) 

### Group-centering
dat.h <- aggregate(dat = math, homework ~ schid, FUN = mean)
math <- merge(x = math, y = dat.h, by = "schid", suffixes = c("",".m"))

math$hwk.cc <- (math$homework - math$homework.m) # Group-centered variable 
head(math) # Check



########################################################### 
###                                                       #
###   Baseline linear model (not multilevel)              #
###                                                       #
###########################################################

### Running OLS model for no-cluster covariates
m.ols <- lm(math ~ ses + sex + homework + public + ratio, data = math)
summary(m.ols)



########################################################### 
###                                                       #
###   Multilevel (random effects) models                  #
###                                                       #
###########################################################


#----------------------------------------------------------
# Random Intercept Only - null model (one-way ANOVA)                                                                                               #
#----------------------------------------------------------
###
m.null <- lmer(math ~ 1 
               + (1|schid), data = math)  # Can also be written without "1"(intercept) in the fixed part 

summary(m.null)

var.null <- as.data.frame(VarCorr(m.null))
icc(m.null)

# Almost 25% of variation explained by difference between clusters -> we need multilevel, for sure...

fixef(m.null) # Print fixed effects 
coef(m.null)$schid # Print coefficients by school 
ranef(m.null)$schid # Print random effects 
dotplot(ranef(m.null, condVar = TRUE))  # plot random intercepts

# Check normality
qqnorm(resid(m.null),col="blue",lwd=2)


#----------------------------------------------------------
# Random coefficient: Only SES as level-1                                                                                           #
#----------------------------------------------------------
###  
m.rc1 <- lmer(math ~ ses + public + ratio +
                     + (1 + ses|schid), data = math)

summary(m.rc1)

icc(m.rc1)
r2(m.rc1)


#----------------------------------------------------------
# Random coefficient: Only Homework as level-1                                                                                           #
#----------------------------------------------------------
###  
m.rc2 <- lmer(math ~ homework + public + ratio +
                + (1 + homework|schid), data = math)

summary(m.rc2)

icc(m.rc2)
r2(m.rc2)


#----------------------------------------------------------
# Random coefficient: Homework + SES as level-1                                                                                           #
#----------------------------------------------------------
###  
m.rc3 <- lmer(math ~ homework + ses + public + ratio +
                + (1 + ses |schid), data = math)

summary(m.rc3)

icc(m.rc3)
r2(m.rc3)


#----------------------------------------------------------
# Random coefficient with SES random slope                                                                                             #
#----------------------------------------------------------
###  
m.complete <- lmer(math ~ ses + sex + homework + public + ratio +
              + (1 + ses |schid), data = math)

summary(m.complete)

icc(m.complete)
r2(m.complete)

# Almost 18% of variation explained by difference between clusters
# Contextual covariates no important

fixef(m.complete) # Print fixed effects 
coef(m.complete)$schid # Print coefficients by school 
ranef(m.complete)$schid # Print random effects 
dotplot(ranef(m.complete, condVar = TRUE))  # plot random intercepts

# Check normality
qqnorm(resid(m.complete),col="blue",lwd=2)

### Significance test for this Variance part of the model (lmerTest)
ranova(m.complete)


### predicted values and interaction with the SES var
math$pred1<- predict(m.complete)

ggplot(data  = math,
       aes(ses, pred1,
           col = as.factor(schid))) +
  geom_point(position = "jitter", alpha= .05) +  
  theme_light()+
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size = 1.5) 

# Now you can compare with initial values and check that this is working right now





#----------------------------------------------------------
# Within-Between Model                                                                                                      #
#----------------------------------------------------------   
###
m.rc2 <- lmer(math ~ ses.cc + ses.m + sex + homework + public + ratio + 
              + (1 + ses.cc|schid), data = math)

summary(m.rc2)

var.ri2 <- as.data.frame(VarCorr(m.rc2)) 

icc(m.rc2)
r2(m.rc2)

# Almost 16% of variation explained by difference between clusters
# Contextual covariates no important

fixef(m.rc2) # Print fixed effects 
coef(m.rc2)$schid # Print coefficients by school 
ranef(m.rc2)$schid # Print random effects 
dotplot(ranef(m.rc2, condVar = TRUE))  # plot random intercepts


# Check normality
qqnorm(resid(m.rc2),col="blue",lwd=2)

### with  sjPlot: Plot Interaction
plot_model(m.rc2, type = "int", terms = c("ses.m", "ses.cc"), mdrt.values= "minmax")
plot_model(m.rc2, type = "pred", terms = c("ses.cc", "schid"))



########################################################### 
###                                                       #
### Model Comparison                                      #
###                                                       #
########################################################### 

# Comparing models with different fixed effects requires MLE (REML is default in lme4)
# "anova" in lme4 automatically refits models using maximum likelihood estimation (MLE) if necessary 
# It also shows AIC and BIC
anova(m.null, m.rc1)
anova(m.rc1, m.rc2)

# Also checking AIC and BIC of all models
AIC(m.ols, m.null, m.rc1, m.rc2)
BIC(m.ols, m.null, m.rc1, m.rc2)
