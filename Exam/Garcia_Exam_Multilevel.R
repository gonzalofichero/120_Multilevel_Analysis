# Loading libraries
library(lme4)
library(lmerTest) # to get p-value in lme4
library(tidyverse)
library(ggplot2)
library(lattice) # Useful graphs for multilevel models 
library(performance) # for ICC measures 
library(psych) # For descriptive statistics 
library(readstata13) # to read data in dta


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




