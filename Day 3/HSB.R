#   |=========================================================================|
#   | Title:              EDSE 2020 - Multilevel Data Analysis                |
#   | Author:             Konrad Turek                                        |
#   | Date:               4-8.05.2020                                         |  
#   |-------------------------------------------------------------------------|
#   | File:     High School and Beyond                                        |  
#   |           - Basic applications -                                        |  
#   |-------------------------------------------------------------------------|
#   |           Data analysed in books:                                        
#   |           - Raudenbush, Bryk (2012) Hierarchical Linear Models    
#   |           - Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models (3rd Ed.), Ch.23  
#   |           Get data: https://socialsciences.mcmaster.ca/jfox/Books/Applied-Regression-3E/datasets/index.html
#   |=========================================================================|

# -------------------------------------------------------------------------
### Introduction
# -------------------------------------------------------------------------
# Level-1: students
# Level-2: schools
# DV: math achievement
# level-1 pred: SES
# level-2 pred: public/catholic
# -------------------------------------------------------------------------
# You can compare the results with Raudenbush, Bryk (2012) & Fox (2016) - below I refere to pages 



###########################################################  
###                                                       #
###   Setup                                               #
###                                                       #
###########################################################  

#----------------------------------------------------------
#      Load Packages          
#----------------------------------------------------------
# if a package is not installed, use install.packages, e.g. install.packages("lme4")
library(lme4)
library(lmerTest) # to get p-value in lme4
library(ggplot2)

# library(arm)
# library(dplyr)
library(lattice) # Useful graphs for multilevel models 
library(performance) # for ICC measures 
library(psych) # For descriptive statistics 


#----------------------------------------------------------
# Loading data
#----------------------------------------------------------
dat<-read.table("Day 3/HSB.txt", header = TRUE)
# Or get online:
dat<-read.table("https://socialsciences.mcmaster.ca/jfox/Books/Applied-Regression-3E/datasets/HSB.txt", header = TRUE)

head(dat)
nrow(dat)
length(unique(dat$school)) # Number of schools
range(table(dat$school)) 	#Min and max number of students per school
mean(table(dat$school)) 	#Average number of students per school

# Correct levels to fit the results in handbooks
dat$sector <- factor(dat$sector, levels=c("Public", "Catholic"))

#----------------------------------------------------------
# Centering 
#----------------------------------------------------------
### Grand-centering - Center individual SES by subtracting the mean SES
# na.rm = TRUE, removes missing values while calculating the mean. Otherwise R returns a missing value (NA)
dat$ses.c <- (dat$ses-mean(dat$ses, na.rm = TRUE)) 
### Group-centering
dat.m <- aggregate(dat = dat, ses ~ school, FUN = mean)
dat <- merge(x = dat, y = dat.m, by = "school", suffixes = c("",".m"))
dat$ses.cc <- (dat$ses-dat$ses.m) # Group-centered variable 
head(dat)
 
########################################################### 
###                                                       #
###   Exploration                                         #
###                                                       #
###########################################################
#----------------------------------------------------------
# Distributions and basic descriptives 
#----------------------------------------------------------
hist(dat$math.achieve)
hist(with(dat, tapply(math.achieve, school, mean))) # histogram of cluster-avarages 

hist(dat$ses)
hist(with(dat, tapply(ses, school, mean))) # histogram of cluster-avarages 




#----------------------------------------------------------
# Plots: ses.cc X math.achieve
#----------------------------------------------------------
# total
ggplot(data  = dat,
       aes(ses.cc, math.achieve))+
  geom_point(position = "jitter")+ # some random noise to see the points better 
  theme_light()+ # colour theme
  geom_smooth(method = lm)  # general fitted loess line  

# add clusters
ggplot(data  = dat,
       aes(ses.cc, math.achieve,
           col = school,
           group = school))+ # colours for classes 
  geom_point(position = "jitter")+
  theme_light()+
  scale_color_gradientn(colours = rainbow(160))+
  theme(legend.position = "none") +
  geom_smooth(method = lm, se = FALSE, size=.3)

# xyplot on a subset 
with(dat[dat$school<2500,], xyplot(math.achieve~ses.cc|school, 
       type = c("p", "g", "r", "smooth")))
    # Alternatively, create the subset separately
    # dat1<-subset(dat, dat$school<2500) # Subset some schools to see the results better  
    #   length(unique(dat1$school)) # Number of schools
    # xyplot(math.achieve~ses.cc|school, dat1,
    #        type = c("p", "g", "r", "smooth"))

# Sector 
bwplot(math.achieve~sector, dat)

xyplot(math.achieve~ses.cc|sector, dat, jitter.x=TRUE, 
       type = c("p", "g", "r","smooth")) 

 

########################################################### 
###                                                       #
###   Baseline linear model (not multilevel)              #
###                                                       #
###########################################################
  
###
m.ols <- lm(math.achieve ~ ses.cc + sector, data = dat)
summary(m.ols)



########################################################### 
###                                                       #
###   Multilevel (random effects) models                  #
###                                                       #
###########################################################
#----------------------------------------------------------
# Random Intercept Only - null model (one-way ANOVA)                                                                                               #
#----------------------------------------------------------
# Raudenbush, Bryk (2012), p.70; Fox (2016), p. 710
  # NOTE:
  # - empty model but useful to get an overview of variance 
  #   at the within and between level (and ICC)
  # - also called Random-effects one-way ANOVA 

###
m.null <- lmer(math.achieve ~ 1 
               + (1|school), data = dat)  # Can also be written without "1"(intercept) in the fixed part 
  
  summary(m.null)

  var.null<-as.data.frame(VarCorr(m.null))
  icc(m.null)

#----------------------------------------------------------
# Random intercept with cluster-averaged SES (Meas-as-Outcomes)                                                                                            #
#----------------------------------------------------------   
# Raudenbush, Bryk (2012), p.73
  
m.ri1 <- lmer(math.achieve ~  ses.m 
               + (1|school), data = dat)
  summary(m.ri1)
  
  var.ri1<-as.data.frame(VarCorr(m.ri1))
  (var.null[1,4]-var.ri1[1,4])/var.null[1,4] 
  # Interpretation: p. 74
  
  icc(m.ri1)

  r2(m.ri1)
  dotplot(ranef(m.ri1, condVar = TRUE), strip = F)


#----------------------------------------------------------
# Random coefficient with SES                                                                                                #
#----------------------------------------------------------
# Raudenbush, Bryk (2012), p.76-79
# Fox(2016), p. 712
###  
m.rc1 <- lmer(math.achieve ~ ses.cc  
               + (1 + ses.cc|school), data = dat)
  summary(m.rc1)
 
  var.rc1<-as.data.frame(VarCorr(m.rc1))
  # Reduction in variance explained at level-1 (residual)
  (var.null[2,4]-var.rc1[4,4])/var.null[2,4] 

  dotplot(ranef(m.rc1, condVar = TRUE), strip = F)

#----------------------------------------------------------
# Random coefficient with SES+Sector (Fox, 2016, p. 714)                                                                                                      #
#----------------------------------------------------------   
  # NOTE
  # Intercepts- and slopes-as-Outcomes Model by Raudenbush and Bryk
  # Coefficients-as-Outcomes Model by Fox(2016) 
  # To get the same results as in the books, Public must be the reference category 

m.rc3 <- lmer(math.achieve ~ ses.cc + ses.m + sector 
              + sector:ses.cc + ses.m:ses.cc 
              + (1 + ses.cc|school), data = dat)

  summary(m.rc3)
  var.ri1<-as.data.frame(VarCorr(m.rc3))
  


########################################################### 
###                                                       #
###   Plot interaction                                    #
###                                                       #
###########################################################

### with  sjPlot
library(sjPlot)
plot_model(m.rc3, type = "int", terms = c("ses.m", "ses.cc"), mdrt.values= "minmax")
plot_model(m.rc3, type = "pred", terms = c("ses.cc", "sector"))



### with effects 
library(effects)
plot(predictorEffects(m.rc3))  
allEffects(m.rc3) #  calculate all effects in a model
int1<-allEffects(m.rc3)[1]  #  calculate only interaction #1
int2<-allEffects(m.rc3)[2]  #  calculate only interaction #2
plot(int1) # plot only interaction 
plot(int1,multiline=TRUE,confint=TRUE,ci.style="bands")
plot(int2,multiline=TRUE,confint=TRUE,ci.style="bands")  



### predicted values and interaction with the categorical var
dat$pred1<- predict(m.rc3)
ggplot(data  = dat,
       aes(ses.cc, pred1,
           col = as.factor(sector)))+
  geom_point(position = "jitter", alpha= .05)+  
  theme_light()+
  geom_smooth(method = lm, se = FALSE, size = 1.5) 


### Plot the between and within effect
# Create 3 categories of schools by their avarage SES level
dat$ses.cat[dat$ses.m< -0.5] <- "A" # Low SES
dat$ses.cat[dat$ses.m>= -0.5 & dat$ses.m<0.5] <-"B" # Medium SES
dat$ses.cat[dat$ses.m>= 0.5] <-"C" # High SES


datP<-subset(dat, dat$sector=="Public") # Subset some schools to see the results better  
datC<-subset(dat, dat$sector=="Catholic") # Subset some schools to see the results better  

ggplot(data  = dat,
       aes(ses.cc, math.achieve))+  
  theme_light()+
  scale_color_gradientn(colours = rainbow(160))+
  theme(legend.position = "none") +
  geom_smooth(aes(group=ses.cat), method = lm, se = TRUE, 
                  size=.3, lty=2, color="navyblue")+
  geom_smooth(aes(ses.m, math.achieve), method =lm , se = FALSE, 
                  size=1, color="maroon")
  


ggplot(data  = dat,
       aes(ses.cc, math.achieve, group=sector))+  
  theme_light()+
  scale_color_gradientn(colours = rainbow(160))+
  theme(legend.position = "none") +
  geom_smooth(aes(group=ses.cat), method = lm, se = TRUE, 
                  size=.3, lty=2, color="navyblue")+
  geom_smooth(aes(ses.m, math.achieve), method =lm , se = FALSE, 
                  size=1, color="maroon")+
  facet_grid(. ~ sector)
