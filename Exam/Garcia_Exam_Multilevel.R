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


