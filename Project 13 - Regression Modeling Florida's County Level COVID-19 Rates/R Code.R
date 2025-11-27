# Loading Packages
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(dplyr)
library(ltm)
library(Hmisc)
library(jtools)
library(gmodels)
library(readxl)

# Entering Dataset
COVID_19 <- read_excel("C:/Users/profz/Desktop/COVID_19.xlsx")

# Creating Dichtomous Dependent Variables
COVID_19$Case_rate_d <- ifelse(COVID_19$Case_rate>mean(COVID_19$Case_rate),1,0)
COVID_19$Hosp_rate_d <- ifelse(COVID_19$Hosp_rate>mean(COVID_19$Hosp_rate),1,0)
COVID_19$Death_rate_d <- ifelse(COVID_19$Death_rate>mean(COVID_19$Death_rate),1,0)

# Descriptive Statistics

## Proportion of Counties Above Average Cases, Hospitalizations, and Deaths
COVID_19_c <- COVID_19[,c(2,6:17,18)]
COVID_19_c
COVID_19_h <- COVID_19[,c(2,6:17,19)]
COVID_19_h
COVID_19_d <- COVID_19[,c(2,6:17,20)]
COVID_19_d

## By Case Rate
table(Case_rate_d)
as.list(COVID_19_c %>% group_by(Case_rate_d) %>% summarise_all(mean))
as.list(COVID_19_c %>% group_by(Case_rate_d) %>% summarise_all(sd))
as.list(COVID_19_c %>% group_by(Case_rate_d) %>% summarise_all(ci))

## By Hospitalization Rate
table(Hosp_rate_d)
as.list(COVID_19_h %>% group_by(Hosp_rate_d) %>% summarise_all(mean))
as.list(COVID_19_h %>% group_by(Hosp_rate_d) %>% summarise_all(sd))
as.list(COVID_19_h %>% group_by(Hosp_rate_d) %>% summarise_all(ci))

# By Death Rate
table(Death_rate_d)
as.list(COVID_19_d %>% group_by(Death_rate_d) %>% summarise_all(mean))
as.list(COVID_19_d %>% group_by(Death_rate_d) %>% summarise_all(sd))
as.list(COVID_19_d %>% group_by(Death_rate_d) %>% summarise_all(ci))

# Seeking Best Predictors - Correlation Analysis

## By Case Rate
m_c_c <- rcorr(as.matrix(COVID_19_c))
m_c_c_0 <- split(round(m_c_c$r,3), row(m_c_c$r)[,14])
m_c_c_0
m_c_c_1 <- split(round(m_c_c$P,3), row(m_c_c$P)[,14])
m_c_c_1
data.frame(m_c_c_1[14])
c_names <- c("Population", "Median Income", "%Below Poverty", "%Uninsured","Diabetic","%Black","%Rural","Obesity","Smoking","Median Age", "%Vaccinated For Flu", "%Unemployed","Overcrowding","Case Rate")
p_values_c <- cbind(c_names,data.frame(m_c_c_0[14]), data.frame(m_c_c_1[14]))
colnames(p_values_c) <- c("Variables", "r", "p-value")
p_values_c

## By Hospitalization Rate
m_c_h <- rcorr(as.matrix(COVID_19_h))
m_c_h_0 <- split(round(m_c_h$r,3), row(m_c_h$r)[,14])
m_c_h_0
m_c_h_1 <- split(round(m_c_h$P,3), row(m_c_h$P)[,14])
m_c_h_1
data.frame(m_c_h_1[14])
c_names <- c("Population", "Median Income", "%Below Poverty", "%Uninsured","Diabetic","%Black","%Rural","Obesity","Smoking","Median Age", "%Vaccinated For Flu", "%Unemployed","Overcrowding","Case Rate")
p_values_h <- cbind(c_names, data.frame(m_c_h_0[14]), data.frame(m_c_h_1[14]))
colnames(p_values_h) <- c("Variables", "r", "p-value")
p_values_h

## By Death Rate
m_c_d <- rcorr(as.matrix(COVID_19_d))
m_c_d_0 <- split(round(m_c_d$r,3), row(m_c_d$r)[,14])
m_c_d_0
m_c_d_1 <- split(round(m_c_d$P,3), row(m_c_d$P)[,14])
m_c_d_1
data.frame(m_c_d_1[14])
c_names <- c("Population", "Median Income", "%Below Poverty", "%Uninsured","Diabetic","%Black","%Rural","Obesity","Smoking","Median Age", "%Vaccinated For Flu", "%Unemployed","Overcrowding","Case Rate")
p_values_d <- cbind(c_names, data.frame(m_c_d_0[14]), data.frame(m_c_d_1[14]))
colnames(p_values_d) <- c("Variables", "r", "p-value")
p_values_d

# Logistic Regression

## By Case Rate
model_c <- glm(Case_rate_d ~ 
                 Median_Income+
                 Percent_Black+
                 PercentRural+
                 Obesity+
                 Smoking+
                 PercentVaccinatedForFlu+
                 Overcrowding, data = COVID_19_c, family = binomial) %>%
  stepAIC(trace = FALSE)
summ(model_c, digits = 4, confint = TRUE, exp=TRUE, vifs = TRUE)
(model_c$coefficients/(1+model_c$coefficients))*100

## By Hospitalization Rate
model_h <- glm(Hosp_rate_d ~ 
                 Pop+
                 Median_Income+
                 percentbelowpoverty+
                 PercentUninsured+
                 Diabetic+
                 Percent_Black+
                 PercentRural+
                 Obesity+
                 Smoking+
                 PercentVaccinatedForFlu+
                 PercentUnemployed, data = COVID_19_h, family = binomial) %>%
  stepAIC(trace = FALSE)
summ(model_h, digits = 4, confint = TRUE, exp=TRUE, vifs = TRUE)
(model_h$coefficients/(1+model_h$coefficients))*100

## By Death Rate
model_d <- glm(Death_rate_d ~ 
                 Median_Income+
                 PercentUninsured+
                 Diabetic+
                 PercentRural+
                 Obesity+
                 Smoking+
                 PercentVaccinatedForFlu+
                 PercentUnemployed, data = COVID_19_d, family = binomial) %>%
  stepAIC(trace = FALSE)
summ(model_d, digits = 4, confint = TRUE, exp=TRUE, vifs = TRUE)
(model_d$coefficients/(1+model_d$coefficients))*100


