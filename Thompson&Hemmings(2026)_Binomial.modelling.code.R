####################################################################

#Paper title: Hatching failure and embryo mortality in endangered populations of 
#Eurasian Curlew (Numenius arquata)

#Authors: Dr Jamie Edward Thompson & Dr Nicola Hemmings

#Coding developed by: Dr Jamie Edward Thompson, School of Biosciences,
#The University of Sheffield

#Code for hatching outcome binomial modelling 

####################################################################


#### clean global environment####
rm(list=ls())


#### 0 - Install/Load necessary libraries ####

#install.packages('lme4')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('ggpubr')
#install.packages('tidyr')
#install.packages('tidyverse')
#install.packages('ggspatial')
#install.packages('glmulti')
#install.packages('sjPlot')
#install.packages('rms')
#install.packages('Hmisc')
#install.packages('report')
#install.packages('DHARMa')
#install.packages('broom.mixed')
#install.packages('performance')

library('lme4')
library('dplyr')
library('ggplot2')
library('ggpubr')
library('tidyr')
library('tidyverse')
library('ggspatial')
library('glmulti')
library('sjPlot')
library('rms')
library('Hmisc')
library('report')
library('DHARMa')
library('broom.mixed')
library('performance')

#### 1 - load necessary dataframe and tidy dataframe up (i.e. remove NA data etc...) ####
#NOTE: set working directory with following:
#setwd("C:/Users/jamie/Documents/UoS_work/PDRA_Hemmings.research.group.work/
#CURLEW.PAPER.1_Patterns.of.hatching.failure/dataset")

#load overall modelling dataset
Main.Curlew.hatching.embryo_modelling.dataset <-
  data.frame(read.csv("Main.Curlew.hatching.embryo_modelling.dataset.csv"))

#create an overall modelling dataset with abandoned eggs removed
Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only<- 
  subset(Main.Curlew.hatching.embryo_modelling.dataset,
         Analyses.Failed.egg.category!='Abandoned')

unique(Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only$Analyses.Failed.egg.category) #check failed and hatched eggs only present in this subset of the data

##for county level analyses##
#omit NA from county factor
Main.Curlew.hatching.embryo_modelling.dataset.NA.omit <- Main.Curlew.hatching.embryo_modelling.dataset %>%
  drop_na(county)

unique(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit$county) #check NA omitted

#create a county dataset with abandoned eggs removed
Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_Failed.Hatched.only<- 
  subset(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit,
         Analyses.Failed.egg.category!='Abandoned')

unique(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_Failed.Hatched.only$Analyses.Failed.egg.category) #check failed and hatched eggs only present in this subset of the data

#create a county dataset with wild clutches only with abandoned eggs included
Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD<- 
  subset(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit,
         Incubation.type=='Wild')

unique(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD$Incubation.type) #check wild clutches only present

#create a county dataset with wild clutches and failed and hatched eggs only
Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only<- 
  subset(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD,
         Analyses.Failed.egg.category!='Abandoned')

unique(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only$Analyses.Failed.egg.category) #check failed and hatched eggs only included

#create a county dataset with headstart clutches only
Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART<- 
  subset(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit,
         Incubation.type=='Headstart')

unique(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART$Incubation.type) #check headstart clutches only present

#### 2 - Mixed-effects Binomial Logistic Regression model run ####
#Using the glmer function in lme4

#####
#QUESTION: What is the effect of incubation type (‘Headstart’ and ‘Wild’) on observed hatching outcomes in the United Kingdom 
#and Republic of Ireland Eurasian Curlew population?
#Random effect: Project.clutch.ID
#Fixed effects: Breeding.year interaction with incubation.type (both variables factorised)
#Dependent response variable: Hatching.outcome.binomial (1 = Hatched vs 0 = Unhatched)

#Failed and hatched eggs only model runs
#factorise Incubation.type variable and Breeding.year and Project.Clutch.ID
Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only %>%
  mutate(across(Incubation.type, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only %>%
  mutate(across(Breeding.year, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only %>%
  mutate(across(Project.clutch.ID, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only %>%
  mutate(across(Hatching.outcome.binomial, as.factor))

str(Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only)

# Model run
#set seed to ensure reproducibility of results
set.seed(42)

mlr.glmer.incubation.type.failedandhatchedonly <- glmer(Hatching.outcome.binomial ~  Incubation.type +
                                           (1 | Project.clutch.ID),
                                           nAGQ=53,
                                         family=binomial(link = "logit"),
                                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                                         data = Main.Curlew.hatching.embryo_modelling.dataset_Failed.Hatched.only)

summary(mlr.glmer.incubation.type.failedandhatchedonly)

# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.glmer.incubation.type.failedandhatchedonly, 
                   method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.glmer.incubation.type.failedandhatchedonly)
#summarise report for model
report::report(mlr.glmer.incubation.type.failedandhatchedonly)

#Final model assumption and fit checking#

#DHARMa package model checks on simulated residuals
sim.resid.mlr.glmer.incubation.type.failedandhatchedonly <- simulateResiduals(fittedModel = mlr.glmer.incubation.type.failedandhatchedonly, refit = T)
testZeroInflation(sim.resid.mlr.glmer.incubation.type.failedandhatchedonly) #no issue detected
testDispersion(sim.resid.mlr.glmer.incubation.type.failedandhatchedonly) #no issue detected
testOutliers(sim.resid.mlr.glmer.incubation.type.failedandhatchedonly) #n.s. outlier test
testUniformity(sim.resid.mlr.glmer.incubation.type.failedandhatchedonly) #n.s. uniformity test


#Model including abandoned eggs
#factorise Incubation.type variable and Breeding.year and Project.Clutch.ID
Main.Curlew.hatching.embryo_modelling.dataset <- 
  Main.Curlew.hatching.embryo_modelling.dataset %>%
  mutate(across(Incubation.type, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset <- 
  Main.Curlew.hatching.embryo_modelling.dataset %>%
  mutate(across(Breeding.year, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset <- 
  Main.Curlew.hatching.embryo_modelling.dataset %>%
  mutate(across(Project.clutch.ID, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset <- 
  Main.Curlew.hatching.embryo_modelling.dataset %>%
  mutate(across(Hatching.outcome.binomial, as.factor))

str(Main.Curlew.hatching.embryo_modelling.dataset)

# Model run
#set seed to ensure reproducibility of results
set.seed(42)
mlr.glmer.incubation.type.abandoned.included <- glmer(Hatching.outcome.binomial ~  Incubation.type +
                                                          (1 | Project.clutch.ID),
                                                        nAGQ=53,
                                                        family=binomial(link = "logit"),
                                                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                                                        data = Main.Curlew.hatching.embryo_modelling.dataset)

summary(mlr.glmer.incubation.type.abandoned.included)

# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.glmer.incubation.type.abandoned.included, 
        method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.glmer.incubation.type.abandoned.included)
#summarise report for model
report::report(mlr.glmer.incubation.type.abandoned.included)

#QUESTION: What is the effect of latitude, longitude and/or breeding year on observed hatching outcomes in the United Kingdom 
#and Republic of Ireland Eurasian Curlew population? 
#Random effects: Project.clutch.ID (to account for pseudoreplication due to presence 
#of egg samples from the same clutch and to account for unquantified possible maternal,
#genetic, behavioural and localised environmental effects not accounted for) 

#Fixed effects: Latitude and longitude (continuous data) and 
#Breeding.year (2022, 2023, 2024 - factorised variable)

#Dependent response variable: Hatching.outcome.binomial (1 = Hatched vs 0 = Unhatched)


#### TESTING WILD CLUTCHES ONLY EXCLUDING ABANDONED EGGS ####

#factorise Breeding.year and Project.Clutch.ID

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only %>%
  mutate(across(Breeding.year, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only %>%
  mutate(across(Project.clutch.ID, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only %>%
  mutate(across(Hatching.outcome.binomial, as.factor))

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only)

#centre/scale continuous predictor variables before model selection

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only$Region.central.latitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only$Region.central.latitude.coordinate)

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only$Region.central.longitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only$Region.central.longitude.coordinate)

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only)

# Model run
#set seed to ensure reproducibility of results
set.seed(42)
mlr.glmer.WILD.failed.hatched.only.MODEL <- glmer(Hatching.outcome.binomial ~ Region.central.latitude.coordinate + 
                                      Region.central.longitude.coordinate + Breeding.year +
                                     (1 | Project.clutch.ID),
                                   nAGQ=53,
                                   family=binomial(link = "logit"),
                                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                                   data = Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD.failed.hatched.only)

summary(mlr.glmer.WILD.failed.hatched.only.MODEL)
# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.glmer.WILD.failed.hatched.only.MODEL, 
        method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.glmer.WILD.failed.hatched.only.MODEL)
#summarise report for model
report::report(mlr.glmer.WILD.failed.hatched.only.MODEL)

####

#Final model assumption and fit checking#

#DHARMa package model checks on simulated residuals
sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL <- simulateResiduals(fittedModel = mlr.glmer.WILD.failed.hatched.only.MODEL, refit = T)
testZeroInflation(sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL) #no issue detected
testDispersion(sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL) #no issue detected
testOutliers(sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL) #n.s. outlier test
testUniformity(sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL) #n.s. uniformity test
testQuantiles(sim.resid.mlr.glmer.WILD.failed.hatched.only.MODEL) #no significant problems detected

#### TESTING WILD CLUTCHES ONLY INCLUDING ABANDONED EGGS ####

#factorise Breeding.year and Project.Clutch.ID

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD %>%
  mutate(across(Breeding.year, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD %>%
  mutate(across(Project.clutch.ID, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD %>%
  mutate(across(Hatching.outcome.binomial, as.factor))

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD)

#centre/scale continuous predictor variables before model selection

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD$Region.central.latitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD$Region.central.latitude.coordinate)

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD$Region.central.longitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD$Region.central.longitude.coordinate)

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD)

# Model run
#set seed to ensure reproducibility of results
set.seed(42)
mlr.glmer.WILD.including.Abandoned.MODEL <- glmer(Hatching.outcome.binomial ~ Region.central.latitude.coordinate + 
                                                    Region.central.longitude.coordinate + Breeding.year +
                                                    (1 | Project.clutch.ID),
                                                  nAGQ=53,
                                                  family=binomial(link = "logit"),
                                                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                                                  data = Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_WILD)

summary(mlr.glmer.WILD.including.Abandoned.MODEL)
# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.glmer.WILD.including.Abandoned.MODEL, 
        method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.glmer.WILD.including.Abandoned.MODEL)
#summarise report for model
report::report(mlr.glmer.WILD.including.Abandoned.MODEL)

####

#Final model assumption and fit checking#

#DHARMa package model checks on simulated residuals
sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL <- simulateResiduals(fittedModel = mlr.glmer.WILD.including.Abandoned.MODEL, refit = T)
testZeroInflation(sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL) #no issue detected
testDispersion(sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL) #no issue detected
testOutliers(sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL) #n.s. outlier test
testUniformity(sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL) #n.s. uniformity test
testQuantiles(sim.resid.mlr.glmer.WILD.including.Abandoned.MODEL) #no significant problems detected

#### TESTING HEADSTART CLUTCHES ONLY WITH HATCHED AND FAILED EGGS ####

#factorise Breeding.year and Project.Clutch.ID

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART %>%
  mutate(across(Breeding.year, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART %>%
  mutate(across(Project.clutch.ID, as.factor))

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART <- 
  Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART %>%
  mutate(across(Hatching.outcome.binomial, as.factor))

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART)

#centre/scale continuous predictor variables before model selection

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART$Region.central.latitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART$Region.central.latitude.coordinate)

Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART$Region.central.longitude.coordinate <- 
  scale(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART$Region.central.longitude.coordinate)

str(Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART)

#Model run
#set seed to ensure reproducibility of results
set.seed(42)
mlr.glmer.HEADSTART.MODEL <- glmer(Hatching.outcome.binomial ~ Region.central.latitude.coordinate + Region.central.longitude.coordinate +
                                 Breeding.year + (1 | Project.clutch.ID), 
                              family=binomial(link = "logit"),
                              nAGQ=53,
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              data = Main.Curlew.hatching.embryo_modelling.dataset.NA.omit_HEADSTART)

summary(mlr.glmer.HEADSTART.MODEL)
# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.glmer.HEADSTART.MODEL, 
        method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.glmer.HEADSTART.MODEL)
#summarise report for model
report::report(mlr.glmer.HEADSTART.MODEL)

####
#Final model assumption and fit checking#

#DHARMa package model checks on simulated residuals
sim.resid.mlr.glmer.HEADSTART.MODEL <- simulateResiduals(fittedModel = mlr.glmer.HEADSTART.MODEL, refit = T)
testZeroInflation(sim.resid.mlr.glmer.HEADSTART.MODEL) #no issue detected
testDispersion(sim.resid.mlr.glmer.HEADSTART.MODEL) #no issue detected
testOutliers(sim.resid.mlr.glmer.HEADSTART.MODEL) #n.s. outlier test
testUniformity(sim.resid.mlr.glmer.HEADSTART.MODEL) #n.s. uniformity test
testQuantiles(sim.resid.mlr.glmer.HEADSTART.MODEL) #no significant problems detected