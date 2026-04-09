####################################################################

#Paper title: Hatching failure and embryo mortality in endangered populations of 
#Eurasian Curlew (Numenius arquata)

#Authors: Dr Jamie Edward Thompson & Dr Nicola Hemmings

#Coding developed by: Dr Jamie Edward Thompson, School of Biosciences,
#The University of Sheffield

#Code for embryo mortality occurrence clmm modelling 

####################################################################

#### clean global environment####
rm(list=ls())


#### 0 - Install/Load necessary libraries ####

#install.packages('brms')

library('ordinal')
library('ggplot2')
library('ggpubr')
library('tidyr')
library('tidyverse')
library('performance')
library('sjPlot')


#### 1 - load necessary dataframe and tidy dataframe up (i.e. remove NA data etc...) ####
#NOTE: set working directory with following:
#setwd("C:/Users/jamie/Documents/UoS_work/PDRA_Hemmings.research.group.work/
#CURLEW.PAPER.1_Patterns.of.hatching.failure/dataset")

#load dataset
Main.Curlew.embryo_modelling.dataset <-
  data.frame(read.csv("Main.Curlew.hatching.embryo_modelling.dataset.csv"))

#omit Ordinal.embryo.staging NA data
Main.Curlew.embryo_modelling.dataset.Cooney.et.al.Phase.category.NA.omit <- Main.Curlew.embryo_modelling.dataset %>%
  subset(!is.na(Cooney.et.al.Phase.category))

unique(Main.Curlew.embryo_modelling.dataset.Cooney.et.al.Phase.category.NA.omit$Cooney.et.al.Phase.category) #check NA omitted

#omit NA from county factor
Main.Curlew.embryo_modelling.dataset.county.NA.omit <- Main.Curlew.embryo_modelling.dataset.Cooney.et.al.Phase.category.NA.omit %>%
  subset(!is.na(county))

unique(Main.Curlew.embryo_modelling.dataset.county.NA.omit$county) #check NA omitted

#remove hatched eggs from datasets 
Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland<- 
  subset(Main.Curlew.embryo_modelling.dataset.Cooney.et.al.Phase.category.NA.omit,
         Analyses.Failed.egg.category!='Hatched')

Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county<- 
  subset(Main.Curlew.embryo_modelling.dataset.county.NA.omit,
         Analyses.Failed.egg.category!='Hatched')

#Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL <- Main.Curlew.embryo_modelling.dataset.Cooney.et.al.Phase.category.NA.omit
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county$Analyses.Failed.egg.category) #check failed and abandoned eggs only included 
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county$county)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county$Cooney.et.al.Phase.category)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland$Analyses.Failed.egg.category) #check failed and abandoned eggs only included 
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland$county)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland$Cooney.et.al.Phase.category)

#create datasets with abandoned eggs removed
Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only<- 
  subset(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland,
         Analyses.Failed.egg.category!='Abandoned')

Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only<- 
  subset(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county,
         Analyses.Failed.egg.category!='Abandoned')

unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only$Analyses.Failed.egg.category) #check failed eggs only included
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only$county)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only$Cooney.et.al.Phase.category)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Analyses.Failed.egg.category) #check failed eggs only included
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$county)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Cooney.et.al.Phase.category)

#create WILD CLUTCHES only datasets
#failed only unhatched eggs dataset
Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_WILD.Failed.only<- 
  subset(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only,
         Incubation.type!='Headstart')
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_WILD.Failed.only$Analyses.Failed.egg.category) #check hatched and failed eggs only included
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_WILD.Failed.only$Incubation.type) #check Wild clutches only included

#create HEADSTART CLUTCHES only datasets
#failed only unhatched eggs dataset
Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_HEADSTART.Failed.only<- 
  subset(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_Failed.only,
         Incubation.type!='Wild')
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_HEADSTART.Failed.only$Analyses.Failed.egg.category) #check hatched and failed eggs only included
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.county_HEADSTART.Failed.only$Incubation.type) #check Headstart clutches only included

#### 2 -  Model run ####
#Using an Ordinal Regression Model

#####
#QUESTION: What is the effect of incubation type (‘Headstart’ and ‘Wild’) on observed embryo mortality outcomes in the United Kingdom 
#and Republic of Ireland Eurasian Curlew population?

#factorise Project.Clutch.ID

Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Project.clutch.ID <- 
  factor(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Project.clutch.ID)

str(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only)

#make sure Cooney.et.al.Phase.category is an ordered factor
Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Cooney.et.al.Phase.category <- 
  factor(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Cooney.et.al.Phase.category, 
         levels = c("Phase.1", "Phase.2", "Phase.3"), 
         ordered = TRUE)
unique(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only$Cooney.et.al.Phase.category)
str(Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only)

####
#Model run
#set seed to ensure reproducibility of results
set.seed(42)
mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL <- ordinal::clmm(Cooney.et.al.Phase.category  ~ Incubation.type +
                                                        (1 | Project.clutch.ID), 
                                                      link = "logit",
                                                      nAGQ=53,
                                                      control=ordinal::clmm.control(method = "nlminb", maxIter = 2e5),
                                                      data = Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only)

summary(mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL) 
# Calculate Bootstrapped CIs 
#set seed to ensure reproducibility of results
set.seed(42)
confint(mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL, 
        method = "boot", nsim = 1000)
#Calculate marginal and conditional r2
performance::r2_nakagawa(mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL)

##MODEL CHECKS##
#performance::check_model(mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL, residual_type = "pearson", panel = TRUE) #NOT WORKING???

#check likelihood ratio test p-values for removal of random effect from the model
mlr.clm.allUKandIreland.only.Failed.FULL.MODEL <- ordinal::clm(Cooney.et.al.Phase.category  ~  Incubation.type, 
                                                    link = "logit",
                                                    nAGQ= 53,
                                                    data = Main.Curlew.embryo_modelling.dataset.NA.omit.FINAL.allUKandIreland_Failed.only)

anova(mlr.clmm.allUKandIreland.only.Failed.FULL.MODEL, mlr.clm.allUKandIreland.only.Failed.FULL.MODEL) #p = 1.654e-05 *** -> likelihood ratio test for removal of random effect 

#cannot do most of the assumption checks on clmm run on clm version of model
#and run checks
ordinal::nominal_test(mlr.clm.allUKandIreland.only.Failed.FULL.MODEL)
ordinal::scale_test(mlr.clm.allUKandIreland.only.Failed.FULL.MODEL) 

#visual probability plot of final model
plot_model(mlr.clm.allUKandIreland.only.Failed.FULL.MODEL, type = "pred", terms = "Incubation.type[all]", bias_correction = TRUE)
