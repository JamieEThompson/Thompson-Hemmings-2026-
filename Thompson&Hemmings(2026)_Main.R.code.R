####################################################################

#Paper title: Hatching failure and embryo mortality in endangered populations of 
#Eurasian Curlew (Numenius arquata)

#Authors: Dr Jamie Edward Thompson & Dr Nicola Hemmings

#Coding developed by: Dr Jamie Edward Thompson, School of Biosciences,
#The University of Sheffield

#Code for general summary data & graphical presentations 

####################################################################

#clean global environment
rm(list=ls())

##################################
# 0 - Install/Load necessary libraries
##################################

#install.packages('rptR')
#install.packages('lme4')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('ggpubr')
#install.packages('tidyr')
#install.packages('tidyverse')
#install.packages('sf')
#install.packages('ggspatial')

library('rptR')
library('lme4')
library('dplyr')
library('ggplot2')
library('ggpubr')
library('tidyr')
library('tidyverse')
library('sf')
library('ggspatial')

##################################
# 1 - load necessary dataframe
#NOTE: set working directory with following:
#setwd("C:/Users/jamie/Documents/UoS_work/PDRA_Hemmings.research.group.work/
#CURLEW.PAPER.1_Patterns.of.hatching.failure/dataset")
##################################

Main.Curlew.hatching.embryo.dataset <-
  data.frame(read.csv("Main.Curlew.hatching.embryo.dataset.csv"))

##################################
# 2 - data tidying and structuring for analyses

#############
#HATCHING FAILURE DATA
####

###summary hatching failure for headstart vs wild###

#UK and Ireland grouped
#across all breeding seasons
Curlew.hatching.failure.Incubation.type.entire.UK.Ireland <- 
  Main.Curlew.hatching.embryo.dataset %>%
  group_by(Incubation.type) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)

#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.entire.UK.Ireland_breeding.seasons.separate <- 
  Main.Curlew.hatching.embryo.dataset %>%
  group_by(Incubation.type, Breeding.year) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)

#Group by Incubation type and country
#Combined breeding years
Curlew.hatching.failure.Incubation.type.Country <- 
  Main.Curlew.hatching.embryo.dataset %>%
  group_by(Incubation.type, Region.Country) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                        +sum(Clutch.no.eggs.Failed)
                                                        +sum(Clutch.no.eggs.Abandoned)
                                                        +sum(Clutch.no.eggs.Predated)
                                                        +sum(Clutch.no.eggs.Damaged)
                                                        +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)))*100)

#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.Country.breeding.seasons.separated <- 
  Main.Curlew.hatching.embryo.dataset %>%
  group_by(Incubation.type, Region.Country, Breeding.year) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)
#Group by Incubation type and county
#omit NA county data
Curlew.hatching.failure.RAW.county.NA.omitted<- 
  subset(Main.Curlew.hatching.embryo.dataset,
         county!='NA')

#Combined breeding years
Curlew.hatching.failure.Incubation.type.County <- 
  Curlew.hatching.failure.RAW.county.NA.omitted %>%
  group_by(Incubation.type, county) %>%
  summarise(Region.County = unique(Region.County),
            Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)

#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.County.breeding.seasons.separated <- 
  Curlew.hatching.failure.RAW.county.NA.omitted %>%
  group_by(Incubation.type, county, Breeding.year) %>%
  summarise(Region.County = unique(Region.County),
            Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)

#Group by Incubation type and Breeding habitat type
#omit NA county data
Curlew.hatching.failure.RAW.habitat.type.NA.omitted<- 
  subset(Main.Curlew.hatching.embryo.dataset,
         Analyses.breeding.habitat.type!='NA')

#entire UK & Ireland 
#combined breeding season years
Curlew.hatching.failure.Incubation.type.land.use.entire.UK.Ireland <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)


#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.land.use.entire.UK.Ireland_breeding.seasons.separate <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type, Breeding.year) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)
                                                                                                       
#separate countries
#combined breeding season years
Curlew.hatching.failure.Incubation.type.Country.land.use <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, Region.Country, Analyses.breeding.habitat.type) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)


#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.Country.land.use_breeding.seasons.separate <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, Region.Country, Analyses.breeding.habitat.type, Breeding.year) %>%
  summarise(total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)

#separate counties
#combined breeding season years
Curlew.hatching.failure.Incubation.type.County.land.use <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, county, Analyses.breeding.habitat.type) %>%
  summarise(Region.County = unique(Region.County),
            Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)


#Separate breeding seasons
Curlew.hatching.failure.Incubation.type.County.land.use_breeding.seasons.separate <- 
  Curlew.hatching.failure.RAW.habitat.type.NA.omitted %>%
  group_by(Incubation.type, county, Analyses.breeding.habitat.type, Breeding.year) %>%
  summarise(Region.County = unique(Region.County),
            Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(Project.clutch.ID),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_eggs = (sum(Clutch.no.eggs.Hatched)
                          +sum(Clutch.no.eggs.Failed)
                          +sum(Clutch.no.eggs.Abandoned)
                          +sum(Clutch.no.eggs.Predated)
                          +sum(Clutch.no.eggs.Damaged)
                          +sum(Clutch.no.eggs.Unknown)),
            total_hatched=sum(Clutch.no.eggs.Hatched),
            total_failed=sum(Clutch.no.eggs.Failed),
            total_abandoned=sum(Clutch.no.eggs.Abandoned),
            total_predated=sum(Clutch.no.eggs.Predated),
            total_damaged=sum(Clutch.no.eggs.Damaged),
            total_unknown=sum(Clutch.no.eggs.Unknown),
            percentage_hatched=(sum(Clutch.no.eggs.Hatched)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_failed=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                           +sum(Clutch.no.eggs.Failed)
                                                           +sum(Clutch.no.eggs.Abandoned)
                                                           +sum(Clutch.no.eggs.Predated)
                                                           +sum(Clutch.no.eggs.Damaged)
                                                           +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_abandoned=(sum(Clutch.no.eggs.Abandoned)/(sum(Clutch.no.eggs.Hatched)
                                                                 +sum(Clutch.no.eggs.Failed)
                                                                 +sum(Clutch.no.eggs.Abandoned)
                                                                 +sum(Clutch.no.eggs.Predated)
                                                                 +sum(Clutch.no.eggs.Damaged)
                                                                 +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_predated=(sum(Clutch.no.eggs.Predated)/(sum(Clutch.no.eggs.Hatched)
                                                               +sum(Clutch.no.eggs.Failed)
                                                               +sum(Clutch.no.eggs.Abandoned)
                                                               +sum(Clutch.no.eggs.Predated)
                                                               +sum(Clutch.no.eggs.Damaged)
                                                               +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_damaged=(sum(Clutch.no.eggs.Damaged)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            percentage_unknown=(sum(Clutch.no.eggs.Unknown)/(sum(Clutch.no.eggs.Hatched)
                                                             +sum(Clutch.no.eggs.Failed)
                                                             +sum(Clutch.no.eggs.Abandoned)
                                                             +sum(Clutch.no.eggs.Predated)
                                                             +sum(Clutch.no.eggs.Damaged)
                                                             +sum(Clutch.no.eggs.Unknown)))*100,
            traditional.hatching.failure=(sum(Clutch.no.eggs.Failed)/(sum(Clutch.no.eggs.Hatched)
                                                                      +sum(Clutch.no.eggs.Failed)))*100,
            recalculated_hatching.failure=((sum(Clutch.no.eggs.Failed)+sum(Clutch.no.eggs.Abandoned))/(sum(Clutch.no.eggs.Hatched)
                                                                                                       +sum(Clutch.no.eggs.Failed)
                                                                                                       +sum(Clutch.no.eggs.Abandoned)))*100)
#############
#EMBRYO MORTALITY OCCURRENCE DATA
####

#summarise Headstart vs Wild

#Embryo mortality data
#load dataset
Main.Curlew.embryo_modelling.dataset <-
  data.frame(read.csv("Main.Curlew.hatching.embryo_modelling.dataset.csv"))

#create a dataset with abandoned eggs removed and only fertile eggs confirmed
Main.Curlew.embryo_modelling.dataset_Failed.only<- 
  subset(Main.Curlew.embryo_modelling.dataset,
         Analyses.Failed.egg.category!='Abandoned')

unique(Main.Curlew.embryo_modelling.dataset_Failed.only$Analyses.Failed.egg.category)

Main.Curlew.embryo_modelling.dataset_Failed.only_fertile<- 
  subset(Main.Curlew.embryo_modelling.dataset_Failed.only,
         Fertility=='Fertile')

unique(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile$Fertility)

Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit <- 
  subset(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile,
         Cooney.et.al.Phase.category!='NA')

sum(is.na(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit$Cooney.et.al.Phase.category)) #check NAs have been removed

Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded <- 
  subset(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit,
         Analyses.Failed.egg.category!='Hatched')
unique(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded$Analyses.Failed.egg.category)

###summary hatching failure for headstart vs wild###
#only include eggs that were confirmed fertile (failed only)

#UK and Ireland grouped
#across all breeding seasons
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit %>%
  group_by(Incubation.type) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                 +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                 +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                 +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                         +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)

#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland.WITHOUT.HATCHED <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded %>%
  group_by(Incubation.type) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100)           

#Separate breeding seasons
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit %>%
  group_by(Incubation.type, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)

#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate.WITHOUT.HATCHED <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded %>%
  group_by(Incubation.type, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 

#Group by Incubation type and country
#Combined breeding years
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.Country <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit %>%
  group_by(Incubation.type, Region.Country) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)


#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.Country.WITHOUT.HATCHED <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded %>%
  group_by(Incubation.type, Region.Country) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 
#separate breeding years
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.Country.separate.breeding.seasons <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit %>%
  group_by(Incubation.type, Region.Country, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)


#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.Country.separate.breeding.seasons.WITHOUT.HATCHED <- 
  Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded %>%
  group_by(Incubation.type, Region.Country, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 

#Group by Incubation type and county
#omit NA county data
#WITH HATCHED
Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted<- 
  subset(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit,
         county!='NA')

#WITHOUT HATCHED
Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted.Hatched.excluded<- 
  subset(Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded,
         county!='NA')

#Combined breeding years
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.County <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted %>%
  group_by(Incubation.type, county) %>%
  summarise(Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)

#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.WITHOUT.HATCHED <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted.Hatched.excluded %>%
  group_by(Incubation.type, county) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 

#separated breeding seasons
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted %>%
  group_by(Incubation.type, county, Breeding.year) %>%
  summarise(Latitude = unique(Region.central.latitude.coordinate),
            Longitude = unique(Region.central.longitude.coordinate),
            total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)

#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons.WITHOUT.HATCHED <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.county.NA.omitted.Hatched.excluded %>%
  group_by(Incubation.type, county, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 

#Group by Incubation type and Breeding habitat type
#omit NA county data
#WITH HATCHED
Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted<- 
  subset( Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit,
         Analyses.breeding.habitat.type!='NA')

#WITHOUT HATCHED
Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted.Hatched.excluded<- 
  subset( Main.Curlew.embryo_modelling.dataset_Failed.only_fertile.NA.omit.Hatched.excluded,
          Analyses.breeding.habitat.type!='NA')


#entire UK and Ireland
#combined breeding seasons
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Habitat.type.entire.UK.Ireland <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)
#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Habitat.type.entire.UK.Ireland.WITHOUT.HATCHED <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted.Hatched.excluded %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 
#separate breeding seasons
#WITH HATCHED
Curlew.embryo.mortality.FAILEDonly.Habitat.type.entire.UK.Ireland.separated.breeding.seasons <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            total_Hatched=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100,
            Percentage_Phase.4=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.4'])))*100)
#WITHOUT HATCHED
Curlew.embryo.mortality.FAILEDonly.Habitat.type.entire.UK.Ireland.separated.breeding.seasons.WITHOUT.HATCHED <- 
  Curlew.embryo.mortality.RAW.fertile.failed.eggs.only.Habitat.type.NA.omitted.Hatched.excluded %>%
  group_by(Incubation.type, Analyses.breeding.habitat.type, Breeding.year) %>%
  summarise(total_clutch = length(unique(Project.clutch.ID)),
            total_eggs = length(unique(Egg.ID)),
            total_Phase.1=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1']),
            total_Phase.2=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2']),
            total_Phase.3=length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3']),
            Percentage_Phase.1=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.2=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100,
            Percentage_Phase.3=(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])/(length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.1'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.2'])
                                                                                                               +length(Cooney.et.al.Phase.category[Cooney.et.al.Phase.category == 'Phase.3'])))*100) 

####################################################
# 3 - Graphical presentation of data


#Hatching failure rates county map figures
#load necessary libraries
library('dplyr')
library('tidyverse')
library('sf') 
library('ggspatial')
library('ggplot2')
library('ggrepel')

#NOTE: set working directory for this section with following:
#setwd("C:/Users/jamie/Documents/UoS_work/PDRA_Hemmings.research.group.work/
#CURLEW.PAPER.1_Patterns.of.hatching.failure/shapefiles_for.county.map.plotting")

UK_counties.map <- st_read("GB/georef-united-kingdom-county-unitary-authority-millesime.shp")
All.Ireland_counties.map <- st_read("All.of.Ireland/counties.shp")


#Need to tidy up UK shapefile to make GB map
#remove Northern Ireland data points
GB_counties.map <- UK_counties.map[-c(36,79,100,132,160,166,167,168,189,202,213),]

# Check CRS of both shapefiles
print(st_crs(GB_counties.map))
print(st_crs(All.Ireland_counties.map))

# If CRS are different, reproject to a common CRS (e.g., WGS84)
if (st_crs(GB_counties.map) != st_crs(All.Ireland_counties.map)) {
  GB_counties.map <- st_transform(GB_counties.map, st_crs(All.Ireland_counties.map))
}

names(GB_counties.map)
names(All.Ireland_counties.map)

#Need to make sure both shapefiles have the same columns and names
#Columns we will need: "county" (IRE = NAME_EN; GB = ctyua_name), "geometry"
#change column names to county
GB_counties.map <- GB_counties.map %>%
  rename(county = ctyua_name)

All.Ireland_counties.map <- All.Ireland_counties.map %>%
  rename(county = NAME_EN)

#check names have changes
names(GB_counties.map)
names(All.Ireland_counties.map)

#remove unneccessary columns so that only county and geometry are in the dataset
GB_counties.map<- GB_counties.map %>% select(county,geometry)
All.Ireland_counties.map<- All.Ireland_counties.map %>% select(county,geometry)

#combine maps
combined_GBandIreland <- rbind(GB_counties.map, All.Ireland_counties.map)
list(combined_GBandIreland$county)


#Hatching failure data#

#merge shapefile with data
#county combined breeding season
Hatching.failure.combined.breeding.years.GBandIreland <- merge(combined_GBandIreland,Curlew.hatching.failure.Incubation.type.County,by="county")
Hatching.failure.combined.breeding.years.GBandIreland_county <- bind_rows(Hatching.failure.combined.breeding.years.GBandIreland, combined_GBandIreland) #this gets all shapefiles with no data into map too

#WILD clutches hatching failure rates
Hatching.failure.combined.breeding.years.GBandIreland_county.WILD<- 
  subset(Hatching.failure.combined.breeding.years.GBandIreland_county,
         Incubation.type!='Headstart')

Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.x <- bind_rows(Hatching.failure.combined.breeding.years.GBandIreland_county.WILD, combined_GBandIreland) #this gets all shapefiles with no data into map too
#Need to drop duplicated rows from combining before plotting final figure
Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.FINAL<- Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.x[!duplicated(Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.x$county), ] 

#HEADSTART clutches hatching failure rates
Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART<- 
  subset(Hatching.failure.combined.breeding.years.GBandIreland_county,
         Incubation.type!='Wild')

Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART.x <- bind_rows(Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART, combined_GBandIreland) #this gets all shapefiles with no data into map too
#Need to drop duplicated rows from combining before plotting final figure
Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART.FINAL<- Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART.x[!duplicated(Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART.x$county), ] 

###plot final figures###

County.map_WILD.Traditional = ggplot(Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.FINAL) +
  geom_sf(aes(fill = traditional.hatching.failure))+
            scale_fill_gradient(low = "cornsilk", high = "coral4", na.value = "white", limits = c(0, 100))+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "inside", 
        legend.position.inside = c(0.1, 0.85),legend.background=element_blank())+
  labs(fill='')+
  xlab("Longitude") +
  ylab("Latitude") 
  
County.map_WILD.Recalculated = ggplot(Hatching.failure.combined.breeding.years.GBandIreland_county.WILD.FINAL) +
  geom_sf(aes(fill = recalculated_hatching.failure))+
  scale_fill_gradient(low = "cornsilk", high = "coral4", na.value = "white", limits = c(0, 100))+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "inside", 
        legend.position.inside = c(0.1, 0.85),legend.background=element_blank())+
  labs(fill='')+
  xlab("Longitude") +
  ylab("Latitude") 

County.map_HEADSTART.Traditional = ggplot(Hatching.failure.combined.breeding.years.GBandIreland_county.HEADSTART.FINAL) +
  geom_sf(aes(fill = traditional.hatching.failure))+
  scale_fill_gradient(low = "cornsilk", high = "coral4", na.value = "white", limits = c(0, 100))+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = "inside", 
        legend.position.inside = c(0.1, 0.85),legend.background=element_blank())+
  labs(fill='')+
  xlab("Longitude") +
  ylab("Latitude") 

#Merge figures side by side 
library(cowplot)
plot_grid(County.map_WILD.Traditional, County.map_WILD.Recalculated, County.map_HEADSTART.Traditional, nrow = 1,labels = c("A", "B", "C"))


#Plot latitude vs percentage failure (years combined)
library(smplot2)

#separated WILD and HEADSTART clutches data 
Curlew.hatching.failure.Incubation.type.County.WILD<- 
  subset(Curlew.hatching.failure.Incubation.type.County,
         Incubation.type!='Headstart')

Curlew.hatching.failure.Incubation.type.County.HEADSTART<- 
  subset(Curlew.hatching.failure.Incubation.type.County,
         Incubation.type!='Wild')

Wild.lat.vs.trad.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD, mapping = 
         aes(x = Latitude, y = traditional.hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
  stat_cor(method = 'spearman')  +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(51,58)+
  ylim(0,105)

Wild.lat.vs.recal.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD, mapping = 
       aes(x = Latitude, y = recalculated_hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Recalculated hatching failure rate (%)") +
  xlim(51,58)+
  ylim(0,105)

Headstart.lat.vs.trad.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.HEADSTART, mapping = 
                                                  aes(x = Latitude, y = traditional.hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(51,58)+
  ylim(0,105)

#Merge figures side by side 

library(cowplot)
plot_grid(Wild.lat.vs.trad.hatching.failure_all, Wild.lat.vs.recal.hatching.failure_all, Headstart.lat.vs.trad.hatching.failure_all, nrow = 1,labels = c("D", "E", "F"))

#plot longitude vs hatching failure
Wild.long.vs.trad.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD, mapping = 
                                                  aes(x = Longitude, y = traditional.hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Traditional hatching failure rate") +
  xlim(-10,1.2)+
  ylim(0,105)

Wild.long.vs.recal.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD, mapping = 
                                                   aes(x = Longitude, y = recalculated_hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Recalculated hatching failure rate") +
  xlim(-10,1.2)+
  ylim(0,105)

Headstart.long.vs.trad.hatching.failure_all <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.HEADSTART, mapping = 
                                                       aes(x = Longitude, y = traditional.hatching.failure)) +
  geom_point(shape = 21, fill = 'coral4', color = 'black', aes(size = total_clutch)) + 
  sm_hvgrid() + 
 stat_cor(method = 'spearman') +
  theme(legend.position = c(0.8, 0.8),legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  labs(size='Clutch sample size')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Traditional hatching failure rate") +
  xlim(-10,1.2)+
  ylim(0,105)

library(cowplot)
plot_grid(Wild.long.vs.trad.hatching.failure_all, Wild.long.vs.recal.hatching.failure_all, Headstart.long.vs.trad.hatching.failure_all, nrow = 1,labels = c("G", "H", "I"))

#Plot latitude/longitude vs percentage failure (breeding seasons separated)
library(smplot2)

#separated WILD and HEADSTART clutches data 
Curlew.hatching.failure.Incubation.type.County.WILD_separated.years<- 
  subset(Curlew.hatching.failure.Incubation.type.County.breeding.seasons.separated,
         Incubation.type!='Headstart')
Curlew.hatching.failure.Incubation.type.County.WILD_separated.years$Breeding.year <- 
  as.factor(Curlew.hatching.failure.Incubation.type.County.WILD_separated.years$Breeding.year)

Curlew.hatching.failure.Incubation.type.County.HEADSTART_separated.years<- 
  subset(Curlew.hatching.failure.Incubation.type.County.breeding.seasons.separated,
         Incubation.type!='Wild')
Curlew.hatching.failure.Incubation.type.County.HEADSTART_separated.years$Breeding.year <- 
  as.factor(Curlew.hatching.failure.Incubation.type.County.HEADSTART_separated.years$Breeding.year)

Wild.lat.vs.trad.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD_separated.years, mapping = 
                                                  aes(x = Latitude, y = traditional.hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") +
  theme(legend.position = c(0.83, 0.75), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', color ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(51,58)+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, by = 20))

Wild.lat.vs.recalc.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD_separated.years, mapping = 
                                                                      aes(x = Latitude, y = recalculated_hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") +
  theme(legend.position = c(0.83, 0.75), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', color ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Recalculated hatching failure rate (%)") +
  xlim(51,58)+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, by = 20))

Headstart.lat.vs.trad.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.HEADSTART_separated.years, mapping = 
                                                                      aes(x = Latitude, y = traditional.hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") + 
  theme(legend.position = c(0.83, 0.75), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', fill ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Latitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(51,58)+
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, by = 20))

library(cowplot)
plot_grid(Wild.lat.vs.trad.hatching.failure_separate.breeding.years, Wild.lat.vs.recalc.hatching.failure_separate.breeding.years, Headstart.lat.vs.trad.hatching.failure_separate.breeding.years, nrow = 1,labels = c("A", "B", "C"))

Wild.long.vs.trad.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD_separated.years, mapping = 
                                                                      aes(x = Longitude, y = traditional.hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") + 
  theme(legend.position = c(0.81, 0.78), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', color ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(-10,1.2)+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, by = 20))

Wild.long.vs.recalc.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.WILD_separated.years, mapping = 
                                                                        aes(x = Longitude, y = recalculated_hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") + 
  theme(legend.position = c(0.81, 0.78), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', color ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Recalculated hatching failure rate (%)") +
  xlim(-10,1.2)+
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, by = 20))

Headstart.long.vs.trad.hatching.failure_separate.breeding.years <- ggplot(data = Curlew.hatching.failure.Incubation.type.County.HEADSTART_separated.years, mapping = 
                                                                           aes(x = Longitude, y = traditional.hatching.failure, fill = Breeding.year, color = Breeding.year)) +
  geom_point(shape = 21, color = "black", aes(size = total_clutch)) +
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  sm_hvgrid() + 
  stat_cor(method = 'spearman') +
  scale_color_manual(values=c("pink2", "coral2","coral4")) +
  guides(color = "none") + 
  theme(legend.position = c(0.81, 0.78), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
  labs(size='Clutch sample size', color ='Breeding year')+
  scale_size_continuous(breaks = c(1, 5, 10, 20, 30, 40, 50))+
  xlab("Longitude") +
  ylab("Traditional hatching failure rate (%)") +
  xlim(-10,1.2)+
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, by = 20))

library(cowplot)
plot_grid(Wild.long.vs.trad.hatching.failure_separate.breeding.years, Wild.long.vs.recalc.hatching.failure_separate.breeding.years, Headstart.long.vs.trad.hatching.failure_separate.breeding.years, nrow = 1,labels = c("D", "E", "F"))


#Bar plots for landuse type

#split data to wild and headstart
#separated WILD and HEADSTART clutches data 
Curlew.hatching.failure.Incubation.type.landuse.WILD<- 
  subset(Curlew.hatching.failure.Incubation.type.land.use.entire.UK.Ireland,
         Incubation.type!='Headstart')

Curlew.hatching.failure.Incubation.type.landuse.HEADSTART<- 
  subset(Curlew.hatching.failure.Incubation.type.land.use.entire.UK.Ireland,
         Incubation.type!='Wild')

#Wild vs Headstart traditional hatching failure 
ggplot(data = Curlew.hatching.failure.Incubation.type.land.use.entire.UK.Ireland, mapping = 
         aes(x = Analyses.breeding.habitat.type, y = traditional.hatching.failure, fill = Incubation.type, colour = Incubation.type, label = total_clutch, group = Incubation.type, na.omit = FALSE)) + 
  sm_hvgrid() + 
  geom_bar(position = position_dodge(), stat='identity')+ 
  geom_text(position = position_dodge(width = 1),
    vjust = -0.5, size = 3)+ 
  theme(legend.position = c(0.85, 0.85), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  scale_fill_manual(values=c("coral2","coral4")) +
  scale_color_manual(values=c("coral2","coral4"))+
  guides(color = "none")+
  labs(fill = 'Incubation type')+
  xlab("Breeding habitat type") +
  ylab("Traditional hatching failure rate")

#Wild traditional vs recalculated hatching failure 
#restructure data to plot bar plots 
library(reshape2)
Curlew.hatching.failure.Incubation.type.landuse.WILD.reshaped <- melt(Curlew.hatching.failure.Incubation.type.landuse.WILD[,c('Analyses.breeding.habitat.type','traditional.hatching.failure','recalculated_hatching.failure')],id.vars = 1)

ggplot(data = Curlew.hatching.failure.Incubation.type.landuse.WILD.reshaped, mapping = 
         aes(x = Analyses.breeding.habitat.type, y = value, fill = variable, color = variable, na.omit = FALSE)) + 
  sm_hvgrid() + 
  geom_bar(position = position_dodge(), stat='identity')+ 
  theme(legend.position = c(0.78, 0.85), legend.background=element_blank(), legend.box.background = element_rect(colour = "black"))+
  scale_fill_manual(values=c("coral2","coral4"), labels=c("Traditional", "Recalculated")) +
  scale_color_manual(values=c("coral2","coral4"))+
  guides(color = "none")+
  labs(fill = 'Calculation method')+
  xlab("Breeding habitat type") +
  ylab("Hatching failure rate (%)")


#Embryo mortality data

#WITH HATCHED EGGS#
#plotting stacked proportion bars for entire UK and Ireland population
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.1<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.1
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.2<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.2
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.3<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.3
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Hatched<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.4
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate.restructured <-
  melt(Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate, id.vars = c("Incubation.type", "Breeding.year"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"))


#FAILED EGGS - Headstart vs Wild across breeding seasons
Embryo.mortality.stacked.entire.UKandIreland <- Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate.restructured%>%
  ggplot(aes( x = Breeding.year, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black", linewidth = .3)+
  facet_wrap(~Incubation.type, strip.position = "bottom")+
  theme(text = element_text(size = 9))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.placement = "outside")+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo mortality stage')+
  xlab("Breeding year") +
  ylab("Embryo mortality outcome (%)") 


#plotting stacked proportion bars across latitude/longitude
#FAILED WILD EGGS - combined years
#Remove headstart from dataset
Curlew.embryo.mortality.Incubation.type.county.WILD.failed<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County,
         Incubation.type!='Headstart')

#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.Wild.only.Lat <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  
 
#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.Wild.only.Long <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.long.restructured%>%
                                                           mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
                                                         ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#FAILED WILD EGGS - separated years
#Remove headstart from dataset
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2022')

Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2023')

Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2024')

#2022 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2022.Wild.only.Lat <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2022.Wild.only.Long <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2022.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#2023 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2023.Wild.only.Lat <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2023.Wild.only.Long <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2023.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)") 

#2024 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2024.Wild.only.Lat <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.1
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.2
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.3
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Hatched<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024$Percentage_Phase.4
#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3", "Hatched"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2024.Wild.only.Long <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.2024.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4", "grey")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)") 

#WITHOUT HATCHED EGGS#
#plotting stacked proportion bars for entire UK and Ireland population
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.1<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.2<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Phase.3<-Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate$Percentage_Phase.3.without.hatched
Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate.restructured <-
  melt(Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate, id.vars = c("Incubation.type", "Breeding.year"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"))


#FAILED EGGS - Headstart vs Wild across breeding seasons
Embryo.mortality.stacked.entire.UKandIreland.without.hatched <- Curlew.embryo.mortality.FAILEDonly.Incubation.type.entire.UK.Ireland_breeding.seasons.separate.restructured%>%
  ggplot(aes( x = Breeding.year, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black", linewidth = .3)+
  facet_wrap(~Incubation.type, strip.position = "bottom")+
  theme(text = element_text(size = 9))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.placement = "outside")+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo mortality stage')+
  xlab("Breeding year") +
  ylab("Embryo mortality outcome (%)") 


#plotting stacked proportion bars across latitude/longitude
#FAILED WILD EGGS - combined years
#Remove headstart from dataset
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County,
         Incubation.type!='Headstart')

#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.Wild.only.Lat.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.Wild.only.Long.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#FAILED WILD EGGS - separated years
#Remove headstart from dataset
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2022')

Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2023')

Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024<- 
  subset(Curlew.embryo.mortality.FAILEDonly.Incubation.type.County.separate.breeding.seasons,
         Incubation.type!='Headstart' & Breeding.year == '2024')

#2022 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2022.Wild.only.Lat.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2022.Wild.only.Long.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2022.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#2023 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2023.Wild.only.Lat.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2023.Wild.only.Long.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2023.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  


#2024 graphs
#ordered by increasing Latitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Lat.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Latitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Longitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024.lat.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024, id.vars = c("Latitude", "Lat.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm=TRUE)

Embryo.mortality.stacked.Failed.2024.Wild.only.Lat.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024.lat.restructured%>%
  mutate(Long.coordinate = fct_reorder(Lat.coordinate, Latitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Lat.coordinate, y = value, fill = variable)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Latitude, Longitude) coordinates") +
  ylab("Embryo mortality outcome (%)")  

#ordered by increasing Longitude
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Long.coordinate <- paste("(", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Longitude), ", ", as.character(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Latitude), ")")
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.1<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.1.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.2<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.2.without.hatched
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Phase.3<-Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024$Percentage_Phase.3.without.hatched

#need to reshape data for stacked bars plot
Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024.long.restructured <-
  melt(Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024, id.vars = c("Longitude", "Long.coordinate"),
       measure.vars = c("Phase.1", "Phase.2", "Phase.3"), na.rm = TRUE)

Embryo.mortality.stacked.Failed.2024.Wild.only.Long.without.hatched <- Curlew.embryo.mortality.Incubation.type.county.WILD.failed.without.hatched.2024.long.restructured%>%
  mutate(Long.coordinate = fct_reorder(Long.coordinate, Longitude, .desc = FALSE)) %>% 
  ggplot(aes( x = Long.coordinate, y = value, fill = variable, na.rm = TRUE)) +
  geom_bar(, stat="identity", colour = "black")+
  theme(text = element_text(size = 9), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(values=c("pink2", "coral2","coral4")) +
  labs(fill='Embryo Mortality Stage')+
  xlab("(Longitude, Latitude) coordinates") +
  ylab("Embryo mortality outcome (%)") 
