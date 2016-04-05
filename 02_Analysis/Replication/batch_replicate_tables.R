####  GOV 2001: REPLICATION PAPER   ####
##PAPER: BESLEY AND REYNAL-QUEROL (2014, APSR)
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, SHOM MAZUMDER
##FIRST CREATED: 03/10/2016
##LAST UPDATED: 03/16/2016
##PURPOSE: THIS SCRIPT REPLICATES ALL OF THE TABLES/ANALYSES IN BESLEY AND REYNAL-QUEROL (2014)
##NOTES:

####Load Packages####
library(foreign)
library(ggplot2)
library(plyr)
library(stargazer)
library(sandwich)
library(multiwayvcov) 
library(MASS)
library(rms)
library(lmtest)
library(ordinal)
library(aod)

####Set working directory####
setwd("~/Google Drive/GOV_2001_REPLICATION")

####READ IN DATA####
tab2_3.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta')

####FUNCTIONS####

#function for clustered SEs
cluster.se<-function(model, cluster){
  vcovCL<-cluster.vcov(model, cluster)
  coef<-coeftest(model, vcovCL)
  return(coef)
}

#replicate table 2
source('02_Analysis/Replication/table_2.R')

#replicate table 3
source('02_Analysis/Replication/table_3.R')

#replicate table 4
source('02_Analysis/Replication/table_4.R')

#replicate table 5
source('02_Analysis/Replication/table_5.R')

#replicate table 6
source('02_Analysis/Replication/table_6.R')

#replicate table 7
source('02_Analysis/Replication/table_7.R')



