####  GOV 2001: REPLICATION PAPER   ####
##PAPER: BESLEY AND REYNAL-QUEROL (2014, APSR)
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, SHOM MAZUMDER
##FIRST CREATED: 03/10/2016
##LAST UPDATED: 03/10/2016
##PURPOSE: THIS SCRIPT REPLICATES ALL OF THE TABLES/ANALYSES IN BESLEY AND REYNAL-QUEROL (2014)
##NOTES:Need to figure out how to get robust standard errors for ologit 
########################################################################################################################################################


##Load Packages
library(foreign)
library(ggplot2)
library(dplyr)
library(stargazer)
library(sandwich)
library(multiwayvcov) 
library(MASS)
library(rms)

##Set working directory
setwd("~/Google Drive/GOV_2001_REPLICATION")

###################TEST##################
library(ordinal)
tab2.5 <- clm(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + f_french + f_pothco + f_belg + 
                f_italy + f_germ + region_nNUNN + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
              data = tab2_3.dat)

#########################################

####REPLICATE TABLE 2####

#read in data to replicate tables 2 and 3
tab2_3.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta')

#run regressions for table 2
tab2.1 <- lm(CivilWarIncidence ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab2.1$vcovHC <- vcovHC(tab2.1,type='HC1')

tab2.2 <- lm(CivilWarIncidence ~ WarPrevalence14001700 + lrgdpl2631970 + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + 
               region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               abs_latitudeNUNN + longitudeNUNN + rain_minNUNN  + humid_maxNUNN +  low_tempNUNN + ln_coastline_areaNUNN + 
               island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + ETHPOL + yellow + rugged,
             data=tab2_3.dat)
tab2.2$vcovHC <- vcovHC(tab2.2,type='HC1')

tab2.3 <- lm(Purges ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab2.3$vcovHC <- vcovHC(tab2.3,type='HC1')

tab2.4 <- lm(Purges ~ WarPrevalence14001700 + lrgdpl2631970 + legor_frNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + 
               f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,data=tab2_3.dat)
tab2.4$vcovHC <- vcovHC(tab2.4,type='HC1')

tab2.5 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + 
                 f_italy + f_germ + region_nNUNN + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,
               data = tab2_3.dat,Hess = T)
tab2.5$vcov <- solve(-tab2.5$Hessian)#need to extract the vcov mat from the Hessian using Fisher information

tab2.6 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + lrgdpl2631970 + legor_frNUNN + f_french + f_spain + 
                 f_pothco + f_dutch + f_belg + f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
               data=tab2_3.dat,Hess = T)
tab2.6$vcov <- solve(-tab2.6$Hessian)#need to extract the vcov mat from the Hessian using Fisher information

tab2.7 <- lm(CivilWarIncidence ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + region_sNUNN + 
               region_wNUNN + region_eNUNN + region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + 
               f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + low_tempNUNN + 
               ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + 
               ETHPOL + yellow + rugged,data = tab2_3.dat)
tab2.7$vcovHC <- vcovHC(tab2.7,type='HC1')

tab2.8 <- lm(Purges ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + 
               region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + 
               f_italy + f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + low_tempNUNN + 
               ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + 
               ln_avg_all_diamonds_pop + ETHPOL + yellow + rugged,data=tab2_3.dat)
tab2.8$vcovHC <- vcovHC(tab2.8,type='HC1')

tab2.9 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + 
                 region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN + f_french + f_spain + f_pothco + f_dutch + 
                 f_belg + f_italy + f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + 
                 low_tempNUNN + ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + 
                 ln_avg_oil_pop + ln_avg_all_diamonds_pop + ETHPOL + yellow + rugged,data=tab2_3.dat)
tab2.9$vcov <- solve(-tab2.9$Hessian)#need to extract the vcov mat from the Hessian using Fisher information

