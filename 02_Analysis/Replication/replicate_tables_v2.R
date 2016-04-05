####  GOV 2001: REPLICATION PAPER   ####
##PAPER: BESLEY AND REYNAL-QUEROL (2014, APSR)
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, SHOM MAZUMDER
##FIRST CREATED: 03/10/2016
##LAST UPDATED: 03/16/2016
##PURPOSE: THIS SCRIPT REPLICATES ALL OF THE TABLES/ANALYSES IN BESLEY AND REYNAL-QUEROL (2014)
##NOTES:

#Table 2
#Need to figure out how to get robust standard errors for ologit (models 2.5, 2.6, 2.9)

#Table 3
#coef for popdensity in model 3.3 is not correct and SE for war prevalence in model 3.5 is not correct
#neither is R^2 for models 3.4 or 3.5, although coefs are correct for both 

#Table 4
#barely started. coefs are right when using controls from tab2&3, but SEs are wrong // coefs are wrong when using controls they recommend
########################################################################################################################################################


##Load Packages
library(foreign)
library(ggplot2)
library(plyr)
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
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
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


#########################################

####REPLICATE TABLE 3####
#run regressions for table 3
tab3.1 <- lm(lrgdpl2632000 ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab3.1$vcovHC <- vcovHC(tab3.1,type='HC1')
tab3.1$errors <- sqrt(diag(tab3.1$vcovHC))

tab3.2<- lm(lrgdpl2632000 ~ WarPrevalence14001700 + lrgdpl2631970 + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN +
              region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ +
              abs_latitudeNUNN + longitudeNUNN + rain_minNUNN  + humid_maxNUNN +  low_tempNUNN + ln_coastline_areaNUNN +
              island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + ethnic_fractionalizationNUNN + yellow + rugged,
             data=tab2_3.dat)
tab3.2$vcovHC <- vcovHC(tab3.2,type='HC1')
tab3.2$errors <- sqrt(diag(tab3.2$vcovHC))
summary(tab3.2)

tab3.3<- lm(lrgdpl2632000 ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + 
  region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
  abs_latitudeNUNN + longitudeNUNN + rain_minNUNN  + humid_maxNUNN +  low_tempNUNN + ln_coastline_areaNUNN + 
  island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + ethnic_fractionalizationNUNN + yellow + rugged,
data=tab2_3.dat)
tab3.3$vcovHC <- vcovHC(tab3.3,type='HC1')
tab3.3$errors <- sqrt(diag(tab3.3$vcovHC))
summary(tab3.3)

tab3.4 <- lm(avexpr ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab3.4$vcovHC <- vcovHC(tab3.4,type='HC1')
tab3.4$errors <- sqrt(diag(tab3.4$vcovHC))
summary(tab3.4)

tab3.5 <- lm(xconst5 ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab3.5$vcovHC <- vcovHC(tab3.5,type='HC1')
tab3.5$errors <- sqrt(diag(tab3.5$vcovHC))
summary(tab3.5)

#########################################

####REPLICATE TABLE 4####

library(lmtest)
#read in data for table 4
tab4.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#run regressions for table 4
tab4.1<- lm(inter_group_trust~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
              region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN , 
            data = tab4.dat)
coeftest(tab4.1) #still need to add in clustered VCov matrix 
tab4.1$errors #these are not right (but coef is not right w/ controls added)
summary(tab4.1)

##other controls 
# lrgdpl2631960+ age + age2+ male + education + occupation + religion + living_conditions + district
