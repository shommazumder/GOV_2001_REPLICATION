####  GOV 2001: REPLICATION PAPER   ####
##PAPER: BESLEY AND REYNAL-QUEROL (2014, APSR)
##AUTHORS: Bowles et al.
##FIRST CREATED: 03/10/2016
##LAST UPDATED: 03/21/2016
##PURPOSE: THIS SCRIPT REPLICATES ALL OF THE TABLES/ANALYSES IN BESLEY AND REYNAL-QUEROL (2014)
##NOTES:

#Table 2
#Need to figure out how to get robust standard errors for ologit (models 2.5, 2.6, 2.9)

#Table 3
#coef for popdensity in model 3.3 is not correct and SE for war prevalence in model 3.5 is not correct
#neither is R^2 for models 3.4 or 3.5, although coefs are correct for both 

#Table 4
#barely started. coefs are right when using controls from tab2&3, but coefs are wrong when using controls they recommend (?)

#Table 5 
#everything is done except need to add Conley SEs for models 2 and 4 

#Table 6
#Coefs/ses look fine, need to do dummies p value tests

#Table 7
#same as 6

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
library(lmtest)

##Set working directory
setwd("~/Google Drive (Harvard)/GOV_2001_REPLICATION")

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

#read in data for table 4
tab4.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#function for clustered SEs
cluster.se<-function(model, cluster){
  vcovCL<-cluster.vcov(model, cluster)
  coef<-coeftest(model, vcovCL)
  return(coef)
}

#run regressions for table 4
#using controls they recommend
tab4.1<- lm(inter_group_trust~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
              region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN + lrgdpl2631960+ age + age2+ male + 
              education + occupation + religion + living_conditions,
            data = tab4.dat)
#using just controls from tab 3
tab4.1<- lm(inter_group_trust~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
              region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN + townvill,
            data = tab4.dat)

tab4.1.cluster <- cluster.se(tab4.1, district)
#not right... 

#########################################

####REPLICATE TABLE 5####

#read in data for tables 5, 6, 7
tab567.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

tab5.1<- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
            data=tab567.dat)
tab5.1.cluster <- cluster.se(tab5.1, tab567.dat$country)

tab5.2 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
               elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
              data=tab567.dat)
tab5.2.cluster <- cluster.se(tab5.2, tab567.dat$country)
##need to do Conley SEs 

tab5.3<- lm(llightnight2007~ HistoricalConflictGrid + lpopdens90 + country,
            data=tab567.dat)
tab5.3.cluster <- cluster.se(tab5.3, tab567.dat$country)

tab5.4 <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
               elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
             data=tab567.dat)
tab5.4.cluster <- cluster.se(tab5.4, tab567.dat$country)
##need to do Conley SEs 


#########################################

####  REPLICATE TABLE 6  ####

library(survey)
library(aod)

tab6.1 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + 
              precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + 
              lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + 
              ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + 
              carthage + factor(country), 
            data=tab567.dat)
tab6.1.cluster <- cluster.se(tab6.1, tab567.dat$country)

tab6.1.vcov.CL <- cluster.vcov(tab6.1,tab567.dat$country)

coef.names <- rownames(as.matrix(coef(tab6.1)))
dummies.precolonial <- 11:33

tab6.1.wald <- wald.test(b = coef(tab6.1), Sigma=tab6.1.vcov.CL, Terms = dummies.precolonial, df=51)

tab6.2 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 +  + ethnicdivdum + factor(country), 
   data=tab567.dat)
tab6.2.cluster <- cluster.se(tab6.2, tab567.dat$country)

tab6.3 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + logslave_obs + factor(country),
   data=tab567.dat)
tab6.3.cluster <- cluster.se(tab6.3, tab567.dat$country)

tab6.4 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + grid_cap + factor(country),
   data=tab567.dat)
tab6.4.cluster <- cluster.se(tab6.4, tab567.dat$country)

tab6.5 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + JurisHierac + factor(country), 
   data=tab567.dat)
tab6.5.cluster <- cluster.se(tab6.5, tab567.dat$country)

tab6.6 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + llightnight1992 + sharemin + factor(economynum) + factor(country), 
   data=tab567.dat)
tab6.6.cluster <- cluster.se(tab6.6, tab567.dat$country)

tab6.6.vcov.CL <- cluster.vcov(tab6.6,tab567.dat$country)
coef.names <- rownames(as.matrix(coef(tab6.6)))
dummies.economic <- 12:38

tab6.6.wald <- wald.test(b = coef(tab6.6), Sigma=tab6.6.vcov.CL, Terms = dummies.economic, df=51)

tab6.7 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + ethnicdivdum + logslave_obs + grid_cap + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + JurisHierac + llightnight1992 + sharemin + factor(economynum) + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
   data=tab567.dat)
tab6.7.cluster <- cluster.se(tab6.7, tab567.dat$country)

#########################################

####  REPLICATE TABLE 7  ####

tab7.1 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
             data=tab567.dat)
tab7.1.cluster <- cluster.se(tab7.1, tab567.dat$country)

tab7.1.vcov.CL <- cluster.vcov(tab7.1,tab567.dat$country)
tab7.1.wald <- wald.test(b = coef(tab7.1), Sigma=tab7.1.vcov.CL, Terms = dummies.precolonial, df=51)

tab7.2 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + ethnicdivdum + factor(country),
             data=tab567.dat)
tab7.2.cluster <- cluster.se(tab7.2, tab567.dat$country)

tab7.3 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + logslave_obs + factor(country),
             data=tab567.dat)
tab7.3.cluster <- cluster.se(tab7.3, tab567.dat$country)

tab7.4 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + grid_cap + factor(country),
             data=tab567.dat)
tab7.4.cluster <- cluster.se(tab7.4, tab567.dat$country)

tab7.5 <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + JurisHierac + factor(country),
             data=tab567.dat)
tab7.5.cluster <- cluster.se(tab7.5, tab567.dat$country)

tab7.6 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + llightnight1992 + sharemin + factor(economynum) + factor(country),
             data=tab567.dat)
tab7.6.cluster <- cluster.se(tab7.6, tab567.dat$country)

coef.names <- rownames(as.matrix(coef(tab7.6)))
dummies.economic <- 12:38
tab7.6.vcov.CL <- cluster.vcov(tab7.6,tab567.dat$country)

tab7.6.wald <- wald.test(b = coef(tab7.6), Sigma=tab7.6.vcov.CL, Terms = dummies.economic, df=51)

tab7.7 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + ethnicdivdum + logslave_obs + grid_cap + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + JurisHierac + llightnight1992 + sharemin + factor(economynum) + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
             data=tab567.dat)
tab7.7.cluster <- cluster.se(tab7.7, tab567.dat$country)

