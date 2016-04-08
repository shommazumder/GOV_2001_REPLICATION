####DIAGNOSTICS####
##Date: 04/03/2012

##load packages
library(foreign)

####SET UP####
#set working directory
setwd('~/Google Drive/GOV_2001_REPLICATION/01_Data/REPLICATION_APSR_2014_BRQ/')

#load data for tables 2 and 3
tab2_3.dat <- read.dta('REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta')

#load data for sub-national afrobarometer analysis
tab4.dat <- read.dta('REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#load in grid-level data
tab567.dat <- read.dta('REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

#create indicator variables if ever had historic conflict
tab2_3.dat$everWar <- ifelse(tab2_3.dat$WarPrevalence14001700==0,0,1)
tab4.dat$everWar <- ifelse(tab4.dat$WarPrevalence14001700==0,0,1)

####CHECK FOR COMMON SUPPORT (CROSS-NATIONAL)####

#ESTIMATE PROPENSITY SCORE FOR EXTENSIVE MARGIN
pscore.model.crossnat <- glm(everWar ~ f_french + f_pothco + f_belg + f_italy + f_germ + 
                      region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
                    data=tab2_3.dat,
                    family=binomial(link='logit'),
                    na.action = na.exclude)
pscores.crossnat <- predict.glm(pscore.model.crossnat,type='response',na.action = na.exclude)
#pscores.crossnat <- c(pscores[1:16],NA,pscores[17:48])#correct for missing data in Eritrea
tab2_3.dat$pscores <- pscores.crossnat

#PLOT DENSITY OF PROPENSITY SCORES BY TREATMENT
library(ggplot2)
pscore.plot.crossnat <- ggplot(data=tab2_3.dat,aes(x=pscores,group=factor(everWar),colour=factor(everWar)))+
  geom_density(size=1.5)+
  scale_color_discrete(guide = guide_legend(title='Ever Historical War'))+xlab('Propensity Scores')+
  ggtitle('Distribution of Propensity Scores (Before Matching, Cross-National)')
pscore.plot.crossnat

#Run KS test
ks.test(tab2_3.dat$pscores[tab2_3.dat$everWar==1],tab2_3.dat$pscores[tab2_3.dat$everWar==0])

####CHECK FOR COMMON SUPPORT (SUB-NATIONAL)####

#ESTIMATE PROPENSITY SCORE FOR EXTENSIVE MARGIN
pscore.model.subnat <- glm(everWar ~ age+age2+male+urban_dum+ln_export_pop+ln_pop_dens_1400+
                             factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                             data=tab4.dat,
                             family=binomial(link='logit'),
                           na.action = na.exclude)
pscores.subnat <- predict.glm(pscore.model.subnat,type='response',na.action = na.exclude)
tab4.dat$pscores <- pscores.subnat

#PLOT DENSITY OF PROPENSITY SCORES BY TREATMENT
pscore.plot.subnat <- ggplot(data=tab4.dat,aes(x=pscores,group=factor(everWar),colour=factor(everWar)))+
  geom_density(size=1.5)+
  scale_color_discrete(guide = guide_legend(title='Ever Historical War'))+xlab('Propensity Scores')+
  ggtitle('Distribution of Propensity Scores (Before Matching, Sub-National)')
pscore.plot.subnat

#Run KS test
ks.test(tab4.dat$pscores[tab4.dat$everWar==1],tab4.dat$pscores[tab4.dat$everWar==0])


####CHECK FOR COMMON SUPPORT (GRID-LEVEL)####

#ESTIMATE PROPENSITY SCORE FOR EXTENSIVE MARGIN
pscore.model.grid <- glm(HistoricalConflictGrid ~ d1 + elev_srtm_pred + rough + tempav_8008 + 
                           precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + 
                           lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + 
                           ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + 
                           carthage + factor(country),
                           data=tab567.dat,
                           family=binomial(link='logit'),
                           na.action = na.exclude)
pscores.grid <- predict.glm(pscore.model.grid,type='response',na.action = na.exclude)
tab567.dat$pscores <- pscores.grid

#PLOT DENSITY OF PROPENSITY SCORES BY TREATMENT
pscore.plot.grid <- ggplot(data=tab567.dat,aes(x=pscores,group=factor(HistoricalConflictGrid),colour=factor(HistoricalConflictGrid)))+
  geom_density(size=1.5)+
  scale_color_discrete(guide = guide_legend(title='Historical Conflict in Grid'))+xlab('Propensity Scores')+
  ggtitle('Distribution of Propensity Scores (Before Matching, Grid-Level)')
pscore.plot.grid

#Run KS test
ks.test(tab567.dat$pscores[tab567.dat$HistoricalConflictGrid==1],
        tab567.dat$pscores[tab567.dat$HistoricalConflictGrid==0])
