####DIAGNOSTICS####
##Date: 04/05/2012
library(foreign)

####SET UP####
#set working directory
setwd('~/Google Drive/GOV_2001_REPLICATION/')

#load data for tables 2 and 3
tab2_3.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta')

#load data for sub-national afrobarometer analysis
tab4.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#load in grid-level data
tab567.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

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


############Logging / binary indicators - test on DV

tab2_3.dat$civilwar_binary <- NULL
for (i in 1:length(tab2_3.dat$civilwar_binary)){
  if (tab2_3.dat$CivilWarIncidence[i]!= 0){
  tab2_3.dat$civilwar_binary[i] <- 1
  } else {
    tab2_3.dat$civilwar_binary[i] <- 0
  }
} 
tab2_3.dat$purges_binary <- NULL
for (i in 1:length(tab2_3.dat$purges_binary)){
  if (tab2_3.dat$Purges[i]==0.000){
    tab2_3.dat$purges_binary[i] <- 0
  } else {
    tab2_3.dat$purges_binary[i] <- 1
  }
} 
## test out regressions for table 2 using new Dep Var 
tab2.1.binary <- lm(civilwar_binary ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab2.1.binary$vcovHC <- vcovHC(tab2.1.binary,type='HC1')

tab2.2.binary <- lm(civilwar_binary ~ WarPrevalence14001700 + lrgdpl2631970 + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + 
               region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               abs_latitudeNUNN + longitudeNUNN + rain_minNUNN  + humid_maxNUNN +  low_tempNUNN + ln_coastline_areaNUNN + 
               island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + ETHPOL + yellow + rugged,
             data=tab2_3.dat)
tab2.2.binary$vcovHC <- vcovHC(tab2.2.binary,type='HC1')

tab2.3.binary <- lm(purges_binary ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab2.3.binary$vcovHC <- vcovHC(tab2.3.binary,type='HC1')

tab2.4.binary <- lm(purges_binary ~ WarPrevalence14001700 + lrgdpl2631970 + legor_frNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + 
               f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,data=tab2_3.dat)
tab2.4.binary$vcovHC <- vcovHC(tab2.4.binary,type='HC1')

tab2.7.binary <- lm(civilwar_binary ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + region_sNUNN + 
               region_wNUNN + region_eNUNN + region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + 
               f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + low_tempNUNN + 
               ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + 
               ETHPOL + yellow + rugged,data = tab2_3.dat)
tab2.7.binary$vcovHC <- vcovHC(tab2.7.binary,type='HC1')

tab2.8.binary <- lm(purges_binary ~ WarPrevalence14001700 + ln_export_area + ln_pop_dens_1400 + lrgdpl2631970 + region_nNUNN + 
               region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + 
               f_italy + f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + low_tempNUNN + 
               ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + ln_avg_gold_pop + ln_avg_oil_pop + 
               ln_avg_all_diamonds_pop + ETHPOL + yellow + rugged,data=tab2_3.dat)
tab2.8.binary$vcovHC <- vcovHC(tab2.8.binary,type='HC1')

#put all of the robust standard errors into a list
table2.binary.rse <- list(sqrt(diag(tab2.1.binary$vcovHC)),sqrt(diag(tab2.2.binary$vcovHC)),sqrt(diag(tab2.3.binary$vcovHC)),sqrt(diag(tab2.4.binary$vcovHC)),
                sqrt(diag(tab2.7.binary$vcovHC)), sqrt(diag(tab2.8.binary$vcovHC)))

#put all dependent variables into a vector
table2.bin.dep.var.labels <- c('Civil war','Civil war','Purges',
                           'Purges','Conflict (ordered)','Conflict (ordered)',
                           'Civil war','Purges','Conflict (ordered)')

#put all covariate labels into a vector
table2.bin.cov.labels <- c('War Prevalence 1400-1700','Slave exports','Population Density in 1400')

#specify variables to keep in the table
table2.bin.keeps <- c('WarPrevalence14001700','ln_export_area','ln_pop_dens_1400')

#output models into a table
stargazer(tab2.1.binary,tab2.2.binary,tab2.3.binary,tab2.4.binary,tab2.7.binary,tab2.8.binary,
          out='02_Analysis/Extended_Analysis/table_2_binary_replication.tex',
          dep.var.labels = table2.bin.dep.var.labels,
          digits = 3,
          float = T,
          keep = table2.bin.keeps,
          covariate.labels = table2.bin.cov.labels,
          se = table2.binary.rse,
          column.sep.width = '2pt',
          title = 'Political Violence (Replication)',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)

tab2.1 <- lm(CivilWarIncidence ~ WarPrevalence14001700 + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy + f_germ + 
               region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,data = tab2_3.dat)
tab2.1$vcovHC <- vcovHC(tab2.1,type='HC1')

pdf(file = "02_Analysis/Extended_Analysis/tab1_1_influence.pdf",width=6,height=5)
plot(tab2.1, labels.id = tab2_3.dat$country, 5) ## South Africa, Angola, Algeria super influential
dev.off()


tab4.1<- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
              lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
              ln_coastline_areaNUNN+islam+
              ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
              f_pothco+
              rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
            data = tab4.dat)

pdf(file = "02_Analysis/Extended_Analysis/tab4_1_scale_location.pdf",width=6,height=5)
plot(tab4.1, 3) ## ## Is this art??
dev.off()

tab7.1 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
             data=tab567.dat)

pdf(file = "02_Analysis/Extended_Analysis/tab7_1_qq.pdf",width=6,height=5)
plot(tab7.1,2)
dev.off()

