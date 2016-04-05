#read in data for tables 2 and 3
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
                 f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + region_cNUNN,
               data = tab2_3.dat,Hess = T)
names(coef(tab2.5))#check which variables are linearly dependent
tab2.5 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + f_french + f_pothco + f_belg + 
                 f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
               data = tab2_3.dat,Hess = T)#re-estimate model dropping f_spain, f_dutch, region_cNUNN
tab2.5$vcov <- vcov(tab2.5)#extract vcov mat
tab2.5$estfun <- estfun(tab2.5)#calculate score mat
tab2.5$vcovHC <- sandwich(tab2.5, meat=crossprod(tab2.5$estfun)/length(tab2_3.dat$ConflictOrderedVar))#calculate robust se

tab2.6 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + lrgdpl2631970 + legor_frNUNN + f_french + f_spain + 
                 f_pothco + f_dutch + f_belg + f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
               data=tab2_3.dat,Hess = T)
names(coef(tab2.6))#check which variables are linearly dependent
tab2.6 <- polr(as.factor(ConflictOrderedVar) ~ WarPrevalence14001700 + lrgdpl2631970 + legor_frNUNN + f_french + 
                 f_pothco + f_belg + f_italy + f_germ + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN,
               data=tab2_3.dat,Hess = T)#re-estimate model dropping f_spain,f_dutch
tab2.6$vcov <- vcov(tab2.6)#extract the variance-covariance mat
tab2.6$estfun <- estfun(tab2.6)#calculate score mat
tab2.6$vcovHC <- sandwich(tab2.6, meat=crossprod(tab2.6$estfun)/length(tab2_3.dat$ConflictOrderedVar))#calculate robust se

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
                 region_sNUNN + region_wNUNN + region_eNUNN + f_french + f_pothco + 
                 f_belg + f_italy + f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + 
                 low_tempNUNN + ln_coastline_areaNUNN + island_dumNUNN + islam+ ln_avg_gold_pop + 
                 ln_avg_oil_pop+ ln_avg_all_diamonds_pop+ yellow  + rugged,data=tab2_3.dat)#dropped legor_frNUNN,ETHPOL for convergence issues
tab2.9$vcov <- vcov(tab2.9)#extract the variance-covariance mat
tab2.9$estfun <- estfun(tab2.9)#calculate score mat
tab2.9$vcovHC <- sandwich(tab2.9, meat=crossprod(tab2.9$estfun)/length(tab2_3.dat$ConflictOrderedVar))#calculate robust se

#put all of the robust standard errors into a list
table2.rse <- list(sqrt(diag(tab2.1$vcovHC)),sqrt(diag(tab2.2$vcovHC)),sqrt(diag(tab2.3$vcovHC)),sqrt(diag(tab2.4$vcovHC)),
                   sqrt(diag(tab2.5$vcovHC)),sqrt(diag(tab2.6$vcovHC)),sqrt(diag(tab2.7$vcovHC)),
                   sqrt(diag(tab2.8$vcovHC)),sqrt(diag(tab2.9$vcovHC)))

#put all dependent variables into a vector
table2.dep.var.labels <- c('Civil war','Civil war','Purges',
                           'Purges','Conflict (ordered)','Conflict (ordered)',
                           'Civil war','Purges','Conflict (ordered)')

#put all covariate labels into a vector
table2.cov.labels <- c('War Prevalence 1400-1700','Slave exports','Population Density in 1400')

#specify variables to keep in the table
table2.keeps <- c('WarPrevalence14001700','ln_export_area','ln_pop_dens_1400')

#output models into a table
stargazer(tab2.1,tab2.2,tab2.3,tab2.4,tab2.5,tab2.6,tab2.7,tab2.8,tab2.9,
          out='02_Analysis/Replication/table_2_replication.tex',
          dep.var.labels = table2.dep.var.labels,
          digits = 3,
          float = T,
          keep = table2.keeps,
          covariate.labels = table2.cov.labels,
          se = table2.rse,
          column.sep.width = '2pt',
          title = 'Political Violence (Replication)',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)

