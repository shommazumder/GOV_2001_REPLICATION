####REPLICATE TABLE 3####

tab2_3.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta')

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

tab3.3<- lm(lrgdpl2632000 ~ WarPrevalence14001700 + ln_export_pop + ln_pop_dens_1400 + 
              lrgdpl2631970 + region_nNUNN + region_sNUNN + region_wNUNN + region_eNUNN + 
              region_cNUNN + f_french + f_spain + f_pothco + f_dutch + f_belg + f_italy +
              f_germ + abs_latitudeNUNN + longitudeNUNN + rain_minNUNN  + humid_maxNUNN + 
              low_tempNUNN + ln_coastline_areaNUNN + island_dumNUNN + islam + legor_frNUNN + 
              ln_avg_gold_pop + ln_avg_oil_pop + ln_avg_all_diamonds_pop + 
              ethnic_fractionalizationNUNN + yellow + rugged, data=tab2_3.dat)
tab3.3$vcovHC <- vcovHC(tab3.3,type='HC1')
tab3.3$errors <- sqrt(diag(tab3.3$vcovHC))
summary(tab3.3)

tab3.4 <- lm(avexpr ~ WarPrevalence14001700 + lrgdpl2631970+ abs_latitudeNUNN +
             longitudeNUNN + rain_minNUNN + humid_maxNUNN +low_tempNUNN+
             ln_coastline_areaNUNN +island_dumNUNN+ islam +legor_frNUNN+ region_nNUNN +
           region_nNUNN + region_sNUNN + region_wNUNN +region_eNUNN+ region_cNUNN +
             ln_avg_gold_pop+ ln_avg_oil_pop+ ln_avg_all_diamonds_pop+ f_french+
             f_spain+ f_pothco +f_dutch+ f_belg+ f_italy +f_germ+
             ethnic_fractionalizationNUNN +yellow +rugged, data=tab2_3.dat)
tab3.4$vcovHC <- vcovHC(tab3.4,type='HC1')
tab3.4$errors <- sqrt(diag(tab3.4$vcovHC))
summary(tab3.4)

tab3.5 <- lm(xconst5 ~ WarPrevalence14001700 +lrgdpl2631970+ abs_latitudeNUNN+
               longitudeNUNN +rain_minNUNN+ humid_maxNUNN+ low_tempNUNN +
               ln_coastline_areaNUNN+ island_dumNUNN +islam+ legor_frNUNN+ region_nNUNN+
               region_nNUNN + region_sNUNN +region_wNUNN +region_eNUNN+ region_cNUNN +
               ln_avg_gold_pop + ln_avg_oil_pop+ ln_avg_all_diamonds_pop+ f_french +
               f_spain +f_pothco+ f_dutch +f_belg+ f_italy+ f_germ+ ethnic_fractionalizationNUNN +
               yellow+ rugged + f_french + f_spain+ f_pothco+ f_dutch +f_belg+ f_italy+
               f_germ,data = tab2_3.dat)
tab3.5$vcovHC <- vcovHC(tab3.5,type='HC1')
tab3.5$errors <- sqrt(diag(tab3.5$vcovHC))
summary(tab3.5)

#put all of the robust standard errors into a list
table3.rse <- list(tab3.1$errors, tab3.2$errors, tab3.3$errors, tab3.4$errors, tab3.5$errors)

#put all dependent variables into a vector
table3.dep.var.labels <- c('GDP per capita, 2000','GDP per capita, 2000','GDP per capita, 2000',
                           'Expropriation Risk','Checks and Balances')

#put all covariate labels into a vector
table3.cov.labels <- c('War Prevalence 1400-1700','Slave exports','Population Density in 1400')

#specify variables to keep in the table
table3.keeps <- c('WarPrevalence14001700','ln_export_pop','ln_pop_dens_1400')

#output models into a table
stargazer(tab3.1,tab3.2,tab3.3,tab3.4,tab3.5,
          out='02_Analysis/Replication/table_3_replication.tex',
          dep.var.labels = table3.dep.var.labels,
          digits = 3,
          float = T,
          keep = table3.keeps,
          covariate.labels = table3.cov.labels,
          se = table3.rse,
          column.sep.width = '2pt',
          title = 'GDP and Institutions (Replication)',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)





