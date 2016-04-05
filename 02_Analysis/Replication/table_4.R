####REPLICATE TABLE 4####

#read in data for table 4
tab4.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#function for clustered SEs
cluster.se<-function(model, cluster){
  vcovCL<-cluster.vcov(model, cluster)
  coef<-coeftest(model, vcovCL)
  return(coef)
}

tab4.1<- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
              lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
              ln_coastline_areaNUNN+islam+
              ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
              f_pothco+
              rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
            data = tab4.dat)

tab4.1.cluster <- cluster.se(tab4.1, tab4.dat$townvill)
tab4.1$vcovHC <- vcovHC(tab4.1,type='HC1')
tab4.1$errors <- sqrt(diag(tab4.1$vcovHC))

tab4.2 <- lm(identitydumeth ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               f_pothco+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.2.cluster <- cluster.se(tab4.2, tab4.dat$townvill)
tab4.2$vcovHC <- vcovHC(tab4.2,type='HC1')
tab4.2$errors <- sqrt(diag(tab4.2$vcovHC))

tab4.3 <- lm(identitydumnat ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+f_pothco+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.3.cluster <- cluster.se(tab4.3, tab4.dat$townvill)
tab4.3$vcovHC <- vcovHC(tab4.3,type='HC1')
tab4.3$errors <- sqrt(diag(tab4.3$vcovHC))

tab4.4 <- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.4.cluster <- cluster.se(tab4.4, tab4.dat$townvill)
tab4.4$vcovHC <- vcovHC(tab4.4,type='HC1')
tab4.4$errors <- sqrt(diag(tab4.4$vcovHC))

tab4.5 <- lm(identitydumeth ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.5.cluster <- cluster.se(tab4.5, tab4.dat$townvill)
tab4.5$vcovHC <- vcovHC(tab4.5,type='HC1')
tab4.5$errors <- sqrt(diag(tab4.5$vcovHC))

tab4.6 <- lm(identitydumnat ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.6.cluster <- cluster.se(tab4.6, tab4.dat$townvill)
tab4.6$vcovHC <- vcovHC(tab4.6,type='HC1')
tab4.6$errors <- sqrt(diag(tab4.6$vcovHC))




#put all of the robust standard errors into a list
table4.rse <- list(tab4.1$errors, tab4.2$errors, tab4.3$errors, tab4.4$errors, tab4.5$errors, tab4.6$errors)

#put all dependent variables into a vector
table4.dep.var.labels <- c('Inter group','Ethnic Identity','National Identity',
                           'Inter group','Ethnic Identity','National Identity')

#put all covariate labels into a vector
table4.cov.labels <- c('War prevalence 1400-1700','Civil war prevalence')

#specify variables to keep in the table
table4.keeps <- c('WarPrevalence14001700','CivilWarIncidence')

#output models into a table
stargazer(tab4.1,tab4.2,tab4.3,tab4.4,tab4.5,tab4.6,
          out='02_Analysis/Replication/table_4_replication.tex',
          dep.var.labels = table4.dep.var.labels,
          digits = 3,
          float = T,
          keep = table4.keeps,
          covariate.labels = table4.cov.labels,
          se = table4.rse,
          column.sep.width = '2pt',
          title = 'Trust and Identity',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)


