####  REPLICATE TABLE 7  ####

tab567.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

tab7.1 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
             data=tab567.dat)
tab7.1.cluster <- cluster.se(tab7.1, tab567.dat$country)

tab7.1.vcov.CL <- cluster.vcov(tab7.1,tab567.dat$country)
tab7.1.wald <- wald.test(b = coef(tab7.1), Sigma=tab7.1.vcov.CL, Terms = dummies.precolonial, df=51)

tab7.2 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + ethnicdivdum + factor(country),
             data=tab567.dat)
tab7.2.cluster <- cluster.se(tab7.2, tab567.dat$country)
tab7.2.vcov.CL <- cluster.vcov(tab7.2,tab567.dat$country)

tab7.3 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + logslave_obs + factor(country),
             data=tab567.dat)
tab7.3.cluster <- cluster.se(tab7.3, tab567.dat$country)
tab7.3.vcov.CL <- cluster.vcov(tab7.3,tab567.dat$country)

tab7.4 <- lm(llightnight2007 ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + grid_cap + factor(country),
             data=tab567.dat)
tab7.4.cluster <- cluster.se(tab7.4, tab567.dat$country)
tab7.4.vcov.CL <- cluster.vcov(tab7.4,tab567.dat$country)

tab7.5 <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + JurisHierac + factor(country),
             data=tab567.dat)
tab7.5.cluster <- cluster.se(tab7.5, tab567.dat$country)
tab7.5.vcov.CL <- cluster.vcov(tab7.5,tab567.dat$country)

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
tab7.7.vcov.CL <- cluster.vcov(tab7.7,tab567.dat$country)


#put all of the robust standard errors into a list
table7.rse <- list(sqrt(diag(tab7.1.vcov.CL)), sqrt(diag(tab7.2.vcov.CL)), sqrt(diag(tab7.3.vcov.CL)),
                   sqrt(diag(tab7.4.vcov.CL)), sqrt(diag(tab7.5.vcov.CL)), sqrt(diag(tab7.6.vcov.CL)),
                   sqrt(diag(tab7.7.vcov.CL)))
                   

#put all dependent variables into a vector
table7.dep.var.labels <- c('Economic Development')

#put all covariate labels into a vector
table7.cov.labels <- c('Historic Conflict in Grid', 'City in 1400', 'Ethnic diversity',
                       'Slave exports', 'Capital city in Grid', 'Distance to capital 0-10\\%', 
                       'Distance to capital 10-25\\%', 'Distance to capital 25-50\\%', 'Distance to capital 50-75\\%',
                       'Distance to capital 75-90\\%', 'Jurisdictional hierarchy', 'Log light density 1992', 
                       'Mineral share')

#specify variables to keep in the table
table7.keeps <- c('HistoricalConflictGrid', 'gridcity1400', 'ethnicdivdum', 'logslave_obs', 
                  'grid_cap', 'cdist1_10', 'cdist1_25', 'cdist1_50', 'cdist1_75', 'cdist1_90perc',
                    'JurisHierac', 'llightnight1992', 'sharemin')

#output models into a table
stargazer(tab7.1, tab7.2, tab7.3, tab7.4, tab7.5, tab7.6, tab7.7,  
          out='02_Analysis/Replication/table_7_replication.tex',
          dep.var.labels = table7.dep.var.labels,
          digits = 3,
          float = T,
          keep = table7.keeps,
          covariate.labels = table7.cov.labels,
          se = table7.rse,
          column.sep.width = '2pt',
          title = 'Light Density in Grid Cells (Replication)',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)



