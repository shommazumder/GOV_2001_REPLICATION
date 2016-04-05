####  REPLICATE TABLE 6  ####

tab567.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

library(aod)

tab6.1 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + 
               precsdnew_80_08 + area + lpopdens90 + gridcity1400 + zulu_king + merina + monomotapa + 
               lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + 
               ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + 
               carthage + factor(country), 
             data=tab567.dat)
tab6.1.cluster <- cluster.se(tab6.1, tab567.dat$country)

tab6.1$vcovCL <- cluster.vcov(tab6.1,tab567.dat$country)

coef.names <- rownames(as.matrix(coef(tab6.1)))
dummies.precolonial <- 11:33

tab6.1.wald <- wald.test(b = coef(tab6.1), Sigma=tab6.1$vcovCL, Terms = dummies.precolonial, df=51)

tab6.2 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 +  + ethnicdivdum + factor(country), 
             data=tab567.dat)
tab6.2.cluster <- cluster.se(tab6.2, tab567.dat$country)
tab6.2$vcovCL <- cluster.vcov(tab6.2,tab567.dat$country)

tab6.3 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + logslave_obs + factor(country),
             data=tab567.dat)
tab6.3.cluster <- cluster.se(tab6.3, tab567.dat$country)
tab6.3$vcovCL <- cluster.vcov(tab6.3, tab567.dat$country)

tab6.4 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + grid_cap + factor(country),
             data=tab567.dat)
tab6.4.cluster <- cluster.se(tab6.4, tab567.dat$country)
tab6.4$vcovCL <- cluster.vcov(tab6.4, tab567.dat$country)

tab6.5 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + JurisHierac + factor(country), 
             data=tab567.dat)
tab6.5.cluster <- cluster.se(tab6.5, tab567.dat$country)
tab6.5$vcovCL <- cluster.vcov(tab6.5, tab567.dat$country)

tab6.6 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + llightnight1992 + sharemin + factor(economynum) + factor(country), 
             data=tab567.dat)
tab6.6.cluster <- cluster.se(tab6.6, tab567.dat$country)
tab6.6$vcovCL <- cluster.vcov(tab6.6, tab567.dat$country)

tab6.6.vcov.CL <- cluster.vcov(tab6.6,tab567.dat$country)
coef.names <- rownames(as.matrix(coef(tab6.6)))
dummies.economic <- 12:38

tab6.6.wald <- wald.test(b = coef(tab6.6), Sigma=tab6.6.vcov.CL, Terms = dummies.economic, df=51)

tab6.7 <- lm(ConflictGrid ~ HistoricalConflictGrid + d1 + elev_srtm_pred + rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + gridcity1400 + ethnicdivdum + logslave_obs + grid_cap + cdist1_10 + cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + JurisHierac + llightnight1992 + sharemin + factor(economynum) + zulu_king + merina + monomotapa + lozi + malawi + kilwa + lunda + congo + luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + carthage + factor(country),
             data=tab567.dat)
tab6.7.cluster <- cluster.se(tab6.7, tab567.dat$country)
tab6.7$vcovCL <- cluster.vcov(tab6.7, tab567.dat$country)

#put all of the clustered SEs into a list
table6.rse <- list(sqrt(diag(tab6.1$vcovCL)),sqrt(diag(tab6.2$vcovCL)),sqrt(diag(tab6.3$vcovCL)),sqrt(diag(tab6.4$vcovCL)),
                   sqrt(diag(tab6.5$vcovCL)),sqrt(diag(tab6.6$vcovCL)),sqrt(diag(tab6.7$vcovCL)))

#put all covariate labels into a vector
table6.cov.labels <- c('Historic Conflict in Grid', 'City in 1400', 'Ethnic diversity',
                       'Slave exports', 'Capital city in Grid', 'Distance to capital 0-10\\%', 
                       'Distance to capital 10-25\\%', 'Distance to capital 25-50\\%', 'Distance to capital 50-75\\%',
                       'Distance to capital 75-90\\%', 'Jurisdictional hierarchy', 'Log light density 1992', 
                       'Mineral share')

#specify variables to keep in the table
table6.keeps <- c('HistoricalConflictGrid', 'gridcity1400', 'ethnicdivdum', 'logslave_obs', 
                  'grid_cap', 'cdist1_10', 'cdist1_25', 'cdist1_50', 'cdist1_75', 'cdist1_90perc',
                  'JurisHierac', 'llightnight1992', 'sharemin')

#output models into a table
stargazer(tab6.1,tab6.2,tab6.3,tab6.4,tab6.5,tab6.6,tab6.7,
          out='02_Analysis/Replication/table_6_replication.tex',
          dep.var.labels = NULL,
          omit='country', 
          omit.labels = 'Country FE',
          digits = 3,
          float = T,
          keep = table6.keeps,
          covariate.labels = table6.cov.labels,
          se = table6.rse,
          column.sep.width = '2pt',
          title = 'Conflict in Grid Cells (Replication)',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)
