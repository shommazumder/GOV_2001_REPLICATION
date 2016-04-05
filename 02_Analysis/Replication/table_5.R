#########################################

####REPLICATE TABLE 5####

#read in data for tables 5, 6, 7
tab567.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')

tab5.1<- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
            data=tab567.dat)
tab5.1.cluster <- cluster.se(tab5.1, tab567.dat$country)
tab5.1$vcovHC <- vcovHC(tab5.1,type='HC1')
tab5.1$errors <- sqrt(diag(tab5.1$vcovHC))

tab5.2 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
               elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
             data=tab567.dat)
tab5.2.cluster <- cluster.se(tab5.2, tab567.dat$country)
tab5.2$vcovHC <- vcovHC(tab5.2,type='HC1')
tab5.2$errors <- sqrt(diag(tab5.2$vcovHC))
##need to do Conley SEs 

tab5.3<- lm(llightnight2007~ HistoricalConflictGrid + lpopdens90 + country,
            data=tab567.dat)
tab5.3.cluster <- cluster.se(tab5.3, tab567.dat$country)
tab5.3$vcovHC <- vcovHC(tab5.3,type='HC1')
tab5.3$errors <- sqrt(diag(tab5.3$vcovHC))

tab5.4 <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
               elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
             data=tab567.dat)
tab5.4.cluster <- cluster.se(tab5.4, tab567.dat$country)
tab5.4$vcovHC <- vcovHC(tab5.4,type='HC1')
tab5.4$errors <- sqrt(diag(tab5.4$vcovHC))
##need to do Conley SEs 



#put all of the robust standard errors into a list
table5.rse <- list(tab5.1$errors, tab5.2$errors, tab5.3$errors, tab5.4$errors)

#put all dependent variables into a vector
table5.dep.var.labels <- c('Conflict 1997-2010','Conflict 1997-2010','Log of light density in 2007',
                           'Log of light density in 2007')

#put all covariate labels into a vector
table5.cov.labels <- c('Historical conflict in grid','lpopdensity')

#specify variables to keep in the table
table5.keeps <- c('HistoricalConflictGrid','lpopdens90')

#output models into a table
stargazer(tab5.1,tab5.2,tab5.3,tab5.4,
          out='02_Analysis/Replication/table_5_replication.tex',
          dep.var.labels = table5.dep.var.labels,
          digits = 3,
          float = T,
          keep = table5.keeps,
          covariate.labels = table5.cov.labels,
          se = table5.rse,
          column.sep.width = '2pt',
          title = 'Conflict and Light Density in Grid Cells: Core Results',
          keep.stat = c('n','rsq'),
          font.size = 'footnotesize',
          no.space = T)

