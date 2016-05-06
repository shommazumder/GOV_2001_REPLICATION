library(foreign)
library(plyr)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(stargazer)

##HEAD
afro5 <- read.spss("~/Google Drive/GOV_2001_REPLICATION/02_Analysis/Final_Paper/Afrobarometer/merged_r5_data_0.sav", to.data.frame = TRUE)
afro4 <- read.spss("~/Google Drive/GOV_2001_REPLICATION/02_Analysis/Final_Paper/Afrobarometer/merged_r4_data.sav", to.data.frame = TRUE)
afro3 <- read.spss("~/Google Drive/GOV_2001_REPLICATION/02_Analysis/Final_Paper/Afrobarometer/merged_r3_data.sav", to.data.frame = TRUE)
tab4.dat <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

####AFROBAROMETER 3 - 2005 
afro3 <- afro3[order(afro3$respno),]
View(afro3)
afro3.rep <- afro3[,c("country", "respno", "urbrur", "backchk", "region", "district",
                      "q82", "q84d", "q84c", "q55h")]
#recode ethnic ID questions
afro3.rep$ethid <- as.character(afro3.rep$q82)
#about 5% are not applicable -- 1478
afro3.rep$ethid[afro3.rep$q82=="Ethnic ID only"] <- "1"
afro3.rep$ethid[afro3.rep$q82!="Ethnic ID only"] <- "0"
afro3.rep$ethid[afro3.rep$ethid=="Missing"] <- NA #13 values missing 
afro3.rep$ethid <- as.numeric(as.character(afro3.rep$ethid))
#recode national ID questions
afro3.rep$natid <- as.character(afro3.rep$q82)
afro3.rep$natid[afro3.rep$q82=="National ID only"] <- "1"
afro3.rep$natid[afro3.rep$q82!="National ID only"] <- "0"
afro3.rep$natid[afro3.rep$q82=="Missing"] <- NA #13 values missing
afro3.rep$natid <- as.numeric(as.character(afro3.rep$natid))
#they are the same when compared to tab4.dat identity dummies 
#intragroup trust 
afro3.rep$trust <- as.character(afro3.rep$q84d)
afro3.rep$trust[afro3.rep$trust=="A lot"] <- 3
afro3.rep$trust[afro3.rep$trust=="Somewhat"] <- 2
afro3.rep$trust[afro3.rep$trust=="Just a little"] <- 1
afro3.rep$trust[afro3.rep$trust=="Not at all"] <- 0
afro3.rep$trust[afro3.rep$trust=="Don't know"] <- NA
afro3.rep$trust[afro3.rep$trust=="Missing data"] <- NA
afro3.rep$trust[afro3.rep$trust=="N/A"] <- NA
table(afro3.rep$trust)
afro3.rep$trust <- as.numeric(as.character(afro3.rep$trust))

#NEW VARIABLES
#trust within ethnic group
## (could historic conflict just lower trust levels in general?)
afro3.rep$trust.own <- as.character(afro3.rep$q84c)
afro3.rep$trust.own[afro3.rep$trust.own=="A lot"] <- 3
afro3.rep$trust.own[afro3.rep$trust.own=="Somewhat"] <- 2
afro3.rep$trust.own[afro3.rep$trust.own=="Just a little"] <- 1
afro3.rep$trust.own[afro3.rep$trust.own=="Not at all"] <- 0
afro3.rep$trust.own[afro3.rep$trust.own=="Don't know"] <- NA
afro3.rep$trust.own[afro3.rep$trust.own=="Missing data"] <- NA
afro3.rep$trust.own[afro3.rep$trust.own=="N/A"] <- NA
table(afro3.rep$trust.own)
afro3.rep$trust.own <- as.numeric(as.character(afro3.rep$trust.own))

#check and see if ingroup trust is lower too (use table 4 regressions) 
tab4.ingroup<- lm(afro3.rep$trust.own ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
              lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
              ln_coastline_areaNUNN+islam+
              ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
              f_pothco+
              rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
            data = tab4.dat)

tab4.1.i.cluster <- cluster.se(tab4.ingroup, tab4.dat$townvill)
tab4.ingroup$vcovHC <- vcovHC(tab4.ingroup,type='HC1')
tab4.ingroup$errors <- sqrt(diag(tab4.ingroup$vcovHC))

tab4.ingroup.2 <- lm(afro3.rep$trust.own ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.1.2i.cluster <- cluster.se(tab4.ingroup.2, tab4.dat$townvill)
tab4.ingroup.2$vcovHC <- vcovHC(tab4.ingroup.2,type='HC1')
tab4.ingroup.2$errors <- sqrt(diag(tab4.ingroup.2$vcovHC))


##regressions from besley and RQ
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

tab4.4 <- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)

tab4.4.cluster <- cluster.se(tab4.4, tab4.dat$townvill)
tab4.4$vcovHC <- vcovHC(tab4.4,type='HC1')
tab4.4$errors <- sqrt(diag(tab4.4$vcovHC))


#make table
#put all of the robust standard errors into a list
table4.rse <- list(tab4.1$errors, tab4.ingroup$errors, tab4.4$errors, tab4.ingroup.2$errors)

#put all dependent variables into a vector
table4.dep.var.labels <- c('Inter group (replication)','Intra group','Inter group (replication)',
                           'Intra group')

#put all covariate labels into a vector
table4.cov.labels <- c('War prevalence 1400-1700','Civil war prevalence')

#specify variables to keep in the table
table4.keeps <- c('WarPrevalence14001700','CivilWarIncidence')

#output models into a table
stargazer(tab4.1,tab4.ingroup, tab4.4,tab4.ingroup.2,
     #     out='02_Analysis/Replication/table_4_replication.tex',
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








###EXTRAS
#check to see if trust in police is lower 
## (could historic conflict just lower trust levels in general?)
afro3.rep$police <- as.character(afro3.rep$q55h)
afro3.rep$police[afro3.rep$police=="A lot"] <- 3
afro3.rep$police[afro3.rep$police=="Somewhat"] <- 2
afro3.rep$police[afro3.rep$police=="Just a little"] <- 1
afro3.rep$police[afro3.rep$police=="Not at all"] <- 0
afro3.rep$police[afro3.rep$police=="Don't know/Haven't heard enough"] <- NA
afro3.rep$police[afro3.rep$police=="Missing"] <- NA
afro3.rep$police[afro3.rep$police=="N/A"] <- NA
table(afro3.rep$police)
#use same regression: 
tab4.police<- lm(afro3.rep$police ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
              lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
              ln_coastline_areaNUNN+islam+
              ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
              f_pothco+
              rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
            data = tab4.dat)
tab4.police.2 <- lm(afro3.rep$police ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
               lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
               ln_coastline_areaNUNN+islam+ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
               rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
             data = tab4.dat)
summary(tab4.police)
summary(tab4.police.2)

####AFROBAROMETER 4 - 2008
afro4.rep <- afro4[,c("COUNTRY", "RESPNO", "URBRUR", "BACKCHK", "REGION", "DISTRICT", 
                       "Q83", "Q84A")]
#recode ethnic ID questions
afro4.rep$ethid <- as.character(afro4.rep$Q83)
levels(afro4.rep$Q83)
afro4.rep$ethid[afro4.rep$ethid=="Missing"] <- NA #136 values missing 
afro4.rep$ethid[afro4.rep$Q83=="Ethnic ID only"] <- "1"
afro4.rep$ethid[afro4.rep$Q83!="Ethnic ID only"] <- "0"
afro4.rep$ethid <- as.numeric(as.character(afro4.rep$ethid))
#recode national ID questions
afro4.rep$natid <- as.character(afro4.rep$Q83)
afro4.rep$natid[afro4.rep$Q83=="Missing"] <- NA #136 values missing 
afro4.rep$natid[afro4.rep$Q83=="National ID only"] <- "1"
afro4.rep$natid[afro4.rep$Q83!="National ID only"] <- "0"
afro4.rep$natid <- as.numeric(as.character(afro4.rep$natid))
View(afro4.rep)

###AFROBAROMETER 5 - 2015
#afro5 <- read.spss("~/Google Drive/merged_r5_data_0.sav", to.data.frame = TRUE)
#write.csv(afro5, "barometer_2015.csv")

###Recode questions - recode q21 "govt like a parent variable"
afro5$q21a <- as.character(afro5$Q21)
levels(afro5$Q21)
afro5$q21a[afro5$q21a=="Missing"] <- NA #only 15 of these
afro5$q21a[afro5$q21a=="Agree very strongly with 1"] <- "1"
afro5$q21a[afro5$q21a=="Agree with 1"] <- "2"
afro5$q21a[afro5$q21a=="Agree with 2"] <- "3"
afro5$q21a[afro5$q21a=="Agree very strongly with 2"] <- "4"
afro5$q21a[afro5$q21a=="Agree with neither"] <- "5"
afro5$q21a[afro5$q21a=="Don't know"] <- "6"
afro5$q21a <- as.numeric(as.character(afro5$q21a))

###Recode questions - recode q59h "trust the police"
afro5$q59ha <- as.character(afro5$Q59H)
levels(afro5$Q59H)
table(afro5$Q59H)
afro5$q59ha[afro5$q59ha=="Missing"] <- NA #only 38 of these
afro5$q59ha[afro5$q59ha=="Not at all"] <- "1"
afro5$q59ha[afro5$q59ha=="Just a little"] <- "2"
afro5$q59ha[afro5$q59ha=="Somewhat"] <- "3"
afro5$q59ha[afro5$q59ha=="A lot"] <- "4"
afro5$q59ha[afro5$q59ha=="Don't know/Haven't heard enough"] <- "5"
afro5$q59ha <- as.numeric(as.character(afro5$q59ha))

###Recode questions - recode q59h "trust the army"
afro5$q59ia <- as.character(afro5$Q59I)
levels(afro5$Q59I)
table(afro5$Q59I)
afro5$q59ia[afro5$q59ia=="Missing"] <- NA #only 39 of these
afro5$q59ia[afro5$q59ia=="Not at all"] <- "1"
afro5$q59ia[afro5$q59ia=="Just a little"] <- "2"
afro5$q59ia[afro5$q59ia=="Somewhat"] <- "3"
afro5$q59ia[afro5$q59ia=="A lot"] <- "4"
afro5$q59ia[afro5$q59ia=="Don't know/Haven't heard enough"] <- "5"
afro5$q59ia <- as.numeric(as.character(afro5$q59ia))

#Aggregate at regional, country, and urban/rural level
means <- ddply(afro5, .(COUNTRY, URBRUR, REGION), summarize, q21 = mean(q21a),
q59h = mean(q59ha), q59i = mean(q59ia))

