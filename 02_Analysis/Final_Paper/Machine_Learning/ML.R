#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 05/04/2016
##PURPOSE:THIS SCRIPT IMPLEMENTS A LASSO PROCEDURE.
#######################################################################

#library(foreign)
#library(ggplot2)
#library(plyr)
#library(stargazer)
#library(sandwich)
#library(multiwayvcov) 
#library(MASS)
#library(rms)
#library(lmtest)
#library(ordinal)
#library(aod)

setwd("~/Google Drive/GOV_2001_REPLICATION/")

######## Get all table outputs ######################################################################################

source('02_Analysis/Replication/batch_replicate_tables.R')

######## Lasso for Table 2 ######################################################################################

library(glmnet)

var.selection <- function(dataset, model){
  set.seed(02138)
  data <- dataset
  y <- as.matrix(data[,all.vars(model$call$formula)[1]])
  covar <- colnames(data) %in% rownames(as.data.frame(model$coefficients))
  x <- as.matrix(data[,covar])
  mat <- na.omit(cbind(y,x))
  y <- mat[,1]
  x <- mat[,2:ncol(mat)]
  fit <- glmnet(x, y, alpha = 1)
  plot(fit,xvar="lambda")
  cvfit <- cv.glmnet(x, y)
  plot(cvfit)
  coefficients <- coef(fit,s=cvfit$lambda.min)
  dep.var <- paste(as.character( model$terms[[2]] ) ,"~", sep=" ")
  indep.var <- paste(names(coefficients[2:length(coefficients),1])[coefficients[2:length(coefficients),1]!=0] , collapse = "+")
  f <- as.formula(paste(dep.var, indep.var, sep=" "))
  out <- lm(f, data = dataset)
  out.coef <- coeftest(out, vcov = vcovHC(out, "HC1"))
  return(list(coefficients, out, out.coef))
}

## Generating output tables using the lasso-generated tables

out2.1 <- var.selection(tab2_3.dat, tab2.1)[[2]]
out2.1.robust <- var.selection(tab2_3.dat, tab2.1)[[3]]
tab2.1.robust <- coeftest(tab2.1, vcov = vcovHC(tab2.1, "HC1"))

out2.2 <- var.selection(tab2_3.dat, tab2.2)[[2]]
out2.2.robust <- var.selection(tab2_3.dat, tab2.2)[[3]]
tab2.2.robust <- coeftest(tab2.2, vcov = vcovHC(tab2.2, "HC1"))

lasso.1.dep.var.labels <- c('Civil war incidence')
lasso.1.lasso <- list(c('Lasso',"No","Yes","No","Yes"))
lasso.1.spec <- list(c('Specification',"Baseline","Baseline","Secondary","Secondary"))
lasso.1.omit <- list(c('Variables omitted',"No","No","Yes","No"))
lasso.1.cov.labels <- c('Pre-colonial conflict','French colony','Portuguese colony','Belgian colony','Italian colony','German colony','North','South','West','East','Central')

lasso.1.keeps <- c(names(tab2.1$coefficients), names(out2.2$coefficients))

## Table output for cross-country lasso

stargazer(tab2.1.robust, out2.1.robust, tab2.2.robust, out2.2.robust, 
          out = '03_Drafts/Final_Paper/lasso_1.tex',
          dep.var.labels = lasso.1.dep.var.labels,
          covariate.labels = lasso.1.cov.labels,
          digits = 2,
          float = T,
          column.sep.width = '2pt',
          title = 'Cross-country lasso',
          keep = lasso.1.keeps,
          keep.stat = c('n','rsq'),
          add.lines = c(lasso.1.lasso,lasso.1.spec,lasso.1.omit),
          font.size = 'footnotesize')


######## Lasso for interaction effects ######################################################################################

## Interaction formulae for the Afrobarometer/grid cell lasso analysis

f.4.4 <- as.formula(inter_group_trust ~ WarPrevalence14001700 + (age + age2 + male + urban_dum +  + 
                                                                   CivilWarIncidence + ln_export_pop + ln_pop_dens_1400 + lrgdpl2631960 + 
                                                                   abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + 
                                                                   low_tempNUNN + ln_coastline_areaNUNN + islam + ln_avg_gold_pop + 
                                                                   ln_avg_oil_pop + ln_avg_all_diamonds_pop + rugged + factor(education) + 
                                                                   factor(occupation) + factor(religion) + factor(living_conditions))*WarPrevalence14001700)

f.4.5 <- as.formula(identitydumeth ~ WarPrevalence14001700 + (age + age2 + male + urban_dum +  
                                                                CivilWarIncidence + ln_export_pop + ln_pop_dens_1400 + lrgdpl2631960 + 
                                                                abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + 
                                                                low_tempNUNN + ln_coastline_areaNUNN + islam + ln_avg_gold_pop + 
                                                                ln_avg_oil_pop + ln_avg_all_diamonds_pop + rugged + factor(education) + 
                                                                factor(occupation) + factor(religion) + factor(living_conditions))*WarPrevalence14001700)

f.4.6 <- as.formula(identitydumnat ~ WarPrevalence14001700 + (age + age2 + male + urban_dum +  + 
                                                                CivilWarIncidence + ln_export_pop + ln_pop_dens_1400 + lrgdpl2631960 + 
                                                                abs_latitudeNUNN + longitudeNUNN + rain_minNUNN + humid_maxNUNN + 
                                                                low_tempNUNN + ln_coastline_areaNUNN + islam + ln_avg_gold_pop + 
                                                                ln_avg_oil_pop + ln_avg_all_diamonds_pop + rugged + factor(education) + 
                                                                factor(occupation) + factor(religion) + factor(living_conditions))*WarPrevalence14001700)

f.5.2 <- as.formula(ConflictGrid ~ HistoricalConflictGrid + (lpopdens90 + country + 
                                                               d1 + elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + 
                                                               area)*HistoricalConflictGrid)

f.5.4 <- as.formula(llightnight2007 ~ HistoricalConflictGrid + (lpopdens90 + country + 
                                                                  d1 + elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + 
                                                                  area)*HistoricalConflictGrid)

f.6.7 <- as.formula(ConflictGrid ~ HistoricalConflictGrid + (d1 + elev_srtm_pred + 
                                                               rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + 
                                                               gridcity1400 + ethnicdivdum + logslave_obs + grid_cap + cdist1_10 + 
                                                               cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + JurisHierac + 
                                                               llightnight1992 + sharemin + factor(economynum) + zulu_king + 
                                                               merina + monomotapa + lozi + malawi + kilwa + lunda + congo + 
                                                               luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + 
                                                               wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + 
                                                               carthage + factor(country))*HistoricalConflictGrid)

f.7.7 <- as.formula(llightnight2007 ~ HistoricalConflictGrid + (d1 + elev_srtm_pred + 
                                                                  rough + tempav_8008 + precsdnew_80_08 + area + lpopdens90 + 
                                                                  gridcity1400 + ethnicdivdum + logslave_obs + grid_cap + cdist1_10 + 
                                                                  cdist1_25 + cdist1_50 + cdist1_75 + cdist1_90perc + JurisHierac + 
                                                                  llightnight1992 + sharemin + factor(economynum) + zulu_king + 
                                                                  merina + monomotapa + lozi + malawi + kilwa + lunda + congo + 
                                                                  luba + rwanda + buganda + ashanti + yoruba + ethiopia + axum + 
                                                                  wolof + ghana + mali + kush + songhay + kanem_born + clas_egypt + 
                                                                  carthage + factor(country))*HistoricalConflictGrid)

## Function to select variables (no regression output)

interactions.selection <- function(df, f, dep.var){
  set.seed(02138)
  x <- model.matrix(f, df)
  y <- NA
  for (i in as.numeric(rownames(x))){
    y[i] <- as.matrix(dep.var[i])
  }
  y <- na.omit(y)
  fit <- glmnet(x, y, alpha = 1)
  plot(fit,xvar="lambda")
  cvfit <- cv.glmnet(x, y)
  plot(cvfit)
  coefficients <- coef(fit,s=cvfit$lambda.min)
  return(list(coefficients))
}

int4.4 <- as.matrix(interactions.selection(tab4.dat,f.4.4,tab4.dat$inter_group_trust)[[1]])
rownames(int4.4)[78:96]
int4.5 <- as.matrix(interactions.selection(tab4.dat,f.4.5,tab4.dat$identitydumeth)[[1]])
int4.6 <- as.matrix(interactions.selection(tab4.dat,f.4.6,tab4.dat$identitydumnat)[[1]])

## Selected interactions for Afrobarometer data

vars <- rownames(int4.4)[78:96]
col1 <- ifelse(int4.4[78:96]==0," ",ifelse(int4.4[78:96]>0,"Positive","Negative"))
col2 <- ifelse(int4.5[78:96]==0," ",ifelse(int4.5[78:96]>0,"Positive","Negative"))
col3 <- ifelse(int4.6[78:96]==0," ",ifelse(int4.6[78:96]>0,"Positive","Negative"))

int.table.1 <- cbind(col1,col2,col3)

int.1.cov.labels <- c('Age','Age squared','Male','Urban','Civil war incidence','Log slave exports','Log pop density (1400)','GDP per capita','Latitude','Longitude','Rainfall','Humidity','Temperature','Log coastline area','Islam','Log gold per capita','Log oil per capita','Log diamonds per capita','Ruggedness')
int.table.1 <- cbind(int.1.cov.labels,int.table.1)
colnames(int.table.1) <- c("Interacted variable","Inter-group trust","Ethnic identity","National identity")

stargazer(int.table.1,
          #out = '03_Drafts/Final_Paper/lasso_2a.tex',
          column.sep.width = '2pt',
          title = 'Interaction effects selected by lasso (Afrobarometer)',
          font.size = 'footnotesize')

int5.2 <- as.matrix(interactions.selection(tab567.dat,f.5.2,tab567.dat$ConflictGrid)[[1]])
int5.4 <- as.matrix(interactions.selection(tab567.dat,f.5.4,tab567.dat$llightnight2007)[[1]])

int6.7 <- as.matrix(interactions.selection(tab567.dat,f.6.7,tab567.dat$ConflictGrid)[[1]])
rownames(int6.7)[127:145]
int7.7 <- as.matrix(interactions.selection(tab567.dat,f.7.7,tab567.dat$llightnight2007)[[1]])

## Selected interactions for grid cell data

vars <- rownames(int6.7)[127:145]
col1 <- ifelse(int6.7[127:145]==0," ",ifelse(int6.7[127:145]>0,"Positive","Negative"))
col2 <- ifelse(int7.7[127:145]==0," ",ifelse(int7.7[127:145]>0,"Positive","Negative"))

int.table.2 <- cbind(col1,col2)

int.2.cov.labels <- c('Distance to coast','Average elevation','Ruggedness','Average temperature','Average precipitation',
                      'Area','Log pop density (1990)','City in cell in 1400','More than one ethnicity in cell','Log slave exports',
                      'Capital in cell','Distance to capital 0-10 percentile','Distance to capital 10-25 percentile',
                      'Distance to capital 25-50 percentile','Distance to capital 50-75 percentile','Distance to capital 75-90 percentile',
                      'Jurisdictional hierarchy','Log night lights (1992)','Mineral share')
int.table.2 <- cbind(int.2.cov.labels,int.table.2)
colnames(int.table.2) <- c("Interacted variable","Conflict","Light density")

stargazer(int.table.2,
#          out = '03_Drafts/Final_Paper/lasso_2b.tex',
          column.sep.width = '2pt',
          title = 'Interaction effects selected by lasso (grid cell)',
          font.size = 'footnotesize')

