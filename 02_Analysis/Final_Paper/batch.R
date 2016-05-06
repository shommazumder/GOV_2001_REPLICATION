#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 04/19/2016
##PURPOSE: THIS SCRIPT REPLICATES THE ANALYSIS IN BOWLES ET AL (2016)
#######################################################################

####PACKAGES####
required.packages <- c('foreign','ggplot2','stargazer','sandwich','multiwayvcov',
                       'MASS','rms','lmtest','ordinal','aod','glmnet','reshape2',
                       'plyr','dplyr')
if(sum(required.packages %in% rownames(installed.packages()))!=length(required.packages)){
  install.packages(required.packages[which(!(required.packages %in% rownames(installed.packages())))])
}
lapply(required.packages, FUN = function(x){library(x,character.only = TRUE)})

####SETUP####
##Set working directory
setwd(getwd())#make sure to open the rproj file so you don't have directory structure problems!

####FUNCTIONS####
seq.g.var <- function(mod.first, mod.direct, med.vars) {
  require(sandwich)
  mat.x <- model.matrix(mod.direct)
  mat.first <- model.matrix(mod.first)
  n <- nrow(mat.x)
  Fhat <- crossprod(mat.x, mat.first)/n
  Fhat[, !(colnames(mat.first) %in% med.vars)] <- 0
  Mhat.inv <- solve(crossprod(mat.first)/n)
  ghat <- t(estfun(mod.direct)) + Fhat %*% Mhat.inv %*% t(estfun(mod.first))
  meat <- crossprod(t(ghat))/n
  bread <- (t(mat.x)%*%mat.x)/n
  vv <- (n/(n-ncol(mat.x)))*(solve(bread) %*% meat %*% solve(bread))/n
  return(vv)
}

####VARIABLE SELECTION: LASSO####
source('02_Analysis/Final_Paper/Machine_Learning/ML.R')

####MECHANISMS: SEQUENTIAL G####
#Warning: This script takes some time to run since it runs
#several cluster bootstrap procedures for a large dataset
source('02_Analysis/Final_Paper/Sequential-G/seqg-v2.R')

####AFROBAROMETER ANALYSIS: TRUST####
source('02_Analysis/Final_Paper/Afrobarometer/barometer_analysis.R')
