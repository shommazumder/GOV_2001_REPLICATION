#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 04/19/2016
##PURPOSE: THIS SCRIPT REPLICATES THE ANALYSIS IN BOWLES ET AL (2016)
#######################################################################

####PACKAGES####
required.packages <- c('foreign','ggplot2','stargazer','sandwich','multiwayvcov',
                       'MASS','rms','lmtest','ordinal','aod','glmnet')
if(sum(required.packages %in% rownames(installed.packages()))!=length(required.packages)){
  install.packages(required.packages[which(!(required.packages %in% rownames(installed.packages())))])
}
lapply(required.packages, library, character.only = TRUE)

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

####MATCHING####


####MECHANISMS####


####VARIABLE SELECTION####


####MISCELLANEOUS####