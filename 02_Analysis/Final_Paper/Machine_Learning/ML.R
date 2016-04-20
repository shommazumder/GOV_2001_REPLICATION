#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 04/19/2016
##PURPOSE:INSERT PURPOSE
#######################################################################

setwd("~/Google Drive/GOV_2001_REPLICATION/")

######## Get all table outputs ######################################################################################

source('02_Analysis/Replication/batch_replicate_tables.R')

######## Experimenting with Lasso ######################################################################################

library(glmnet)

var.selection <- function(dataset, model){
  data <- dataset
  y <- as.matrix(data[,all.vars(model$call)[1]])
  covar <- colnames(data) %in% rownames(as.data.frame(model$coefficients))
  x <- as.matrix(data[,covar])
  mat <- na.omit(cbind(y,x))
  y <- mat[,1]
  x <- mat[,2:ncol(mat)]
<<<<<<< HEAD
  fit <- glmnet(x, y, alpha = .5)
=======
  fit <- glmnet(x, y, alpha = 1)
>>>>>>> d0416555351873e4356562e5149c598979bd2d93
  plot(fit,xvar="lambda")
  cvfit <- cv.glmnet(x, y)
  plot(cvfit)
  return(coef(fit,s=cvfit$lambda.min))
}

# Should point estimates be stable across iterations?
var.selection(tab4.dat, tab4.1)

