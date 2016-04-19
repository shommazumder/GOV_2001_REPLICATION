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


####MATCHING####


####MECHANISMS####


####VARIABLE SELECTION####


####MISCELLANEOUS####