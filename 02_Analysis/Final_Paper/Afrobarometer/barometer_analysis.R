library(foreign)
library(plyr)

afro5 <- read.spss("~/Google Drive/merged_r5_data_0.sav", to.data.frame = TRUE)
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

