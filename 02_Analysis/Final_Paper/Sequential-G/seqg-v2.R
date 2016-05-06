#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 05/04/2016
##PURPOSE: THIS PAPER EVALUATES THE ACDE OF HISTORICAL CONFLICT
#######################################################################

####PREP####

#read in afrobarometer data
BRQ_ab <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

#read in grid-level data
BRQ_grid <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta')


####AFROBAROMETER ANALYSIS####
#ACDE of war on trust netting out present day GDP per capita
intergroup.first <- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
              lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
              ln_coastline_areaNUNN+islam+
              ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
              f_pothco+
              rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
            data = BRQ_ab)
intergroup.first$clusterVCOV <- cluster.vcov(intergroup.first,BRQ_ab$townvill)
coeftest(intergroup.first,vcov=intergroup.first$clusterVCOV)

intergroup.direct <- lm(I(inter_group_trust - coef(intergroup.first)['lrgdpl2631960'])~age+age2+male+urban_dum+
                          WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                          abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)
intergroup.direct$seq.g.var <- seq.g.var(mod.first = intergroup.first,
                                         mod.direct = intergroup.direct,
                                         med.vars = 'lrgdpl2631960')
coeftest(intergroup.direct,vcov=intergroup.direct$seq.g.var)

#ACDE of war on ethnic identity netting out present day GDP per capita
identityeth.first <- lm(identitydumeth ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                         lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                         ln_coastline_areaNUNN+islam+
                         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                         f_pothco+
                         rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                       data = BRQ_ab)
identityeth.first$clusterVCOV <- cluster.vcov(identityeth.first,BRQ_ab$townvill)
coeftest(identityeth.first,vcov=identityeth.first$clusterVCOV)

identityeth.direct <- lm(I(identitydumeth - coef(identityeth.first)['lrgdpl2631960'])~age+age2+male+urban_dum+
                          WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                          abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)
identityeth.direct$seq.g.var <- seq.g.var(mod.first = identityeth.first,
                                         mod.direct = identityeth.direct,
                                         med.vars = 'lrgdpl2631960')
coeftest(identityeth.direct,vcov=identityeth.direct$seq.g.var)


#ACDE of war on national identity netting out present day GDP per capita
identitynat.first <- lm(identitydumnat ~ age+age2+male+urban_dum+WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                          lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)
identitynat.first$clusterVCOV <- cluster.vcov(identitynat.first,BRQ_ab$townvill)
coeftest(identitynat.first,vcov=identitynat.first$clusterVCOV)

identitynat.direct <- lm(I(identitydumnat - coef(identitynat.first)['lrgdpl2631960'])~age+age2+male+urban_dum+
                           WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                           abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                           ln_coastline_areaNUNN+islam+
                           ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                           f_pothco+
                           rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                         data = BRQ_ab)
identitynat.direct$seq.g.var <- seq.g.var(mod.first = identitynat.first,
                                          mod.direct = identitynat.direct,
                                          med.vars = 'lrgdpl2631960')
coeftest(identitynat.direct,vcov=identitynat.direct$seq.g.var)

####ACDE OF WAR ON TRUST/IDENTITY NETTING OUT GDP AND WAR####

#ACDE of war on trust netting out present day GDP per capita and civil war
intergroup.first.war <- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+
                         ln_export_pop+ln_pop_dens_1400+
                         lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                         ln_coastline_areaNUNN+islam+
                         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                         f_pothco+
                         rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                       data = BRQ_ab)
intergroup.first.war$clusterVCOV <- cluster.vcov(intergroup.first.war,BRQ_ab$townvill)
coeftest(intergroup.first.war,vcov=intergroup.first.war$clusterVCOV)

intergroup.direct.war <- lm(I(inter_group_trust - coef(intergroup.first.war)['lrgdpl2631960']-coef(intergroup.first.war)['CivilWarIncidence'])~
                          age+age2+male+urban_dum+
                          WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                          abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)

## cluster bootstrap the SEs
set.seed(02138)
boots <- 500
intergroup.war.acde.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_ab$townvill)
lookup <- split(1:nrow(BRQ_ab), BRQ_ab$townvill)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_ab[star,]
  ##fear.star <- fear[sample(1:nrow(fear), replace = TRUE),]
  #ate.mod <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
   #             data=brq.star)
  boot.first <- lm(inter_group_trust ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+
                     ln_export_pop+ln_pop_dens_1400+
                     lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                     ln_coastline_areaNUNN+islam+
                     ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                     f_pothco+
                     rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                   data = brq.star)
  boot.direct  <- lm(I(inter_group_trust - coef(boot.first)['lrgdpl2631960']-coef(boot.first)['CivilWarIncidence'])~
                       age+age2+male+urban_dum+
                       WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                       abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                       ln_coastline_areaNUNN+islam+
                       ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                       f_pothco+
                       rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                     data = brq.star)
  intergroup.war.acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(intergroup.war.acde.boots)
quantile(intergroup.war.acde.boots, probs = c(0.025, 0.975))

#ACDE of war on ethnic identity netting out present day GDP per capita
identityeth.first.war <- lm(identitydumeth ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
                          lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)
identityeth.first.war$clusterVCOV <- cluster.vcov(identityeth.first.war,BRQ_ab$townvill)
coeftest(identityeth.first.war,vcov=identityeth.first.war$clusterVCOV)

identityeth.direct.war <- lm(I(identitydumeth - coef(identityeth.first.war)['lrgdpl2631960']-coef(identityeth.first.war)['CivilWarIncidence'])~
                           age+age2+male+urban_dum+
                           WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                           abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                           ln_coastline_areaNUNN+islam+
                           ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                           f_pothco+
                           rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                         data = BRQ_ab)

## cluster bootstrap the SEs
set.seed(02138)
boots <- 500
identityeth.war.acde.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_ab$townvill)
lookup <- split(1:nrow(BRQ_ab), BRQ_ab$townvill)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_ab[star,]
  ##fear.star <- fear[sample(1:nrow(fear), replace = TRUE),]
  #ate.mod <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
  #             data=brq.star)
  boot.first <- lm(identitydumeth ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+
                     ln_export_pop+ln_pop_dens_1400+
                     lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                     ln_coastline_areaNUNN+islam+
                     ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                     f_pothco+
                     rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                   data = brq.star)
  boot.direct  <- lm(I(identitydumeth - coef(boot.first)['lrgdpl2631960']-coef(boot.first)['CivilWarIncidence'])~
                       age+age2+male+urban_dum+
                       WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                       abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                       ln_coastline_areaNUNN+islam+
                       ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                       f_pothco+
                       rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                     data = brq.star)
  identityeth.war.acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(identityeth.war.acde.boots)
quantile(identityeth.war.acde.boots, probs = c(0.025, 0.975))


#ACDE of war on national identity netting out present day GDP per capita
identitynat.first.war <- lm(identitydumnat ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+ln_export_pop+ln_pop_dens_1400+
                          lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                          ln_coastline_areaNUNN+islam+
                          ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                          f_pothco+
                          rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                        data = BRQ_ab)
identitynat.first.war$clusterVCOV <- cluster.vcov(identitynat.first.war,BRQ_ab$townvill)
coeftest(identitynat.first.war,vcov=identitynat.first.war$clusterVCOV)

identitynat.direct.war <- lm(I(identitydumnat - coef(identitynat.first.war)['lrgdpl2631960']-coef(identitynat.first.war)['CivilWarIncidence'])~
                           age+age2+male+urban_dum+
                           WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                           abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                           ln_coastline_areaNUNN+islam+
                           ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                           f_pothco+
                           rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                         data = BRQ_ab)

## cluster bootstrap the SEs
set.seed(02138)
boots <- 500
identitynat.war.acde.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_ab$townvill)
lookup <- split(1:nrow(BRQ_ab), BRQ_ab$townvill)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_ab[star,]
  ##fear.star <- fear[sample(1:nrow(fear), replace = TRUE),]
  #ate.mod <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
  #             data=brq.star)
  boot.first <- lm(identitydumnat ~ age+age2+male+urban_dum+WarPrevalence14001700+CivilWarIncidence+
                     ln_export_pop+ln_pop_dens_1400+
                     lrgdpl2631960+abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                     ln_coastline_areaNUNN+islam+
                     ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                     f_pothco+
                     rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                   data = brq.star)
  boot.direct  <- lm(I(identitydumnat - coef(boot.first)['lrgdpl2631960']-coef(boot.first)['CivilWarIncidence'])~
                       age+age2+male+urban_dum+
                       WarPrevalence14001700+ln_export_pop+ln_pop_dens_1400+
                       abs_latitudeNUNN+longitudeNUNN+rain_minNUNN+humid_maxNUNN+low_tempNUNN+
                       ln_coastline_areaNUNN+islam+
                       ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
                       f_pothco+
                       rugged+factor(education)+factor(occupation)+factor(religion)+factor(living_conditions),
                     data = brq.star)
  identitynat.war.acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(identitynat.war.acde.boots)
quantile(identitynat.war.acde.boots, probs = c(0.025, 0.975))

####GRID-LEVEL ANALYSIS####
##ACDE of historical conflict on contemporary conflict netting out popden90
grid.conflict.first.1 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
                              elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
            data=BRQ_grid)
grid.conflict.direct.1 <- lm(I(ConflictGrid-coef(grid.conflict.first.1)['lpopdens90'])~HistoricalConflictGrid + country+ d1 +
                               elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                             data=BRQ_grid)
coeftest(grid.conflict.direct.1,vcov=grid.conflict.direct.1$seq.g.var)
# cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
grid.conflict.acde.boots <- ate.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_grid$country)
lookup <- split(1:nrow(BRQ_grid), BRQ_grid$country)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_grid[star,]
  boot.first <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
                     elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                   data=brq.star)
  boot.direct  <- lm(I(ConflictGrid-coef(boot.first)['lpopdens90'])~HistoricalConflictGrid+country + d1 +
                       elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                     data=brq.star)
  grid.conflict.acde.boots[b] <- coef(boot.direct)["HistoricalConflictGrid"]
  pt.boots[b] <- coef(boot.first)["HistoricalConflictGrid"]
}
sd(grid.conflict.acde.boots)
quantile(grid.conflict.acde.boots, probs = c(0.025, 0.975))

##ACDE of historical conflict on light density netting out popden90
grid.light.first.1 <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
                           elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                            data=BRQ_grid)
grid.light.direct.1 <- lm(I(llightnight2007-coef(grid.light.first.1)['lpopdens90'])~HistoricalConflictGrid + country + d1 +
                            elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                             data=BRQ_grid)
# cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
grid.light.acde.boots <- ate.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_grid$country)
lookup <- split(1:nrow(BRQ_grid), BRQ_grid$country)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_grid[star,]
  boot.first <- lm(llightnight2007 ~ HistoricalConflictGrid + lpopdens90 + country + d1 +
                     elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                   data=brq.star)
  boot.direct  <- lm(I(llightnight2007-coef(boot.first)['lpopdens90'])~HistoricalConflictGrid+country + d1 +
                       elev_srtm_pred + rough + precsdnew_80_08 + tempav_8008 + area ,
                     data=brq.star)
  grid.light.acde.boots[b] <- coef(boot.direct)["HistoricalConflictGrid"]
  pt.boots[b] <- coef(boot.first)["HistoricalConflictGrid"]
}
sd(grid.light.acde.boots)
quantile(grid.light.acde.boots, probs = c(0.025, 0.975))

####PLOTS####

##Afrobarometer Questions (Ethnic Identity)

#create dataframe for plotting
identity.acde <- c(coef(identityeth.direct)['WarPrevalence14001700'],
                   coef(identitynat.direct)['WarPrevalence14001700'],
                   coef(intergroup.direct)['WarPrevalence14001700'])
identity.acde.se <- c(sqrt(diag(identityeth.direct$seq.g.var))['WarPrevalence14001700'],
                      sqrt(diag(identitynat.direct$seq.g.var))['WarPrevalence14001700'],
                      sqrt(diag(intergroup.direct$seq.g.var))['WarPrevalence14001700'])
identity.acde.lower.95 <- identity.acde-1.96*identity.acde.se
identity.acde.upper.95 <- identity.acde+1.96*identity.acde.se
identity.acde.plot.df <- data.frame(cbind(identity.acde,identity.acde.lower.95,
                                          identity.acde.upper.95,Model=c('Ethnic','National','Intergroup.Trust')),
                                    row.names = NULL)
identity.acde.plot.df$identity.acde <- as.numeric(as.character(identity.acde.plot.df$identity.acde))
identity.acde.plot.df$identity.acde.lower.95 <- as.numeric(as.character(identity.acde.plot.df$identity.acde.lower.95))
identity.acde.plot.df$identity.acde.upper.95 <- as.numeric(as.character(identity.acde.plot.df$identity.acde.upper.95))

#plot ACDEs
identity.acde.plot <- identity.acde.plot.df %>% ggplot(aes(x=Model,y=identity.acde,group=Model)) + 
  geom_point() + 
  geom_hline(yintercept = 0,size = 0.8) + 
  geom_linerange(aes(ymax = identity.acde.upper.95,ymin=identity.acde.lower.95)) +
  scale_x_discrete(labels = c('Prefer Ethnic Identity','Intergroup Trust','Prefer National Identity')) +
  ylab('ACDE') +
  ggtitle('Average Controlled Direct Effect of Historical War Prevalance \n on Contemporary Identity and Trust')+
  theme_bw() + 
  theme(text = element_text(size=15))
print(identity.acde.plot)

##Afrobarometer Questions (Identity Netting out Income and War)

#create dataframe for plotting
identity.war.acde <- c(coef(identityeth.direct.war)['WarPrevalence14001700'],
                   coef(identitynat.direct.war)['WarPrevalence14001700'],
                   coef(intergroup.direct.war)['WarPrevalence14001700'])
identity.war.acde.lower.95 <- c(quantile(identityeth.war.acde.boots,probs = 0.025),
                                quantile(identitynat.war.acde.boots,probs = 0.025),
                                quantile(intergroup.war.acde.boots,probs = 0.025))
identity.war.acde.upper.95 <- c(quantile(identityeth.war.acde.boots,probs = 0.975),
                                quantile(identitynat.war.acde.boots,probs = 0.975),
                                quantile(intergroup.war.acde.boots,probs = 0.975))
identity.war.acde.plot.df <- data.frame(cbind(identity.war.acde,identity.war.acde.lower.95,
                                              identity.war.acde.upper.95,Model=c('Ethnic','National','Intergroup.Trust')),
                                        row.names = NULL)
identity.war.acde.plot.df$identity.war.acde <- as.numeric(as.character(identity.war.acde.plot.df$identity.war.acde))
identity.war.acde.plot.df$identity.war.acde.lower.95 <- as.numeric(as.character(identity.war.acde.plot.df$identity.war.acde.lower.95))
identity.war.acde.plot.df$identity.war.acde.upper.95 <- as.numeric(as.character(identity.war.acde.plot.df$identity.war.acde.upper.95))

#plot ACDEs
identity.war.acde.plot <- identity.war.acde.plot.df %>% ggplot(aes(x=Model,y=identity.war.acde,group=Model)) + 
  geom_point() + 
  geom_hline(yintercept = 0,size=0.5) + 
  geom_linerange(aes(ymax = identity.war.acde.upper.95,ymin=identity.war.acde.lower.95))+
  scale_x_discrete(labels = c('Prefer Ethnic Identity','Intergroup Trust','Prefer National Identity'))+
  ylab('ACDE')+
  ggtitle('Average Controlled Direct Effect of Historical War Prevalance \n on Contemporary Identity and Trust \n (Net of Contemporary Income and Civil Conflict)')+
  theme_bw() + 
  theme(text = element_text(size=15))
print(identity.war.acde.plot)

##GRID-LEVEL PLOTS

#create dataframe for plotting
grid.acde <- c(coef(grid.conflict.direct.1)['HistoricalConflictGrid'],
                       coef(grid.light.direct.1)['HistoricalConflictGrid'])
grid.acde.lower.95 <- c(quantile(grid.conflict.acde.boots,probs = 0.025),
                                quantile(grid.light.acde.boots,probs = 0.025))
grid.acde.upper.95 <- c(quantile(grid.conflict.acde.boots,probs = 0.975),
                        quantile(grid.light.acde.boots,probs = 0.975))
grid.acde.plot.df <- data.frame(cbind(grid.acde,grid.acde.lower.95,
                                      grid.acde.upper.95,Model=c('Conflict','Light')),
                                row.names = NULL)
grid.acde.plot.df$grid.acde <- as.numeric(as.character(grid.acde.plot.df$grid.acde))
grid.acde.plot.df$grid.acde.lower.95 <- as.numeric(as.character(grid.acde.plot.df$grid.acde.lower.95))
grid.acde.plot.df$grid.acde.upper.95 <- as.numeric(as.character(grid.acde.plot.df$grid.acde.upper.95))

#plot ACDEs
grid.acde.plot <- grid.acde.plot.df %>% ggplot(aes(x=Model,y=grid.acde,group=Model)) + 
  geom_point() + 
  geom_hline(yintercept = 0,size=0.5) + 
  geom_linerange(aes(ymax = grid.acde.upper.95,ymin=grid.acde.lower.95))+
  ylab('ACDE')+
  ggtitle('Average Controlled Direct Effect of Historical Conflict \n on Contemporary Conflict and Development \n (Net of Pop. Density in 1990)')+
  theme_bw() + 
  theme(text = element_text(size=15))
print(grid.acde.plot)
