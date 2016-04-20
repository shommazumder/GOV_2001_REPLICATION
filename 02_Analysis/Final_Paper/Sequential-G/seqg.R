#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 04/19/2016
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
intergroup.direct.war$seq.g.var <- seq.g.var(mod.first = intergroup.first.war,
                                         mod.direct = intergroup.direct.war,
                                         med.vars = c('lrgdpl2631960','CivilWarIncidence'))#variance estimation doesn't work right now
coeftest(intergroup.direct.war,vcov=intergroup.direct.war$seq.g.var)
<<<<<<< HEAD
## cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
acde.boots <- pt.boots <- rep(NA, times = boots)
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
  acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(acde.boots)
quantile(acde.boots, probs = c(0.025, 0.975))
=======
>>>>>>> d0416555351873e4356562e5149c598979bd2d93

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
identityeth.direct.war$seq.g.var <- seq.g.var(mod.first = identityeth.first.war,
                                          mod.direct = identityeth.direct.war,
                                          med.vars = c('lrgdpl2631960','CivilWarIncidence'))#variance estimation doesn't work right now
coeftest(identityeth.direct.war,vcov=identityeth.direct.war$seq.g.var)
<<<<<<< HEAD
## cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
acde.boots <- pt.boots <- rep(NA, times = boots)
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
  acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(acde.boots)
quantile(acde.boots, probs = c(0.025, 0.975))
=======
>>>>>>> d0416555351873e4356562e5149c598979bd2d93

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
identitynat.direct.war$seq.g.var <- seq.g.var(mod.first = identitynat.first.war,
                                          mod.direct = identitynat.direct.war,
                                          med.vars = c('lrgdpl2631960','CivilWarIncidence'))#variance estimation doesnt' work right now
coeftest(identitynat.direct.war,vcov=identitynat.direct.war$seq.g.var)
<<<<<<< HEAD
## cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
acde.boots <- pt.boots <- rep(NA, times = boots)
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
  acde.boots[b] <- coef(boot.direct)["WarPrevalence14001700"]
  pt.boots[b] <- coef(boot.first)["WarPrevalence14001700"]
}
sd(acde.boots)
quantile(acde.boots, probs = c(0.025, 0.975))
=======
>>>>>>> d0416555351873e4356562e5149c598979bd2d93

####GRID-LEVEL ANALYSIS####
#ACDE of historical conflict netting out popden90
grid.conflict.first.1 <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
            data=BRQ_grid)
grid.conflict.direct.1 <- lm(I(ConflictGrid-coef(grid.conflict.first.1)['lpopdens90'])~HistoricalConflictGrid+country,
                             data=BRQ_grid)
grid.conflict.direct.1$seq.g.var <- seq.g.var(mod.first = grid.conflict.first.1,
                                              mod.direct = grid.conflict.direct.1,
                                              med.vars = 'lpopdens90')
coeftest(grid.conflict.direct.1,vcov=grid.conflict.direct.1$seq.g.var)
## cluster bootstrap the SEs
set.seed(02138)
boots <- 1000
acde.boots <- ate.boots <- pt.boots <- rep(NA, times = boots)
munis <- unique(BRQ_grid$country)
lookup <- split(1:nrow(BRQ_grid), BRQ_grid$country)
codes <- names(lookup)
for (b in 1:boots) {
  draw <- sample(codes, length(codes), replace = TRUE)
  star <- unlist(lookup[draw])
  brq.star <- BRQ_grid[star,]
  ##fear.star <- fear[sample(1:nrow(fear), replace = TRUE),]
  ate.mod <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
                data=brq.star)
  boot.first <- lm(ConflictGrid ~ HistoricalConflictGrid + lpopdens90 + country,
                   data=brq.star)
  boot.direct  <- lm(I(ConflictGrid-coef(grid.conflict.first.1)['lpopdens90'])~HistoricalConflictGrid+country,
                     data=brq.star)
  acde.boots[b] <- coef(boot.direct)["HistoricalConflictGrid"]
  ate.boots[b] <- coef(ate.mod)["HistoricalConflictGrid"]
  pt.boots[b] <- coef(boot.first)["HistoricalConflictGrid"]
}
sd(acde.boots)
quantile(acde.boots, probs = c(0.025, 0.975))

