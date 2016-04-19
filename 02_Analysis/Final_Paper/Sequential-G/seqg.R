#######################################################################
##AUTHORS: JEREMY BOWLES, JESSIE BULLOCK, AND SHOM MAZUMDER
##DATE CREATED: 04/19/2016
##DATE LAST UPDATED: 04/19/2016
##PURPOSE: THIS PAPER EVALUATES THE ACDE OF HISTORICAL CONFLICT
#######################################################################

####PREP####
#read in data
BRQ_ab <- read.dta('01_Data/REPLICATION_APSR_2014_BRQ/REPLICATION_TABLE_4_APSR_2014_BRQ.dta')

####ANALYSIS####
#Demediated Effect of war on trust netting out present day GDP per capita
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