


Use REPLICATION_TABLE_2_3_APSR_2014_BRQ.dta

***Table 2


regress  CivilWarIncidence WarPrevalence14001700  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN, robust
regress  CivilWarIncidence WarPrevalence14001700  lrgdpl2631970   region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop   ETHPOL   yellow rugged, robust
regress  Purges WarPrevalence14001700  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN, robust
regress   Purges  WarPrevalence14001700  lrgdpl2631970 legor_frNUNN f_french f_spain f_pothco f_dutch f_belg f_italy f_germ   region_nNUNN region_sNUNN region_wNUNN region_eNUNN, robust
ologit  ConflictOrderedVar WarPrevalence14001700  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ   region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN, robust
ologit   ConflictOrderedVar WarPrevalence14001700  lrgdpl2631970 legor_frNUNN f_french f_spain f_pothco f_dutch f_belg f_italy f_germ   region_nNUNN region_sNUNN region_wNUNN region_eNUNN, robust
regress  CivilWarIncidence WarPrevalence14001700 ln_export_area  ln_pop_dens_1400 lrgdpl2631970   region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop   ETHPOL   yellow rugged, robust
regress   Purges  WarPrevalence14001700 ln_export_area  ln_pop_dens_1400 lrgdpl2631970   region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop   ETHPOL   yellow rugged, robust
ologit   ConflictOrderedVar  WarPrevalence14001700 ln_export_area  ln_pop_dens_1400 lrgdpl2631970   region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  f_french f_spain f_pothco f_dutch f_belg f_italy f_germ  abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop   ETHPOL   yellow rugged, robust

***Table 3

regress   lrgdpl2632000  WarPrevalence14001700   f_french f_spain f_pothco f_dutch f_belg f_italy f_germ region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN, robust
regress   lrgdpl2632000 WarPrevalence14001700 lrgdpl2631970 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged, robust
regress   lrgdpl2632000 WarPrevalence14001700 ln_export_pop  ln_pop_dens_1400 lrgdpl2631970 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged, robust
regress  avexpr WarPrevalence14001700 lrgdpl2631970 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged   f_french f_spain f_pothco f_dutch f_belg f_italy f_germ, robust
regress  xconst5 WarPrevalence14001700 lrgdpl2631970 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged   f_french f_spain f_pothco f_dutch f_belg f_italy f_germ, robust

clear

use REPLICATION_TABLE_4_APSR_2014_BRQ.dta
 
***Table 4

xi: reg inter_group_trust age age2 male urban_dum WarPrevalence14001700 ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged  i.education i.occupation i.religion i.living_conditions , cluster(townvill)
xi: reg identitydumeth   age age2 male urban_dum WarPrevalence14001700 ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged  i.education i.occupation i.religion i.living_conditions  , cluster(townvill)
xi: reg identitydumnat   age age2 male urban_dum WarPrevalence14001700   ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged  i.education i.occupation i.religion i.living_conditions , cluster(townvill)
xi: reg inter_group_trust age age2 male urban_dum WarPrevalence14001700   CivilWarIncidence  ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged    i.education i.occupation i.religion i.living_conditions, cluster(townvill)
xi: reg identitydumeth   age age2 male urban_dum WarPrevalence14001700 CivilWarIncidence ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged  i.education i.occupation i.religion i.living_conditions  , cluster(townvill)
xi: reg identitydumnat   age age2 male urban_dum WarPrevalence14001700 CivilWarIncidence  ln_export_pop  ln_pop_dens_1400 lrgdpl2631960 abs_latitudeNUNN longitudeNUNN rain_minNUNN humid_maxNUNN low_tempNUNN ln_coastline_areaNUNN island_dumNUNN islam legor_frNUNN region_nNUNN  region_nNUNN  region_sNUNN region_wNUNN region_eNUNN region_cNUNN  ln_avg_gold_pop ln_avg_oil_pop ln_avg_all_diamonds_pop f_french f_spain f_pothco f_dutch f_belg f_italy f_germ ethnic_fractionalizationNUNN yellow rugged  i.education i.occupation i.religion i.living_conditions  , cluster(townvill)

clear

use  REPLICATION_TABLE_5_6_7_APSR_2014_BRQ.dta

*****TABLE 5

xi: regress ConflictGrid HistoricalConflictGrid lpopdens i.country, cluster(country)
xi: regress ConflictGrid HistoricalConflictGrid lpopdens d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area i.country, cluster(country)
xi: regress llightnight2007  HistoricalConflictGrid lpopdens i.country, cluster(country)
xi: regress llightnight2007 HistoricalConflictGrid lpopdens d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area i.country, cluster(country)
 
gen larea=log(area) 

 ****TABLE 6
   
xi: regress ConflictGrid  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area  lpopdens gridcity1400 zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage  i.country, cluster(country)
test zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage
xi: regress ConflictGrid  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area lpopdens   ethnicdivdum   i.country, cluster(country)
xi: regress ConflictGrid HistoricalConflictGrid d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area  lpopdens  logslave_obs  i.country, cluster(country)
xi: regress ConflictGrid  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area lpopdens cdist1_10 cdist1_25 cdist1_50 cdist1_75 cdist1_90 grid_cap  i.country, cluster(country)
xi: regress ConflictGrid HistoricalConflictGrid lpopdens d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area  JurisHierac i.country, cluster(country)
xi: regress ConflictGrid  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area   lpopdens llightnight1992 sharemin i.economynum   i.country, cluster(country)
test _Ieconomynu_1  _Ieconomynu_2  _Ieconomynu_3  _Ieconomynu_4 _Ieconomynu_5   _Ieconomynu_6  _Ieconomynu_7  _Ieconomynu_8 _Ieconomynu_9  _Ieconomynu_10  _Ieconomynu_11  _Ieconomynu_12 _Ieconomynu_13   _Ieconomynu_14  _Ieconomynu_15  _Ieconomynu_16 _Ieconomynu_17  _Ieconomynu_18  _Ieconomynu_19  _Ieconomynu_20 _Ieconomynu_21   _Ieconomynu_22  _Ieconomynu_23  _Ieconomynu_24 _Ieconomynu_25  _Ieconomynu_26  _Ieconomynu_27 
xi: regress ConflictGrid  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area  lpopdens gridcity1400  ethnicdivdum logslave_obs  grid_cap cdist1_10 cdist1_25 cdist1_50 cdist1_75 cdist1_90   JurisHierac   llightnight1992 sharemin i.economynum   zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage     i.country, cluster(country)

***TABLE 7

xi: regress llightnight2007  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area  lpopdens gridcity1400 zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage  i.country, cluster(country)
test zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage
xi: regress llightnight2007  HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area lpopdens   ethnicdivdum  i.country, cluster(country)
xi: regress llightnight2007 HistoricalConflictGrid d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area  lpopdens  logslave_obs  i.country, cluster(country)
xi: regress llightnight2007  HistoricalConflictGrid d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area lpopdens cdist1_10 cdist1_25 cdist1_50 cdist1_75 cdist1_90 grid_cap  i.country, cluster(country)
xi: regress llightnight2007 HistoricalConflictGrid lpopdens d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08 area lpopdens JurisHierac i.country, cluster(country)
xi: regress llightnight2007 HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area   lpopdens llightnight1992 sharemin i.economynum   i.country, cluster(country)
test _Ieconomynu_1  _Ieconomynu_2  _Ieconomynu_3  _Ieconomynu_4 _Ieconomynu_5   _Ieconomynu_6  _Ieconomynu_7  _Ieconomynu_8 _Ieconomynu_9  _Ieconomynu_10  _Ieconomynu_11  _Ieconomynu_12 _Ieconomynu_13   _Ieconomynu_14  _Ieconomynu_15  _Ieconomynu_16 _Ieconomynu_17  _Ieconomynu_18  _Ieconomynu_19  _Ieconomynu_20 _Ieconomynu_21   _Ieconomynu_22  _Ieconomynu_23  _Ieconomynu_24 _Ieconomynu_25  _Ieconomynu_26  _Ieconomynu_27 
xi: regress llightnight2007 HistoricalConflictGrid  d1 elev_srtm_pred rough  tempav_8008  precsdnew_80_08  area  lpopdens gridcity1400  ethnicdivdum logslave_obs  grid_cap cdist1_10 cdist1_25 cdist1_50 cdist1_75 cdist1_90   JurisHierac   llightnight1992 sharemin i.economynum   zulu_king merina monomotapa lozi malawi kilwa lunda congo luba rwanda buganda ashanti yoruba ethiopia axum wolof ghana mali kush songhay kanem_born clas_egypt carthage     i.country, cluster(country)

 
