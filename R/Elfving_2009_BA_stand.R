#' Stand Basal Area Development Elfving 2009
#'
#' @description This relates to the function given in text in the source: the calibrated function BEY2 suggested for application in Heureka.
#' @source \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf} page 50 in text.
#' @param vegetation Vegetation type according to follows Swedish National forest inventory FALTSKIKT:
#' \tabular{ll}{
#' Code \tab Vegetation \cr
#' 1 \tab  Rich-herb without shrubs \cr
#' 2 \tab Rich-herb with shrubs/bilberry \cr
#' 3 \tab Rich-herb with shrubs/lingonberry \cr
#' 4 \tab Low-herb without shrubs \cr
#' 5 \tab Low-herb with shrubs/bilberry \cr
#' 6 \tab Low-herb with shrubs/lingonberry \cr
#' 7 \tab No field layer \cr
#' 8 \tab Broadleaved grass \cr
#' 9 \tab Thinleaved grass \cr
#' 10 \tab Sedge, high \cr
#' 11 \tab Sedge, low \cr
#' 12 \tab Horsetail, Equisetum ssp. \cr
#' 13 \tab Bilberry \cr
#' 14 \tab Lingonberry \cr
#' 15 \tab Crowberry \cr
#' 16 \tab Poor shrub \cr
#' 17 \tab Lichen, frequent occurrence \cr
#' 18 \tab Lichen, dominating \cr
#' }
#' @param stand_age Age at start of period
#' @param Basal_area_Conifer_m2_ha Conifer basal area
#' @param Basal_area_Pine_m2_ha Pine basal area
#' @param Basal_area_Birch_m2_ha Birch basal area
#' @param Basal_area_after_thinning Basal area m2 per ha after thinning.
#' @param Basal_area_before_thinning Basal area m2 per ha before thinning.
#' @param Basal_area_stand Basal area in the surrounding stand, m2/ha.
#' @param stems_after_thinning Stems per hectare after thinning.
#' @param peatland 1 if plot is Peatland, 0 for others (default).
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param SIS100 Site Index as measured from [forester::SIS_estimate()]
#' @param latitude degrees latitude
#' @param altitude metres above sea level
#' @param ditched True or False; if there is a ditch within 25 m of the plot centre.
#' @param thinned 1 if the plot has been thinned, otherwise 0.
#' @param last_thinned Number of growth seasons since the stand was last thinned.
#'
#' @return
#' @export
Elfving_2009_BA_stand <- function(vegetation, stand_age, Basal_area_Conifer_m2_ha, Basal_area_Pine_m2_ha, Basal_area_Birch_m2_ha, Basal_area_after_thinning, Basal_area_before_thinning, Basal_area_stand, stems_after_thinning, peatland, soil_moisture, SIS100, latitude, altitude, ditched, thinned, last_thinned){

  #A few simple parameter checks.
  stopifnot(vegetation%in% 1:18)
  stopifnot(thinned%in%c(TRUE,FALSE,1,0))
  stopifnot(ditched%in%c(TRUE,FALSE,1,0))
  stopifnot(peatland%in%c(TRUE,FALSE,1,0))
  stopifnot(soil_moisture%in% 1:5)

  #vegetation scaling verified.
  vegetation <- ifelse(vegetation==1,4,ifelse(vegetation==2,2.5,ifelse(vegetation==3,2,ifelse(vegetation==4,3,ifelse(vegetation==5,2.5,ifelse(vegetation==6,2,ifelse(vegetation==7,3,ifelse(vegetation==8,2.5,ifelse(vegetation==9,1.5,ifelse(vegetation==10,-3,ifelse(vegetation==11,-3,ifelse(vegetation==12,1,ifelse(vegetation==13,0,ifelse(vegetation==14,-0.5,ifelse(vegetation==15,-3,ifelse(vegetation==16,-5,ifelse(vegetation==17,-0.5,ifelse(vegetation==18,-1, stop("vegetation must be integer 1-18")))))))))))))))))))

  #Proportions assumed to be to basal area after thinning..
  conifer_ba_proportion <-  (Basal_area_Conifer_m2_ha / Basal_area_after_thinning) / stand_age
  pine_ba_proportion <- (Basal_area_Pine_m2_ha / Basal_area_after_thinning)
  birch_ba_proportion <- (Basal_area_Birch_m2_ha / Basal_area_after_thinning)
  lngrel <- log((Basal_area_before_thinning/Basal_area_stand))


  peat_vegetation <- ifelse(peatland,vegetation,0)

  moist <- ifelse(soil_moisture==4,1,0)
  wet <- ifelse(soil_moisture==5,1,0)

  cold_climate <- exp(-0.01 * ((4835 - 57.6*latitude - 0.9*altitude)-300))

  thinned_recently <- ifelse(thinned & last_thinned<=10,1,0)
  thinned_long_ago <- ifelse(thinned & last_thinned>10 & last_thinned<=25,1,0)

  stem_number_followed_quote <- stems_after_thinning/(stems_after_thinning+80)

  return(
    exp(
      +0.366+
      -0.5842*(log(stand_age))+
      +8.3740*conifer_ba_proportion+
      -0.0237*pine_ba_proportion*vegetation+
      -0.3192*(birch_ba_proportion^2)+
      -10.8034*(cold_climate)*(birch_ba_proportion)+
      +0.5002*log(Basal_area_after_thinning)+
      -0.00632*Basal_area_before_thinning+
      +1.376*(stem_number_followed_quote)+
      +0.0627*vegetation+
      -0.0244*peat_vegetation+
      -0.0498*moist+
      -0.1807*wet+
      +0.0109*SIS100+
      +0.0542*ditched+
      +0.1396*thinned_recently+
      +0.0567*thinned_long_ago+
      -0.06*pine_ba_proportion+
      -0.03*spruce_ba_proportion
      )
  )


}
