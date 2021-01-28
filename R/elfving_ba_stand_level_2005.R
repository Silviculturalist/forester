
#' Stand Basal Area Development Elfving 2005
#'
#' @description Description available: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf} page 50.
#' @param vegetation Variable indicating vegetation type scaled from -5 to +4 as follows:
#'
#' \tabular{llr}{
#' Field Layer Code (NFI) \tab Description \tab Index \cr
#' 1\tab Tall herbs w/o dwarf shrubs \tab 4 \cr
#' 2\tab Tall herbs with bilberry \tab 2.5 \cr
#' 3\tab Tall herbs with cowberry \tab 2 \cr
#' 4\tab Low herbs w/o dwarf shrubs \tab 3 \cr
#' 5\tab Low herbs with bilberry \tab 2.5 \cr
#' 6\tab Low herbs with cowberry \tab 2 \cr
#' 7\tab No field layer \tab 3 \cr
#' 8\tab broad-leafed grasses \tab 2.5 \cr
#' 9\tab narrow-leaved grasses \tab 1.5 \cr
#' 10\tab Sedge, tall \tab -3 \cr
#' 11\tab Sedge, low \tab -3 \cr
#' 12\tab Horsetail \tab 1 \cr
#' 13\tab Bilberry \tab 0 \cr
#' 14\tab Cowberry \tab -0.5 \cr
#' 15\tab Crowberry \tab -3 \cr
#' 16\tab Poor shrub \tab -5 \cr
#' 17\tab Lichen-rich \tab -0.5 \cr
#' 18\tab Lichen-dominated \tab -1 \cr
#' }
#' @param age Age at start of period
#' @param conifer_ba Conifer basal area
#' @param pine_ba Pine basal area
#' @param birch_ba Birch basal area
#' @param ba_followed_trees basal area per ha for followed trees
#' @param ba_total total basal area per ha at start of period
#' @param stem_number_followed stem number per ha for followed trees
#' @param peat_vegetation True or False; if there is peat on the plot.
#' @param soil_moisture Soil moisture code (Dry/mesic/mesic-moist/moist,wet)
#' @param sis site index according to site factors for site-indicative species, mean between measurement 1 and 2.
#' @param latitude degrees latitude
#' @param altitude metres above sea level
#' @param ditch_within_25 True or False; if there is a ditch within 25 m of the plot centre.
#' @param edge_to_open_area_within_20 True or False; if there is a stand edge towards and open area within 20 m of the plot centre.
#' @param divided_plot True or False; if the plot is divided.
#' @param years_since_last_thinned Years since the plot was last thinned. If never, 0
#' @param ba_surrounding_stand Basal area of the surrounding stand
#' @param establishment_year Year established.
#'
#' @return
#' @export
elfving_ba_stand_level_2005 <- function(vegetation, age, conifer_ba, pine_ba, birch_ba, ba_followed_trees, ba_total, stem_number_followed, peat_vegetation, soil_moisture, sis, latitude, altitude, ditch_within_25, edge_to_open_area_within_20, divided_plot, years_since_last_thinned, ba_surrounding_stand, establishment_year){

  #development of intermediary variables.
  conifer_ba_proportion <-  (conifer_ba / ba_total) / age
  pine_ba_proportion <- (pine_ba / ba_total) / age
  birch_ba_proportion <- (birch_ba / ba_total) / age
  lngrel <- log((ba_total/ba_surrounding_stand))

  if(peat_vegetation==TRUE){
    peat_vegetation <-  vegetation
  } else {
    0
  }

  if(soil_moisture=="Moist"){
    moist <- 1
    wet <- 0
  } else if(soil_moisture=="Wet"){
    moist <- 0
    wet <- 1
  } else {
    moist <- 0
    wet <- 1
  }

  cold_climate <- exp(-0.01 * ((4835 - 57.6*latitude - 0.9*altitude)-300))

  #fertris value between 0-1 from Elfving, B. (2001) on fertilised sites with vegetation < 12
  fertris

  if(years_since_last_thinned==0){
    hu0t10 <- 0
    hu10t25 <- 0
  }  else if(years_since_last_thinned<=10){
    hu0t10 <- 1
    hu10t25 <- 0
  } else if(years_since_last_thinned>10 & years_since_last_thinned<=25){
    hu0t10 <- 0
    hu10t25 <- 1
  }

  stem_number_followed_quote <- stem_number_followed/(stem_number_followed+80)

  return(
    exp(0.366 + -0.5842*(log(age)) + 8.3740*conifer_ba_proportion + -0.0237*pine_ba_proportion*vegetation + -0.3192*(birch_ba_proportion^2) + -10.8034*(cold_climate)*(birch_ba_proportion) + 0.5002*log(ba_followed_trees) + -0.00632*ba_total + 1.3760*(stem_number_followed_quote) + 0.0627*vegetation + -0.0244*peat_vegetation + -0.0498*moist + -0.1807*wet + 0.0109*sis + 0.0542*ditch_within_25 + 0.1396*hu0t10 + 0.0567*hu10t25 + -0.06*pine_ba_proportion + -0.03*spruce_ba_proportion)
  )


}
