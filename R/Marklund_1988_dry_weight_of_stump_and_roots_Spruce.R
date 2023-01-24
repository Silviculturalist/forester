#' Dry weight of stump and roots for Spruce, from Marklund 1988
#'
#' @param diameter.cm Diameter of tree at breast height, cm.
#' @param dominant_species "Pinus sylvestris" or "Picea abies";
#' Species for which SI was estimated.
#' @param SI Site index, m.
#' @param Moist_soil If the ground water level is less than 1 m deep, TRUE,
#' otherwise FALSE.
#' @param peat If the humus layer is thicker than 30 cm, TRUE, otherwise
#' FALSE.
#' @param mobile_ground_water_longer_periods If surface/subsurface water flow
#' occurs during longer periods, TRUE, otherwise FALSE.
#'
#' @return Dry weight of stump and roots for Spruce from Marklund 1988
#' @export
Marklund_1988_dry_weight_of_stump_and_roots_Spruce <- function(
  diameter.cm,
  dominant_species,
  SI,
  Moist_soil,
  peat,
  mobile_ground_water_longer_periods
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }

  #G23
  try(
  dry_weight_of_stump_and_roots <-
    -2.4447+
    +10.5381*(diameter.cm/(diameter.cm+14))
  )

  #G24
  try(
  dry_weight_of_stump_and_roots <-
    -2.0810+
    +10.6680*(diameter.cm/(diameter.cm+14))+
    -0.0162*SI*pinus+
    -0.0190*SI*picea
  )

  #G25
  try(
  dry_weight_of_stump_and_roots <-
    -2.2616+
    +10.6277*(diameter.cm/(diameter.cm+14))+
    -0.0102*SI*pinus+
    -0.0144*SI*picea+
    +0.2237*Moist_soil+
    +0.2693*peat+
    -0.1919*mobile_ground_water_longer_periods
  )

  return(exp(dry_weight_of_stump_and_roots))

}
