#' Dry weight of roots above 5 cm for Spruce, from Marklund 1988
#'
#' @param diameter.cm Diameter at breast height, cm.
#' @param dominant_species "Pinus sylvestris" or "Picea abies";
#' Species for which SI was estimated.
#' @param SI Site index, m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param Moist_soil If the ground water level is less than 1 m deep, TRUE,
#' otherwise FALSE.
#' @param peat If the humus layer is thicker than 30 cm, TRUE, otherwise
#' FALSE.
#' @param mobile_ground_water_longer_periods If surface/subsurface water flow
#' occurs during longer periods, TRUE, otherwise FALSE.
#'
#' @return dry weight of roots above 5 cm, kilograms.
#' @export
Marklund_1988_dry_weight_of_roots_above_5_Spruce <- function(
  diameter.cm,
  dominant_species,
  SI,
  age_at_breast_height,
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

  #G28
  try(
  dry_weight_of_roots_above_5_G28 <-
    -6.3851+
    +13.3703*(diameter.cm/(diameter.cm+8))
  )

  #G29
  try(
  dry_weight_of_roots_above_5_G29 <-
    -6.0559+
    +13.6140*(diameter.cm/(diameter.cm+8))+
    -0.0204*SI*pinus+
    -0.0211*SI*picea
  )

  #G30
  try(
  dry_weight_of_roots_above_5_G30 <-
    -5.9948+
    +12.5949*(diameter.cm/(diameter.cm+8))+
    +0.3864*log(height.m)+
    -0.1114*log(age_at_breast_height)+
    -0.0215*SI*pinus+
    -0.0246*SI*picea+
    +0.3267*Moist_soil+
    +0.4094*peat+
    -0.4444*mobile_ground_water_longer_periods
  )

  return(exp(dry_weight_of_roots_above_5))
}
