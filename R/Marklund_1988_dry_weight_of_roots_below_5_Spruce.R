#' Dry weight of roots below 5 cm diameter for Spruce from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, cm.
#'
#' @param dominant_species "Pinus sylvestris" or "Picea abies";
#' Species for which SI was estimated.
#'
#' @param SI Site index, m.
#' @param Moist_soil If the ground water level is less than 1 m deep, TRUE,
#' otherwise FALSE.
#' @param peat If the humus layer is thicker than 30 cm, TRUE, otherwise
#' FALSE.
#'
#' @return Dry weight of roots below 5 cm, in kilograms.
#' @export
Marklund_1988_dry_weight_of_roots_below_5_Spruce <- function(
  diameter.cm,
  dominant_species,
  SI,
  Moist_soil,
  peat
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }


  #G31
  try(
  dry_weight_of_roots_below_5 <-
    -2.5706+
    +7.6283*(diameter.cm/(diameter.cm+12))
  )

  #G32
  try(
  dry_weight_of_roots_below_5 <-
    -2.3177+
    +7.7441*(diameter.cm/(diameter.cm+12))+
    -0.0105*SI*pinus+
    -0.0148*SI*picea
  )


  #G33
  try(
  dry_weight_of_roots_below_5 <-
    -2.4676+
    +7.7375*(diameter.cm/(diameter.cm+12))+
    -0.00675*SI*pinus+
    -0.0117*SI*picea+
    +0.1777*Moist_soil+
    +0.2461*peat
  )

  return(exp(dry_weight_of_roots_below_5))
}
