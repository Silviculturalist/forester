#' Dry weight of roots above 5 cm diameter for Pine, from Marklund (1988)
#'
#' @param diameter.cm Diameter of tree at breast height.
#' @param SI Site index of dominant species (Picea abies or Pinus sylvestris)
#' @param dominant_species "Picea abies" or "Pinus sylvestris".
#' @param Dry_soil If the ground water level is more than 2 m deep, TRUE,
#' otherwise FALSE.
#' @param Moist_soil If the ground water level is less than 1 m deep, TRUE,
#' otherwise FALSE.
#' @param peat If the humus layer is thicker than 30 cm, TRUE, otherwise
#' FALSE.
#' @param altitude Altitude, km.
#'
#' @return Dry weight of roots above 5 cm diameter, in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_roots_above_5 <- function(
  diameter.cm,
  SI,
  dominant_species,
  Dry_soil,
  Moist_soil,
  peat,
  altitude
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }


  #T31
  try(
  dry_weight_of_roots_above_5 <-
    -6.3413+
    +13.2902*(diameter.cm/(diameter.cm+9))
  )

  #T32
  try(
    dry_weight_of_roots_above_5 <-
    -3.5882+
    +13.6524*(diameter.cm/(diameter.cm+9))+
    -0.0467*SI*pinus+
    -0.0448*SI*picea+
    -0.0306*latitude
  )


  #T33
  try(
  dry_weight_of_roots_above_5 <-
    -5.9660+
    +13.7465*(diameter.cm/(diameter.cm+9))+
    -0.0352*SI*pinus+
    -0.0356*SI*picea+
    -0.1443*Dry_soil+
    +0.3052*Moist_soil+
    +0.5078*peat+
    -0.6359*altitude
  )

  return(exp(dry_weight_of_roots_above_5))


}
