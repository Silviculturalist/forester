#' Dry weight of roots below 5 cm diameter for Pine, from Marklund (1988)
#'
#' @param diameter.cm Diameter of the tree at breast height, cm.
#' @param dominant_species Dominant species, for which SI has been estimated.
#' @param SI Site index of the site.
#' @param altitude Altitude, km.
#'
#' @return Dry weight of roots below 5 cm diameter for Pine, in kilograms.
#' @export
#'
#' @examples

Marklund_1988_dry_weight_of_roots_below_5<- function(
  diameter.cm,
  dominant_species,
  SI,
  altitude
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }

  #T34
  try(
  dry_weight_of_roots_below_5 <-
    -3.8375+
    +8.8795*(diameter.cm/(diameter.cm+10))
  )

  #T35
  try(
  dry_weight_of_roots_below_5 <-
    -3.5912+
    +8.9776*(diameter.cm/(diameter.cm+10))+
    -0.0162*SI*pinus+
    -0.0123*SI*picea
  )

  #T36
  try(
  dry_weight_of_roots_below_5 <-
    -3.3979+
    +8.9668*(diameter.cm/(diameter.cm+10))+
    -0.0204*SI*pinus+
    -0.0168*SI*picea+
    -0.4501*altitude
  )

  return(exp(dry_weight_of_roots_below_5))
}
