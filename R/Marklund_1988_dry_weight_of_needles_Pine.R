#' Dry weight of needles in a Pine, from Marklund (1988)
#'
#' @param diameter.cm  Diameter at breast height, in cm.
#' @param height.m Height of tree, in m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param crown_radius.m Maximum crown radius, in metres.
#' @param latitude Latitude.
#' @param altitude Altitude, in km.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#'
#' @return Dry weight of needles in kilograms.
#' @export
Marklund_1988_dry_weight_of_needles_Pine <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  crown_base_height.m,
  crown_radius.m,
  latitude,
  altitude,
  five_years_diameter_increment.mm
){

  #T17
  try(
  dry_weight_needles <-
  -3.7983+
  +7.7681*(diameter.cm/(diameter.cm+7))
  )

#T18
try(
  dry_weight_needles <-
  -3.4781+
  +12.1095*(diameter.cm/(diameter.cm+7))+
  +0.0413*height.m+
  -1.5650*log(height.m)
)

#T19

try(
  dry_weight_needles <-
  -2.6024+
  +9.8471*(diameter.cm/(diameter.cm+7))+
  +0.0260*height.m+
  -1.6717*log(height.m)+
  +1.0419*log(height.m-crown_base_height.m)+
  -0.0123*latitude
  )

#T20
  try(
dry_weight_needles <-
  -4.6082+
  +7.7998*(diameter.cm/(diameter.cm+7))+
  -0.6978*log(height.m)+
  +0.4588*log(height.m-crown_base_height.m)+
  +0.2398*log(crown_radius.m)+
  +0.2632*log(age_at_breast_height)+
  +0.4040*log(five_years_diameter_increment.mm)+
  +0.5144*altitude
)

  return(exp(dry_weight_needles))
}
