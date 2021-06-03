#' Dry weight of living branches for Pine according to Marklund (1988)
#'
#' @param diameter.cm Diameter in cm.
#' @param height.m Height of tree in meters.
#' @param age_at_breast_height Age at breast height.
#' @param form_quotient5 Diameter at 5 m / Diameter at breast height.
#' @param form_quotient3 Diameter at 3 m / Diameter at breast height.
#' @param altitude Altitude, in km.
#' @param crown_radius.m Maximum crown radius, in metres.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#' @param latitude Latitude.
#'
#' @return Dry weight of living branches for a Pine tree, in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_living_branches_Pine<- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  form_quotient5,
  form_quotient3,
  altitude,
  crown_radius.m,
  crown_base_height.m,
  five_years_diameter_increment.mm,
  latitude
){
  #T13
  try(
dry_weight_living_branches <-
  -2.8604+
  +9.1015*(diameter.cm/(diameter.cm+10))
)

#T14
try(
  dry_weight_living_branches <-
  -2.5413+
  +13.3955*(diameter.cm/(diameter.cm+10))+
  -1.1955*log(height.m)
)

#T15
try(
  dry_weight_living_branches <-
  -0.9137+
  +11.4337*(diameter.cm/(diameter.cm+10))+
  -1.4815*log(height.m)+
  +0.9825*log(height.m-crown_base_height.m)+
  -0.0235*latitude
  )

#T16
  try(
dry_weight_living_branches <-
  -2.8445+
  +9.0891*(diameter.cm/(diameter.cm+10))+
  -1.1599*log(height.m)+
  +0.6197*log(height.m-crown_base_height.m)+
  +0.5372*log(crown_radius.m)+
  +0.2011*log(age_at_breast_height)+
  +0.2142*log(five_years_diameter_increment.mm)
)

  return(exp(dry_weight_living_branches))
}
