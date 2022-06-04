#' Dry weight of living branches for Birch from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height in cm.
#' @param height.m Height of tree in metres.
#' @param latitude Latitude, RT90.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#'
#' @return Dry weight of living branches, in kilograms.
#' @export
Marklund_1988_dry_weight_of_living_branches_Birch <- function(
  diameter.cm,
  height.m,
  latitude,
  crown_base_height.m,
  five_years_diameter_increment.mm
){

  #B11
  try(
  dry_weight_of_living_branches <-
    -3.3633+
    +10.2806*(diameter.cm/(diameter.cm+10))
  )

  #B12
  try(
  dry_weight_of_living_branches <-
    +0.0432+
    +12.7821*(diameter.cm/(diameter.cm+10))+
    -0.8525*log(height.m)+
    -0.0409*latitude
  )

  #B13
  try(
  dry_weight_of_living_branches <-
    +0.0282+
    +10.7485*(diameter.cm/(diameter.cm+10))+
    -1.2066*log(height.m)+
    +1.0409*log(height.m-crown_base_height.m)+
    -0.0415*latitude
  )

  #B14
  try(
  dry_weight_of_living_branches <-
    -0.3916+
    +8.0492*(diameter.cm/(diameter.cm+10))+
    -1.1407*log(height.m)+
    +0.7207*log(height.m-crown_base_height.m)+
    +0.9133*log(crown_radius.m)+
    +0.1702*log(age_at_breast_height)+
    +0.1747*log(five_years_diameter_increment.mm)+
    -0.0320*latitude
  )

  return(exp(dry_weight_of_living_branches))
}
