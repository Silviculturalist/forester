#' Dry weight of needles for Spruce, from Marklund 1988.
#'
#' @param diameter.cm Diameter of tree at breast height, cm.
#' @param height.m Height of tree, metres.
#' @param crown_base_height.m Height to the lowest living green branch not
#' separated by more than three whorls from the crown.
#' @param crown_radius.m Maximum crown radius, in metres.
#' @param age_at_breast_height Age at breast height.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#' @param Dmax_10_m_radius Diameter above bark of the thickest tree on a 10 m radius plot.
#'
#' @return Dry weight of needles for a Spruce, kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_needles_Spruce <- function(
  diameter.cm,
  height.m,
  crown_base_height.m,
  crown_radius.m,
  age_at_breast_height,
  five_years_diameter_increment.mm,
  Dmax_10_m_radius
){

  #G15
  try(
  dry_weight_of_needles <-
    -1.9602+
    +7.8171*(diameter.cm/(diameter.cm+12))
  )

  #G16
  try(
  dry_weight_of_needles <-
    -1.8551+
    +9.7809*(diameter.cm/(diameter.cm+12))+
    -0.4873*log(height.m)
  )

  #G17
  try(
  dry_weight_of_needles <-
    -1.5732+
    +8.4127*(diameter.cm/(diameter.cm+12))+
    -1.5628*log(height.m)+
    +1.4032*log(height.m-crown_base_height.m)
  )

  #G18
  try(
  dry_weight_of_needles <-
    -2.6982+
    +6.6949*(diameter.cm/(diameter.cm+12))+
    -0.8733*log(height.m)+
    +0.7249*log(height.m-crown_base_height.m)+
    +0.2066*log(crown_radius.m)+
    +0.2820*log(age_at_breast_height)+
    +0.4526*log(five_years_diameter_increment.mm)+
    -0.1467*log(Dmax_10_m_radius)
  )

  return(exp(dry_weight_of_needles))
}
