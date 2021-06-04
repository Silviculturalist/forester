#' Dry weight of stem bark for Birch from Marklund 1988
#'
#' @param diameter.cm Diameter of tree at breast height, in cm-
#' @param height.m Height of tree in m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param double_bark.mm Double bark thickness, mm.
#' @param latitude Latitude, RT90.
#' @param five_years_diameter_increment Increment from the last five years at
#' breast height, measured in mm.
#' @param Dmax_10_m_radius Diameter of thickest tree on a plot with a 10 metre radius.
#'
#'
#' @return Dry weight of stem bark for Birch, in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_stem_bark_Birch <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  double_bark.mm,
  latitude,
  five_years_diameter_increment,
  Dmax_10_m_radius
){

  #B7
  try(
  dry_weight_of_stem_bark <-
    -3.2518+
    +10.3876*(diameter.cm/(diameter.cm+14))
  )

  #B8
  try(
  dry_weight_of_stem_bark <-
    -4.0778+
    +8.3019*(diameter.cm/(diameter.cm+14))+
    +0.7433*log(height.m)
  )

  #B9
  try(
  dry_weight_of_stem_bark <-
    -3.6430+
    +6.9285*(diameter.cm/(diameter.cm+14))+
    +0.5898*log(height.m)+
    +0.2772*log(age_at_breast_height)+
    +0.2038*log(double_bark.mm)+
    -0.0137*latitude
  )

  #B10
  try(
  dry_weight_of_stem_bark <-
    -2.3569+
    +7.4965*(diameter.cm/(diameter.cm+14))+
    +0.5947*log(height.m)+
    +0.1820*log(double_bark.mm)+
    +0.1972*log(age_at_breast_height)+
    -0.1185*log(five_years_diameter_increment.mm)+
    -0.1974*log(Dmax_10_m_radius)+
    -0.0182*latitude
  )

  return(exp(dry_weight_of_stem_bark))
}
