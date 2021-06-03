#' Dry weight of dead branches from a Pine from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in metres.
#' @param latitude Latitude.
#' @param altitude Altitude, in km.
#' @param age_at_breast_height  Age at breast height.
#' @param five_years_diameter_increment.mm Increment from the last five years at
#' breast height, measured in mm.
#' @param Dmax_10_m_radius Diameter above bark of the thickest tree on a 10 m radius plot.
#'
#' @return Dry weight of dead branches in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_dead_branches_Pine <- function(
  diameter.cm,
  height.m,
  latitude,
  altitude,
  age_at_breast_height,
  five_years_diameter_increment.mm,
  Dmax_10_m_radius
){
  #T21
try(
  dry_weight_dead_branches <-
  -5.3338+
  +9.5938*(diameter.cm/(diameter.cm+10))
)

#T22
try(
  dry_weight_dead_branches <-
  -5.8926+
  +7.1270*(diameter.cm/(diameter.cm+10))+
  -0.0465*height.m+
  +1.1060*log(height.m)
)

#T23
try(
  dry_weight_dead_branches <-
  -0.9305+
  +7.1889*(diameter.cm/(diameter.cm+10))+
  -0.0850*height.m+
  +1.3027*log(height.m)+
  -0.0702*latitude+
  -1.0568*altitude
)

#T24
try(
  dry_weight_dead_branches <-
  -0.8931+
  +10.3377*(diameter.cm/(diameter.cm+10))+
  -0.0865*height.m+
  +0.8701*log(height.m)+
  -0.6209*log(age_at_breast_height)+
  -0.5100*log(five_years_diameter_increment.mm)+
  +0.5846*Dmax_10_m_radius+
  -0.0577*latitude+
  -1.1226*altitude
)

  return(exp(dry_weight_dead_branches))
}
