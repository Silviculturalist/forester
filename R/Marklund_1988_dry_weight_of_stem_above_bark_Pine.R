#' Find the dry weight of a Pine stem above bark according to Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, centimetres.
#' @param height.m Height of tree, metres.
#' @param age_at_breast_height Age of tree at breast height.
#' @param double_bark.mm Double thickness of bark, in mm.
#' @param form_quotient5 Diameter at 5 m / Diameter at breast height.
#' @param form_quotient3 Diameter at 3 m / Diameter at breast height.
#' @param altitude Altitude, in km.
#'
#' @return Dry weight of Pine stem above bark in grams.
#' @export
Marklund_1988_dry_weight_of_stem_above_bark_Pine <- function(
diameter.cm,
height.m,
double_bark.mm,
age_at_breast_height,
form_quotient5,
form_quotient3,
altitude
){

  #T1
  try(
    dry_weight_of_stem_above_bark <-
    -2.3388+
    +11.3264*(diameter.cm/(diameter.cm+13))
  )

  #T2
  try(
    dry_weight_of_stem_above_bark <-
    -2.6768+
    +7.5939*(diameter.cm/(diameter.cm+13))+
    +0.0151*height.m+
    +0.8799*log(height.m)
  )

  #T3
  try(
  dry_weight_of_stem_above_bark <-
    -2.6232+
    +7.7318*(diameter.cm/(diameter.cm+13))+
    +0.0139*height.m+
    +0.8625*log(height.m)+
    -0.0704*log(double_bark.mm)+
    +0.00185*age_at_breast_height
  )

  #T4
  try(
  dry_weight_of_stem_above_bark <-
    -2.4826+
    +7.9039*(diameter.cm/(diameter.cm+13))+
    +0.0184*height.m+
    +0.6939*log(height.m)+
    -0.0731*log(double_bark.mm)+
    +0.00182*age_at_breast_height+
    +0.2382*form_quotient5+
    +0.2217*form_quotient3+
    -0.1596*altitude
  )

  return(exp(dry_weight_of_stem_above_bark))

}
