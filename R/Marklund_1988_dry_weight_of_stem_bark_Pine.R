#' Dry weight of stem bark for Pine from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, centimetres.
#' @param height.m Height of tree, metres.
#' @param age_at_breast_height Age of tree at breast height.
#' @param double_bark.mm Double thickness of bark, in mm.
#' @param form_quotient5 Diameter at 5 m / Diameter at breast height.
#' @param form_quotient3 Diameter at 3 m / Diameter at breast height.
#' @param altitude Altitude, in km.
#'
#' @return Dry weight of Pine stem above bark in kilograms.
#' @export
Marklund_1988_dry_weight_of_stem_bark_Pine <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  double_bark.mm,
  form_quotient5,
  form_quotient3,
  altitude
){

#T9
  try(
dry_weight_of_stem_bark <-
  -2.9748+
  +8.8489*(diameter.cm/(diameter.cm+16))
)

#T10
  try(
dry_weight_of_stem_bark <-
  -3.2765+
  +7.2482*(diameter.cm/(diameter.cm+16))+
  +0.4487*log(height.m)
)

#T11
  try(
dry_weight_of_stem_bark <-
  -3.6065+
  +7.0834*(diameter.cm/(diameter.cm+16))+
  +0.5086*log(height.m)+
  +0.0255*relative_bark_thickness
)

#T12
  try(
dry_weight_of_stem_bark <-
  -3.5076+
  +7.5295*(diameter.cm/(diameter.cm+16))+
  +0.5629*log(height.m)+
  -0.2271*log(height.m-crown_base_height.m)+
  +0.0222*relative_bark_thickness
)

  return(exp(dry_weight_of_stem_bark))

}
