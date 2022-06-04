#' Dry weight of Stem wood for Pine according to Marklund (1988)
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
Marklund_1988_dry_weight_of_stem_wood_Pine <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  double_bark.mm,
  form_quotient5,
  form_quotient3,
  altitude
                                                       ){

  #T5
  try(
  dry_weight_of_stem_wood <-
    -2.2184+
    11.4219*(diameter.cm/(diameter.cm+14))
  )

  #T6
  try(
  dry_weight_of_stem_wood <-
    -2.6864+
    +7.6066*(diameter.cm/(diameter.cm+14))+
    +0.0200*height.m+
    +0.8658*log(height.m)
  )

  #T7
  try(
    dry_weight_of_stem_wood <-
    -2.5325+
    +7.8936*(diameter.cm/(diameter.cm+14))+
    +0.0231*height.m+
    +0.7887*log(height.m)+
    -0.1065*log(double_bark.mm)+
    +0.00201*age_at_breast_height
  )

  #T8

  try(
    dry_weight_of_stem_wood <-
    -2.0028+
    +7.9455*(diameter.cm/(diameter.cm+14))+
    +0.0439*height.m+
    +0.2437*log(height.m)+
    -0.0875*log(double_bark.mm)+
    +0.00172*age_at_breast_height+
    +0.7778*form_quotient5+
    +0.4855*form_quotient3+
    -0.1557*altitude
  )

  return(exp(dry_weight_of_stem_wood))
}
