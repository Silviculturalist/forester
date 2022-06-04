#' Dry weight of stem above bark for Spruce according to Marklund (1988)
#'
#' @param diameter.cm Diameter of tree at breast height, cm.
#' @param height.m Tree height, m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param form_quotient5 Diameter measured at 5 m / Diameter measured at
#' breast height.
#' @param form_quotient3 Diameter measured at 3 m / Diameter measured at
#' breast height.
#'
#' @return Dry weight of stem above bark, in kilograms.
#' @export
Marklund_1988_dry_weight_of_stem_above_bark_Spruce <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  form_quotient5,
  form_quotient3
  ){

  #G1
  try(
  dry_weight_of_stem_above_bark <-
    -2.0571+
    +11.3341*(diameter.cm/(diameter.cm+14))
  )

  #G2
  try(
  dry_weight_of_stem_above_bark <-
    -2.1702+
    +7.4690*(diameter.cm/(diameter.cm+14))+
    +0.0289*height.m+
    +0.6828*log(height.m)
  )

  #G3

  try(
  dry_weight_of_stem_above_bark <-
    -2.1781+
    +7.2601*(diameter.cm/(diameter.cm+14))+
    +0.0371*height.m+
    +0.4803*log(height.m)+
    +0.0934*log(age_at_breast_height)+
    +0.2239*form_quotient5+
    +0.1265*form_quotient3
  )

  return(exp(dry_weight_of_stem_above_bark))
}
