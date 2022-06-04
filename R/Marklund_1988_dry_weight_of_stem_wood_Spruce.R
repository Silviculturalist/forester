#' Dry weight of stem wood for Spruce from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param form_quotient5 Diameter at 5 m / Diameter at breast height.
#' @param form_quotient3 Diameter at 3 m / Diameter at breast height.
#'
#' @return Dry weight of stem wood for Spruce, in kilograms.
#' @export

Marklund_1988_dry_weight_of_stem_wood_Spruce <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  form_quotient5,
  form_quotient3
){

  #G4
  try(
  dry_weight_of_stem_wood <-
    -2.2471+
    +11.4873*(diameter.cm/(diameter.cm+14))
  )

  #G5
  try(
  dry_weight_of_stem_wood <-
    -2.3032+
    +7.2309*(diameter.cm/(diameter.cm+14))+
    +0.0355*height.m+
    +0.7030*log(height.m)
  )

  #G6
  try(
  dry_weight_of_stem_wood <-
    -2.2029+
    +7.0615*(diameter.cm/(diameter.cm+14))+
    +0.0448*height.m+
    +0.4522*log(height.m)+
    +0.0727*log(age_at_breast_height)+
    +0.3154*form_quotient5+
    +0.1467*form_quotient3
  )

  return(exp(dry_weight_of_stem_wood))

}
