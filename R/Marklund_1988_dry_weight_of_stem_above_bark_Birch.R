#' Dry weight of stem above bark for Birch, from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in metres.
#' @param age_at_breast_height Age at breast height.
#'
#' @return Dry weight of stem above bark, in kilograms.
#' @export
Marklund_1988_dry_weight_of_stem_above_bark_Birch <- function(
  diameter.cm,
  height.m,
  age_at_breast_height
){

  #B1
  try(
  dry_weight_of_stem_above_bark <-
    -3.0932+
    +11.0735*(diameter.cm/(diameter.cm+8))
  )

  #B2
  try(
  dry_weight_of_stem_above_bark <-
    -3.5686+
    +8.2827*(diameter.cm/(diameter.cm+7))+
    +0.0393*height.m+
    +0.5772*log(height.m)
  )

  #B3
  try(
  dry_weight_of_stem_above_bark <-
    -3.5194+
    +8.0420*(diameter.cm/(diameter.cm+7))+
    +0.0531*height.m+
    +0.3897*log(height.m)+
    +0.1018*log(age_at_breast_height)
  )

  return(exp(dry_weight_of_stem_above_bark))

}
