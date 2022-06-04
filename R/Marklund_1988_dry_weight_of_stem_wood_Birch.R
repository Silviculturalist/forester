#' Dry weight of stem wood for Birch from Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height in cm
#' @param height.m Height of tree, in m.
#' @param age_at_breast_height Age at breast height.
#' @param double_bark.mm Double bark thickness, mm.
#' @param latitude Latitude, RT90.
#'
#' @return Dry weight of stem wood, in kilograms.
#' @export
Marklund_1988_dry_weight_of_stem_wood_Birch <- function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  double_bark.mm,
  latitude
){

  #B4
  try(
  dry_weight_of_stem_wood <-
    -2.3327+
    +10.8109*(diameter.cm/(diameter.cm+11))
  )

  #B5
  try(
  dry_weight_of_stem_wood <-
    -3.3045+
    +8.1184*(diameter.cm/(diameter.cm+11))+
    +0.9783*log(height.m)
  )

  #B6
  try(
  dry_weight_of_stem_wood <-
    -3.0464+
    +8.3820*(diameter.cm/(diameter.cm+11))+
    +0.9113*log(height.m)+
    +0.1024*log(age_at_breast_height)+
    -0.1067*log(double_bark.mm)+
    -0.00552*latitude
  )

  return(exp(dry_weight_of_stem_wood))

}
