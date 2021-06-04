#' Dry weight of stem bark for Spruce, Marklund (1988)
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, m.
#' @param age_at_breast_height Age of tree at breast height.
#' @param Dmax_10_m_radius Diameter above bark of the thickest tree on a 10 m radius plot.
#' @param dominant_species "Picea abies" or "Pinus sylvestris". Species for which SI was estimated.
#' @param SI Estimated SI.
#' @param double_bark.mm Double bark thickness.
#'
#' @return
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_stem_bark_Spruce <-  function(
  diameter.cm,
  height.m,
  age_at_breast_height,
  Dmax_10_m_radius,
  dominant_species,
  SI,
  double_bark.mm
){

  if(dominant_species=="Picea abies"){
    picea <- TRUE
    pinus <- FALSE
  } else if(dominant_species=="Pinus sylvestris"){
    picea <- FALSE
    pinus <- TRUE
  }

  #G7
  try(
  dry_weight_of_stem_bark <-
    -3.3912+
    +9.8364*(diameter.cm/(diameter.cm+15))
  )

  #G8
  try(
  dry_weight_of_stem_bark <-
    -3.4020+
    +8.3089*(diameter.cm/(diameter.cm+15))+
    +0.0147*height.m+
    +0.2295*log(height.m)
  )

  #G9
  try(
  dry_weight_of_stem_bark <-
    -2.9427+
    +7.2807*(diameter.cm/(diameter.cm+15))+
    +0.0341*height.m+
    +0.3363*log(height.m)+
    -0.0203*SI*pinus+
    -0.0208*SI*picea
  )

  #G10
  try(
  dry_weight_of_stem_bark <-
    -3.1923+
    +6.5893*(diameter.cm/(diameter.cm+15))+
    +0.0353*height.m+
    +0.2818*log(height.m)+
    +0.1662*log(double_bark.mm)+
    +0.1729*log(age_at_breast_height)+
    -0.1836*log(Dmax_10_m_radius)+
    -0.00725*SI*pinus+
    -0.00849*SI*picea
  )

  return(exp(dry_weight_of_stem_bark))

}
